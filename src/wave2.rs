use crate::rules::Rules;
use crate::DIRECTIONS;
use anyhow::Result;
use instant::Instant;
use rand::distributions::Uniform;
use rand::prelude::ThreadRng;
use rand::prelude::{Distribution, SliceRandom};
use std::cmp::{self, Ordering};
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;
use tracing::{error, info, warn};

#[derive(Debug, Clone)]
pub struct Wave2 {
    pub width: isize,
    pub height: isize,
    cells: Vec<HashMap<u16, [isize; 4]>>,
    rules: Rc<Rules>,
    // no_propagate_indices: Vec<bool>,
}

impl Wave2 {
    pub fn new(
        width: isize,
        height: isize,
        rules: &Rules,
        // template_map: Option<(Vec<u16>, u16)>,
    ) -> Wave2 {
        let mut cells = vec![HashMap::new(); (width * height) as usize];

        // initialize all cells in the map to be able to be every possible tile.
        for y in 0..height {
            for x in 0..width {
                for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
                    let target_x = x + direction.0;
                    let target_y = y + direction.1;

                    if target_x < 0 || target_x >= width || target_y < 0 || target_y >= height {
                        continue;
                    }

                    let target_index = (target_x + target_y * width) as usize;
                    let target_cell = &mut cells[target_index];

                    for rule in &rules.ruleset[ordinal] {
                        for allowed_tile in rule.1 {
                            target_cell.entry(*allowed_tile).or_insert([0, 0, 0, 0])[ordinal] += 1;
                        }
                    }
                }
            }
        }

        let mut wave = Wave2 {
            width: width as isize,
            height: height as isize,
            cells,
            rules: Rc::new(rules.clone()),
        };

        // iterate through the whole map and remove tiles that don't have sufficient support to exist
        for y in 0..height {
            for x in 0..width {
                let index = (x + y * width) as usize;

                for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
                    let source_x = x - direction.0;
                    let source_y = y - direction.1;

                    if source_x < 0 || source_x >= width || source_y < 0 || source_y >= height {
                        continue;
                    }

                    let target_cell = &mut wave.cells[index];

                    let mut to_remove = HashSet::new();
                    for tile in &target_cell.keys().cloned().collect::<Vec<_>>() {
                        if target_cell[tile][ordinal] <= 0 {
                            to_remove.insert(*tile);
                        }
                    }
                    // TODO maybe make it 'remove and propagate'?

                    if wave.propagate(index, to_remove) == false {
                        panic!();
                    }
                }
            }
        }

        info!("finish constructor");
        wave
    }

    pub fn print_wave(&self) {
        info!("wave:");
        for y in 0..self.height {
            let mut output = String::new();
            for x in 0..self.width {
                output = format!(
                    "{}{:6}",
                    output,
                    self.cells[(x + y * self.width) as usize].len()
                );
            }
            info!("{}", output);
        }
    }

    pub fn render(&self) -> Vec<u16> {
        let mut ret = Vec::new();
        for v in &self.cells {
            if v.len() > 1 || v.len() == 0 {
                ret.push(15);
            } else {
                ret.push(*v.iter().next().unwrap().0);
            }
        }
        ret
    }

    pub fn is_done(&self) -> bool {
        for v in &self.cells {
            if v.len() != 1 {
                return false;
            }
        }

        true
    }

    pub fn propagate(&mut self, start: usize, to_remove: HashSet<u16>) -> bool {
        #[derive(Clone, Eq, PartialEq, Debug)]
        struct Node {
            target_index: usize,
            projected_possibilities: isize,
            to_remove: HashSet<u16>,
        }

        impl Ord for Node {
            fn cmp(&self, other: &Self) -> Ordering {
                // Notice that the we flip the ordering on costs.
                // In case of a tie we compare positions - this step is necessary
                // to make implementations of `PartialEq` and `Ord` consistent.
                other
                    .projected_possibilities
                    .cmp(&self.projected_possibilities)
            }
        }

        // `PartialOrd` needs to be implemented as well.
        impl PartialOrd for Node {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        let mut vec = Vec::<Node>::new();

        vec.push(Node {
            target_index: start,
            projected_possibilities: self.cells[start].len() as isize,
            to_remove,
        });

        while vec.len() > 0 {
            // info!("propagate step");
            let chosen = vec.pop().unwrap();
            let current_index = chosen.target_index;
            let current_cell = &mut self.cells[current_index];

            let mut removed = HashSet::new();
            for tile in chosen.to_remove {
                if current_cell.remove(&tile).is_some() {
                    removed.insert(tile);
                }
            }

            if current_cell.len() == 0 {
                // unsatisfiable.
                return false;
            }

            for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
                let (source_x, source_y) = (
                    current_index as isize % self.width,
                    current_index as isize / self.width,
                );
                assert!(source_x >= 0 && source_x < self.width);
                assert!(source_y >= 0 && source_y < self.height);

                let target_x = source_x + direction.0;
                let target_y = source_y + direction.1;

                if target_x < 0 || target_x >= self.width || target_y < 0 || target_y >= self.height
                {
                    continue;
                }

                let target_index = (target_x + target_y * self.width) as usize;
                let target_cell = &mut self.cells[target_index];

                let mut tiles_to_remove = HashSet::new();
                for removed_tile in &removed {
                    if let Some(rule) = self.rules.ruleset[ordinal].get(removed_tile) {
                        for allowed_tile in rule {
                            if let Some(target_tile_possibilities) =
                                target_cell.get_mut(allowed_tile)
                            {
                                target_tile_possibilities[ordinal] -= 1;

                                if target_tile_possibilities[ordinal] <= 0 {
                                    tiles_to_remove.insert(*allowed_tile);
                                }
                            }
                        }
                    } else {
                        //panic!("ordinal: {ordinal}, removed_tile: {removed_tile}");
                    }
                }

                if tiles_to_remove.len() > 0 {
                    let mut was_found = false;
                    for i in &mut vec {
                        if i.target_index == target_index {
                            i.to_remove.extend(tiles_to_remove.clone());
                            i.projected_possibilities =
                                (target_cell.len() - i.to_remove.len()) as isize;
                            was_found = true;
                            break;
                        }
                    }

                    if !was_found {
                        vec.push(Node {
                            target_index,
                            projected_possibilities: (target_cell.len() - tiles_to_remove.len())
                                as isize,
                            to_remove: tiles_to_remove,
                        });
                    }

                    vec.sort_by(|a, b| b.projected_possibilities.cmp(&a.projected_possibilities));
                }
            }
        }

        // info!("post-propagate");
        // self.print_wave();

        true
    }

    pub fn get_entropy_indices_in_order(
        &self,
        mut rng: &mut ThreadRng,
        at_least: usize,
    ) -> Result<Vec<(usize, usize)>> {
        // TODO: use real entropy instead of number of possibilities. Requires tile weights.
        let mut entropies = self
            .cells
            .iter()
            .map(|x| x.len())
            .enumerate()
            .filter(|&x| x.1 > 1)
            .collect::<Vec<(usize, usize)>>();

        anyhow::ensure!(entropies.len() >= 1);

        entropies.shuffle(&mut rng);
        entropies.sort_unstable_by_key(|x| x.1);

        entropies.truncate(cmp::max(2, at_least));

        anyhow::Ok(entropies)
    }

    pub fn collapse_index(&mut self, mut rng: &mut ThreadRng, index: usize) -> HashSet<u16> {
        if self.cells[index].len() <= 1 {
            panic!("can't collapse this one.");
        }

        let mut all_tiles: Vec<_> = self.cells[index].iter().collect();
        let chosen_index = Uniform::from(0..all_tiles.len()).sample(&mut rng);
        all_tiles.remove(chosen_index);

        all_tiles.into_iter().map(|x| x.0).cloned().collect()
    }

    pub fn logical_conclusion<F: Fn(&Wave2)>(
        &self,
        update: &F,
        update_interval: u32,
    ) -> Result<Wave2> {
        let mut rng = rand::thread_rng();

        let mut waves = VecDeque::new();
        let mut indices = VecDeque::new();

        let mut current_wave = self.clone();
        let mut current_indices;

        let mut last_time = Instant::now();
        update(&current_wave);

        warn!("Initial Wave");
        current_wave.print_wave();

        current_indices = current_wave.get_entropy_indices_in_order(&mut rng, 100000)?;

        loop {
            while waves.len() > 20 {
                // We're not going to go back 100 iterations, so, start dropping those ones so we don't run out of memory.
                waves.pop_back();
                indices.pop_back();
            }

            let current_index = current_indices.pop();

            if current_index == None {
                current_indices = indices.pop_front().unwrap();
                current_wave = waves.pop_front().unwrap();
                warn!("backtrack");
                continue;
            }

            let (index, _pop_cnt) = current_index.unwrap();

            if Instant::now().duration_since(last_time).as_millis() as u32 > update_interval {
                last_time = Instant::now();
                update(&current_wave);
            }

            let mut wave2 = current_wave.clone();

            let removed = wave2.collapse_index(&mut rng, index);

            if !wave2.propagate(index, removed) {
                continue;
            }

            if wave2.is_done() {
                return anyhow::Ok(wave2);
            }

            // this would normally be a recursive call but I can't figure out how to increase the stack size in wasm.
            waves.push_front(current_wave);
            indices.push_front(current_indices);
            current_wave = wave2;
            current_indices = current_wave.get_entropy_indices_in_order(&mut rng, 2)?;
        }
    }
}
