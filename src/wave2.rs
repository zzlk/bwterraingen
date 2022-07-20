use crate::rules::Rules;
use crate::{DIRECTIONS, MAX_TILE_IDS};
use anyhow::Result;
use cached::proc_macro::cached;
use cached::CachedAsync;
use instant::Instant;
use rand::distributions::Uniform;
use rand::prelude::ThreadRng;
use rand::prelude::{Distribution, SliceRandom};
use std::cmp::{self, Ordering};
use std::collections::{HashMap, HashSet, LinkedList, VecDeque};
use std::rc::Rc;
use tracing::{debug, error, info, warn};

#[derive(Debug)]
struct FlatRules {
    era: u16,
    ruleset: [[Option<HashSet<u16>>; MAX_TILE_IDS]; 4],
}

#[derive(Debug, Clone)]
pub struct Wave2 {
    pub width: isize,
    pub height: isize,
    cells: Vec<HashMap<u16, [i16; 4]>>,
    rules: Rc<FlatRules>,
    example_cell: Rc<HashMap<u16, [i16; 4]>>,
    inverse_mapping: Rc<HashMap<u16, u16>>,
    cache_src: [HashSet<u16>; 4],
    cache: [HashMap<u16, i16>; 4],
    operations: usize,
    // no_propagate_indices: Vec<bool>,
}

impl Wave2 {
    pub fn new(
        width: isize,
        height: isize,
        rules: &Rules,
        template_map_and_mask_tile: Option<(Vec<u16>, u16)>,
    ) -> Wave2 {
        let mut mapping = HashMap::new();
        let mut inverse_mapping = HashMap::new();

        let mut new_rules = [
            [(); MAX_TILE_IDS].map(|_| None),
            [(); MAX_TILE_IDS].map(|_| None),
            [(); MAX_TILE_IDS].map(|_| None),
            [(); MAX_TILE_IDS].map(|_| None),
        ];

        // remap tiles so that they are sequential
        {
            // build up mapping tables, this should contain all keys.
            for (ordinal, _) in DIRECTIONS.iter().enumerate() {
                for &tile in rules.ruleset[ordinal].keys() {
                    if !mapping.contains_key(&tile) {
                        inverse_mapping.insert(mapping.len() as u16, tile);
                        mapping.insert(tile, mapping.len() as u16);
                    }
                }
            }

            // convert rules to dense format
            for (ordinal, _) in DIRECTIONS.iter().enumerate() {
                let old_rule = &rules.ruleset[ordinal];
                let new_rule = &mut new_rules[ordinal];

                let mut rule = [(); MAX_TILE_IDS].map(|_| None);
                for (tile, allowed_tiles) in old_rule {
                    let mut allowed_tiles_remapped = HashSet::new();
                    for allowed_tile in allowed_tiles {
                        allowed_tiles_remapped.insert(mapping[allowed_tile]);
                    }
                    rule[mapping[tile] as usize] = Some(allowed_tiles_remapped);
                }

                *new_rule = rule;
            }
        }

        info!("total unique tiles: {}", new_rules[0].len());

        let rules = FlatRules {
            ruleset: new_rules,
            era: rules.era,
        };

        // initialize all cells in the map to be able to be every possible tile.
        let mut example_cell = HashMap::new();

        // Calculate the kind of 'maximum' cell
        for (ordinal, _) in DIRECTIONS.iter().enumerate() {
            for (_, allowed_tiles) in rules.ruleset[ordinal]
                .iter()
                .filter(|x| x.is_some())
                .map(|x| x.as_ref().unwrap())
                .enumerate()
            {
                for allowed_tile in allowed_tiles {
                    example_cell.entry(*allowed_tile).or_insert([0, 0, 0, 0])[ordinal] += 1;
                }
            }
        }

        let mut wave = Wave2 {
            width: width,
            height: height,
            cells: vec![example_cell.clone(); (width * height) as usize], //Vec::with_capacity((width * height) as usize),
            rules: Rc::new(rules),
            example_cell: Rc::new(example_cell),
            cache_src: [
                HashSet::new(),
                HashSet::new(),
                HashSet::new(),
                HashSet::new(),
            ],
            cache: [
                HashMap::new(),
                HashMap::new(),
                HashMap::new(),
                HashMap::new(),
            ],
            inverse_mapping: Rc::new(inverse_mapping),
            operations: 0,
        };

        // set tiles to tiles from template map and calculate support
        // if let Some((template_map, mask_tile)) = template_map_and_mask_tile {
        //     for y in 0..height {
        //         for x in 0..width {
        //             let index = (x + y * width) as usize;

        //             if template_map[index] != mask_tile {
        //                 wave.cells[index] = HashMap::new();
        //                 let mut template_tile = HashSet::new();
        //                 template_tile.insert(template_map[index]);
        //                 wave.cells.push((*wave.example_cell).clone());
        //             } else {
        //                 wave.cells.push((*wave.example_cell).clone());
        //             }
        //         }
        //     }
        // } else {
        //     for _ in 0..height {
        //         for _ in 0..width {
        //             wave.cells.push((*wave.example_cell).clone());
        //         }
        //     }
        // }

        // calculate support for all tiles
        // for y in 0..wave.height {
        //     for x in 0..wave.width {
        //         let index = (x + y * wave.width) as usize;

        //         for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
        //             let source_x = index as isize % wave.width;
        //             let source_y = index as isize / wave.width;

        //             let target_x = source_x + direction.0;
        //             let target_y = source_y + direction.1;

        //             if target_x < 0
        //                 || target_x >= wave.width
        //                 || target_y < 0
        //                 || target_y >= wave.height
        //             {
        //                 continue;
        //             }

        //             let target_index = (target_x + target_y * wave.width) as usize;

        //             for (_src_tile, allowed_tiles) in &wave.rules.ruleset[ordinal] {
        //                 for allowed_tile in allowed_tiles {
        //                     if let Some(support) = wave.cells[target_index].get_mut(allowed_tile) {
        //                         support[ordinal] += 1;
        //                     }
        //                 }
        //             }
        //         }
        //     }
        // }

        // remove tiles with insufficient support
        loop {
            // info!("cells: {:?}", wave.cells);
            let mut was_empty = true;
            for target_y in 0..wave.height {
                for target_x in 0..wave.width {
                    let index = (target_x + target_y * wave.width) as usize;

                    let mut to_remove = HashSet::new();
                    for (tile, support) in &wave.cells[index] {
                        for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
                            let source_x = target_x - direction.0;
                            let source_y = target_y - direction.1;

                            if source_x < 0
                                || source_x >= wave.width
                                || source_y < 0
                                || source_y >= wave.height
                            {
                                continue;
                            }

                            if support[ordinal] <= 0 {
                                to_remove.insert(*tile);
                            }
                        }
                    }
                    // info!("to_remove: {to_remove:?}");

                    if !to_remove.is_empty() {
                        was_empty = false;
                        for tile in to_remove {
                            wave.cells[index].remove(&tile);

                            for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
                                let target_x = target_x + direction.0;
                                let target_y = target_y + direction.1;

                                if target_x < 0
                                    || target_x >= wave.width
                                    || target_y < 0
                                    || target_y >= wave.height
                                {
                                    continue;
                                }

                                let target_index = (target_x + target_y * wave.width) as usize;

                                if let Some(ruleset) = &wave.rules.ruleset[ordinal][tile as usize] {
                                    for allowed_tile in ruleset {
                                        if let Some(support) =
                                            wave.cells[target_index].get_mut(allowed_tile)
                                        {
                                            support[ordinal] -= 1;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if was_empty == true {
                break;
            }
        }

        // std::thread::sleep(std::time::Duration::from_secs(10));

        // TODO: propagate_remove template tiles to leave only the template tiles for where the mask is the same.

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
                ret.push(self.inverse_mapping[v.iter().next().unwrap().0]);
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

    // fn propagate_add(&mut self, start: usize, to_add: &HashSet<u16>) {
    //     #[derive(Clone, Eq, PartialEq, Debug)]
    //     struct Node {
    //         target_index: usize,
    //         projected_possibilities: isize,
    //         tiles_added: HashSet<u16>,
    //     }

    //     impl Ord for Node {
    //         fn cmp(&self, other: &Self) -> Ordering {
    //             // Notice that the we flip the ordering on costs.
    //             // In case of a tie we compare positions - this step is necessary
    //             // to make implementations of `PartialEq` and `Ord` consistent.
    //             other
    //                 .projected_possibilities
    //                 .cmp(&self.projected_possibilities)
    //         }
    //     }

    //     // `PartialOrd` needs to be implemented as well.
    //     impl PartialOrd for Node {
    //         fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    //             Some(self.cmp(other))
    //         }
    //     }

    //     let rules = self.rules.clone();

    //     let mut global_tiles_added = HashSet::new();

    //     let mut pq = Vec::<Node>::new();

    //     let mut added = HashSet::new();
    //     for tile in to_add {
    //         if !self.cells[start].contains_key(&tile) {
    //             self.cells[start].insert(*tile, [0, 0, 0, 0]);
    //             global_tiles_added.insert((start, *tile));
    //             added.insert(*tile);
    //         }
    //     }

    //     pq.push(Node {
    //         target_index: start,
    //         projected_possibilities: self.cells[start].len() as isize,
    //         tiles_added: added,
    //     });

    //     while pq.len() > 0 {
    //         let chosen = pq.pop().unwrap();
    //         let current_index = chosen.target_index;

    //         for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
    //             let (source_x, source_y) = (
    //                 current_index as isize % self.width,
    //                 current_index as isize / self.width,
    //             );
    //             assert!(source_x >= 0 && source_x < self.width);
    //             assert!(source_y >= 0 && source_y < self.height);

    //             let target_x = source_x + direction.0;
    //             let target_y = source_y + direction.1;

    //             if target_x < 0 || target_x >= self.width || target_y < 0 || target_y >= self.height
    //             {
    //                 continue;
    //             }

    //             let target_index = (target_x + target_y * self.width) as usize;

    //             let mut tiles_added = HashSet::new();
    //             for added_tile in &chosen.tiles_added {
    //                 if let Some(rule) = rules.ruleset[ordinal].get(added_tile) {
    //                     for allowed_tile in rule {
    //                         if !self.cells[target_index].contains_key(allowed_tile) {
    //                             self.cells[target_index].insert(*allowed_tile, [0, 0, 0, 0]);
    //                             global_tiles_added.insert((target_index, *allowed_tile));
    //                             tiles_added.insert(*allowed_tile);
    //                         } else {
    //                             self.cells[target_index].get_mut(allowed_tile).unwrap()[ordinal] +=
    //                                 1;
    //                         }
    //                     }
    //                 }
    //             }

    //             if tiles_added.len() > 0 {
    //                 let mut was_found = false;
    //                 for i in &mut pq {
    //                     if i.target_index == target_index {
    //                         i.tiles_added.extend(tiles_added.clone());
    //                         i.projected_possibilities =
    //                             (self.cells[target_index].len() + i.tiles_added.len()) as isize;
    //                         was_found = true;
    //                         break;
    //                     }
    //                 }

    //                 if !was_found {
    //                     pq.push(Node {
    //                         target_index,
    //                         projected_possibilities: (self.cells[target_index].len()
    //                             + tiles_added.len())
    //                             as isize,
    //                         tiles_added,
    //                     });
    //                 }

    //                 pq.sort_by(|a, b| b.projected_possibilities.cmp(&a.projected_possibilities));
    //             }
    //         }
    //     }

    //     // calculate support for all inserted tiles
    //     for (index, tile) in global_tiles_added {
    //         self.calculate_support_for_tile(index, tile);
    //     }
    // }

    // fn calculate_support_for_tile(&mut self, cell_index: usize, tile: u16) {
    //     // assert!(*self.cells[cell_index].get(&tile).unwrap() == [0, 0, 0, 0]);

    //     *self.cells[cell_index].get_mut(&tile).unwrap() = [0, 0, 0, 0];

    //     for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
    //         let (start_x, start_y) = (
    //             cell_index as isize % self.width,
    //             cell_index as isize / self.width,
    //         );
    //         assert!(start_x >= 0 && start_x < self.width);
    //         assert!(start_y >= 0 && start_y < self.height);

    //         let source_x = start_x - direction.0;
    //         let source_y = start_y - direction.1;

    //         if source_x < 0 || source_x >= self.width || source_y < 0 || source_y >= self.height {
    //             continue;
    //         }

    //         let source_index = (source_x + source_y * self.width) as usize;
    //         let source_cell = &mut self.cells[source_index];

    //         let mut to_do = Vec::new();
    //         for (source_tile, _) in &*source_cell {
    //             if let Some(rules) = self.rules.ruleset[ordinal].get(source_tile) {
    //                 if let Some(allowed_tile) = rules.get(&tile) {
    //                     to_do.push((allowed_tile, ordinal));
    //                 }
    //             }
    //         }

    //         for item in to_do {
    //             if let Some(support) = self.cells[cell_index].get_mut(&item.0) {
    //                 support[item.1] += 1;
    //             }
    //         }
    //     }
    // }

    // pub fn unpropagate(&mut self, start: usize, tiles_removed: &HashSet<u16>) {
    //     // debug!("unpropagating");
    //     // self.print_wave();
    //     #[derive(Clone, Eq, PartialEq, Debug)]
    //     struct Node {
    //         target_index: usize,
    //         projected_possibilities: isize,
    //         tiles_inserted: HashSet<u16>,
    //     }

    //     impl Ord for Node {
    //         fn cmp(&self, other: &Self) -> Ordering {
    //             // Notice that the we flip the ordering on costs.
    //             // In case of a tie we compare positions - this step is necessary
    //             // to make implementations of `PartialEq` and `Ord` consistent.
    //             other
    //                 .projected_possibilities
    //                 .cmp(&self.projected_possibilities)
    //         }
    //     }

    //     // `PartialOrd` needs to be implemented as well.
    //     impl PartialOrd for Node {
    //         fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    //             Some(self.cmp(other))
    //         }
    //     }

    //     let mut tiles_added = HashSet::new();

    //     for tile in tiles_removed {
    //         // debug!("start: {start}, tile: {tile}");
    //         if self.cells[start].insert(*tile, [0, 0, 0, 0]).is_some() {
    //             panic!();
    //         } else {
    //             tiles_added.insert((start, *tile));
    //             // self.calculate_support_for_tile(start, *tile);
    //         }
    //     }

    //     let mut vec = Vec::<Node>::new();
    //     vec.push(Node {
    //         target_index: start,
    //         projected_possibilities: self.cells[start].len() as isize,
    //         tiles_inserted: tiles_removed.clone(),
    //     });

    //     while vec.len() > 0 {
    //         // self.print_wave();
    //         // info!("propagate step");
    //         let chosen = vec.pop().unwrap();
    //         let current_index = chosen.target_index;

    //         for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
    //             let (source_x, source_y) = (
    //                 current_index as isize % self.width,
    //                 current_index as isize / self.width,
    //             );
    //             assert!(source_x >= 0 && source_x < self.width);
    //             assert!(source_y >= 0 && source_y < self.height);

    //             let target_x = source_x + direction.0;
    //             let target_y = source_y + direction.1;

    //             if target_x < 0 || target_x >= self.width || target_y < 0 || target_y >= self.height
    //             {
    //                 continue;
    //             }

    //             let target_index = (target_x + target_y * self.width) as usize;

    //             // debug!("unpropagating. potentially incrementing counts. ordinal: {ordinal}, source: {source_x},{source_y}, target: {target_x},{target_y}");
    //             // debug!("cells before: {:?}", self.cells);

    //             // update support for surrounding cells based on newly re-inserted tiles
    //             let mut tiles_inserted = HashSet::new();
    //             for inserted_tile in &chosen.tiles_inserted {
    //                 if let Some(rule) = self.rules.ruleset[ordinal].get(inserted_tile) {
    //                     for allowed_tile in rule.clone() {
    //                         if !self.cells[target_index].contains_key(&allowed_tile) {
    //                             // debug!("inserting new tile");
    //                             tiles_inserted.insert(allowed_tile);

    //                             self.cells[target_index].insert(allowed_tile, [0, 0, 0, 0]);
    //                             tiles_added.insert((target_index, allowed_tile));

    //                             // add in support from existing tiles
    //                             // self.calculate_support_for_tile(target_index, allowed_tile);
    //                         } else {
    //                             // debug!("inserting new tile2");

    //                             self.cells[target_index].insert(allowed_tile, [0, 0, 0, 0]);
    //                             tiles_added.insert((target_index, allowed_tile));

    //                             // add in support from existing tiles
    //                             // self.calculate_support_for_tile(target_index, allowed_tile);
    //                             // let target_tile_possibilities =
    //                             //     self.cells[target_index].get_mut(&allowed_tile).unwrap();

    //                             // debug!(
    //                             //     "    unpropagating. incrementing. allowed_tile: {allowed_tile}"
    //                             // );
    //                             // target_tile_possibilities[ordinal] += 1;
    //                         }
    //                     }
    //                 }
    //             }

    //             // debug!("cells after: {:?}", self.cells);
    //             // info!("meme");

    //             if tiles_inserted.len() > 0 {
    //                 let mut was_found = false;
    //                 for i in &mut vec {
    //                     if i.target_index == target_index {
    //                         i.tiles_inserted.extend(tiles_inserted.clone());
    //                         i.projected_possibilities =
    //                             (self.cells[target_index].len() + i.tiles_inserted.len()) as isize;
    //                         was_found = true;
    //                         break;
    //                     }
    //                 }

    //                 if !was_found {
    //                     vec.push(Node {
    //                         target_index,
    //                         projected_possibilities: (self.cells[target_index].len()
    //                             + tiles_inserted.len())
    //                             as isize,
    //                         tiles_inserted,
    //                     });
    //                 }

    //                 vec.sort_by(|a, b| b.projected_possibilities.cmp(&a.projected_possibilities));
    //             }
    //         }
    //     }

    //     // remove added tiles without sufficient support
    //     loop {
    //         // info!("memes");

    //         // calculate support for all added tiles
    //         for (index, tile) in &tiles_added {
    //             // info!("memes2: {}", tiles_added.len());
    //             if self.cells[*index].contains_key(tile) {
    //                 self.calculate_support_for_tile(*index, *tile);
    //             }
    //         }

    //         let mut was_removed = false;
    //         for (index, tile) in &tiles_added {
    //             // info!("memes3");
    //             for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
    //                 let target_x = *index as isize % self.width;
    //                 let target_y = *index as isize / self.width;
    //                 assert!(target_x >= 0 && target_x < self.width);
    //                 assert!(target_y >= 0 && target_y < self.height);

    //                 let source_x = target_x - direction.0;
    //                 let source_y = target_y - direction.1;

    //                 if source_x < 0
    //                     || source_x >= self.width
    //                     || source_y < 0
    //                     || source_y >= self.height
    //                 {
    //                     continue;
    //                 }

    //                 // let source_index = (source_x + source_y * self.width) as usize;

    //                 let mut has_zero_support = true;

    //                 if let Some(cell) = self.cells[*index].get(tile) {
    //                     has_zero_support = has_zero_support && cell[ordinal] == 0;
    //                 } else {
    //                     has_zero_support = true;
    //                 }

    //                 if has_zero_support {
    //                     if self.cells[*index].remove(tile).is_some() {
    //                         was_removed = true;
    //                     }
    //                 }
    //             }
    //         }

    //         if was_removed {
    //             continue;
    //         }

    //         break;
    //     }
    // }

    pub fn unpropagate_v2(&mut self, propagation_backups: &HashMap<(usize, u16), [i16; 4]>) {
        for ((index, tile), pb) in propagation_backups {
            self.cells[*index].insert(*tile, *pb);
        }
    }

    pub fn propagate_remove(
        &mut self,
        start: usize,
        to_remove: &HashSet<u16>,
    ) -> (Option<usize>, HashMap<(usize, u16), [i16; 4]>) {
        // debug!("propagate_remove");
        #[derive(Clone, Eq, PartialEq, Debug)]
        struct Node {
            target_index: usize,
            projected_possibilities: isize,
            removed: HashSet<u16>,
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

        for tile in to_remove {
            assert!(self.cells[start].contains_key(tile));
        }

        let rules = self.rules.clone();

        let mut pq = Vec::<Node>::new();

        let mut backup = HashMap::new();

        let mut removed = HashSet::new();
        for tile in to_remove {
            if let Some(support) = self.cells[start].remove(&tile) {
                if !backup.contains_key(&(start, *tile)) {
                    backup.insert((start, *tile), support);
                }

                removed.insert(*tile);
            }
        }

        pq.push(Node {
            target_index: start,
            projected_possibilities: self.cells[start].len() as isize,
            removed: removed.clone(),
        });

        while pq.len() > 0 {
            // info!("pq: {:?}", pq);
            let chosen = pq.pop().unwrap();
            let current_index = chosen.target_index;

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

                // {
                //     // if ordinal == 0 {
                //     //     info!("chosen.removed: {:?}", chosen.removed);
                //     // }

                //     for tile_to_add in chosen
                //         .removed
                //         .difference(&self.cache_src[ordinal])
                //         .cloned()
                //         .collect::<Vec<_>>()
                //     {
                //         // info!("adding: {tile_to_add}");
                //         self.cache_src[ordinal].insert(tile_to_add);

                //         if let Some(rule) = rules.ruleset[ordinal].get(&tile_to_add) {
                //             for allowed_tile in rule {
                //                 self.operations += 1;
                //                 *self.cache[ordinal].entry(*allowed_tile).or_insert(0) += 1;
                //             }
                //         }
                //     }

                //     // remove superfluous items from cache
                //     for tile_to_remove in self.cache_src[ordinal]
                //         .difference(&chosen.removed)
                //         .cloned()
                //         .collect::<Vec<_>>()
                //     {
                //         //info!("removing: {tile_to_remove}");
                //         self.cache_src[ordinal].remove(&tile_to_remove);

                //         if let Some(rule) = rules.ruleset[ordinal].get(&tile_to_remove) {
                //             for allowed_tile in rule {
                //                 let m = self.cache[ordinal].get_mut(allowed_tile).unwrap();

                //                 self.operations += 1;
                //                 *m -= 1;

                //                 if *m == 0 {
                //                     self.cache[ordinal]
                //                         .remove(allowed_tile)
                //                         .expect("needs to actually remove");
                //                 }
                //             }
                //         }
                //     }

                //     assert_eq!(chosen.removed, self.cache_src[ordinal]);
                // }

                // panic!();
                let mut target_tiles = HashMap::new();
                for removed_tile in &chosen.removed {
                    if let Some(rule) = &rules.ruleset[ordinal][*removed_tile as usize] {
                        for allowed_tile in rule {
                            self.operations += 1;
                            *target_tiles.entry(allowed_tile).or_insert(0) += 1;
                        }
                    }
                }

                // debug!("target_tiles: {:?}", target_tiles);

                let mut newly_removed_tiles = HashSet::new();
                for (target_tile, delta) in target_tiles {
                    if let Some(support) = target_cell.get_mut(&target_tile) {
                        if !backup.contains_key(&(target_index, *target_tile)) {
                            backup.insert((target_index, *target_tile), *support);
                        }

                        support[ordinal] -= delta;

                        if support[ordinal] <= 0 {
                            target_cell.remove(&target_tile);
                            newly_removed_tiles.insert(*target_tile);

                            if target_cell.len() == 0 {
                                // unsatisfiable.
                                return (Some(target_index), backup);
                            }
                        }
                    }
                }

                if newly_removed_tiles.len() > 0 {
                    let mut was_found = false;
                    for i in &mut pq {
                        if i.target_index == target_index {
                            i.removed.extend(newly_removed_tiles.clone());
                            i.projected_possibilities = i.removed.len() as isize;
                            was_found = true;
                            break;
                        }
                    }

                    if !was_found {
                        pq.push(Node {
                            target_index,
                            projected_possibilities: newly_removed_tiles.len() as isize,
                            removed: newly_removed_tiles,
                        });
                    }

                    pq.sort_by(|a, b| a.projected_possibilities.cmp(&b.projected_possibilities));
                }
            }
        }

        // info!("post-propagate");
        // self.print_wave();

        (None, backup)
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

    // pub fn get_entropy_indices_in_order(
    //     &self,
    //     mut rng: &mut ThreadRng,
    //     at_least: usize,
    // ) -> Result<Vec<(usize, usize)>> {
    //     // TODO: use real entropy instead of number of possibilities. Requires tile weights.
    //     let mut entropies = self
    //         .cells
    //         .iter()
    //         .map(|x| x.len())
    //         .enumerate()
    //         .filter(|&x| x.1 > 1)
    //         .map(|z| {
    //             let x = z.0 % self.width as usize;
    //             let y = z.0 / self.width as usize;

    //             let r = (f64::sqrt((x * x + y * y) as f64) * 1000.0) as usize;

    //             (z.0, r)
    //         })
    //         .collect::<Vec<(usize, usize)>>();

    //     anyhow::ensure!(entropies.len() >= 1);

    //     entropies.shuffle(&mut rng);
    //     entropies.sort_unstable_by_key(|x| x.1);

    //     entropies.truncate(cmp::max(2, at_least));

    //     anyhow::Ok(entropies)
    // }

    pub fn choose_removal_set(&self, mut rng: &mut ThreadRng, index: usize) -> HashSet<u16> {
        if self.cells[index].len() <= 1 {
            panic!("can't collapse this one.");
        }

        let mut all_tiles: Vec<_> = self.cells[index].iter().collect();
        let chosen_index = Uniform::from(0..all_tiles.len()).sample(&mut rng);
        all_tiles.remove(chosen_index);

        all_tiles.into_iter().map(|x| x.0).cloned().collect()
    }

    pub fn normalize_support(&mut self, x_min: isize, y_min: isize, x_max: isize, y_max: isize) {
        // // iterate through the whole map and remove tiles that don't have sufficient support to exist
        for y in y_min..=y_max {
            for x in x_min..=x_max {
                let index = (x + y * self.width) as usize;

                let mut to_remove = HashSet::new();
                for (tile, support) in &self.cells[index] {
                    for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
                        let source_x = x - direction.0;
                        let source_y = y - direction.1;

                        if source_x < 0
                            || source_x >= self.width
                            || source_y < 0
                            || source_y >= self.height
                        {
                            continue;
                        }

                        if support[ordinal] <= 0 {
                            to_remove.insert(*tile);
                        }
                    }
                }

                if self.propagate_remove(index, &to_remove).0.is_some() {
                    panic!();
                }

                self.print_wave();
            }
        }
    }

    pub fn logical_conclusion<F: Fn(&Wave2)>(
        &mut self,
        update: &F,
        update_interval: u32,
        nuke_radius: isize,
    ) -> Result<Wave2> {
        update(&self);

        // // shrink it to save memory
        for y in 0..self.height {
            for x in 0..self.width {
                let index = (x + y * self.width) as usize;
                let source_cell = &mut self.cells[index];

                source_cell.shrink_to_fit();
            }
        }

        self.cells.shrink_to_fit();

        let mut rng = rand::thread_rng();

        // let mut waves = LinkedList::new();
        let mut propagation_backups = VecDeque::new();
        let mut indices = VecDeque::new();

        let mut current_wave = self.clone();
        let mut current_indices;

        let mut last_time = Instant::now();

        let mut failures = HashMap::new();

        current_indices = current_wave.get_entropy_indices_in_order(&mut rng, 100000)?;

        // info!("start main loop");
        // return anyhow::Ok(self.clone());

        debug!("start main loop");

        loop {
            //debug!("main loop iteration");
            while propagation_backups.len() > 100 {
                // We're not going to go back 100 iterations, so, start dropping those ones so we don't run out of memory.
                // waves.pop_back();
                indices.pop_back();
                propagation_backups.pop_back();
            }

            let current_index = current_indices.pop();

            if current_index == None {
                current_wave.unpropagate_v2(&propagation_backups.pop_front().unwrap());
                current_indices = indices.pop_front().unwrap();
                // unpropagate?
                //current_wave = waves.pop_front().unwrap();
                // let propagation = waves.pop_front().unwrap();
                // warn!("backtrack");
                // self.unpropagate(waves.index, &propagation.to_remove);
                continue;
            }

            let (index, _pop_cnt) = current_index.unwrap();

            if Instant::now().duration_since(last_time).as_millis() as u32 > update_interval {
                last_time = Instant::now();
                update(&current_wave);
            }

            //let mut new_wave = current_wave.clone();
            let to_remove = current_wave.choose_removal_set(&mut rng, index);

            let (failed_at, propagation_backup) = current_wave.propagate_remove(index, &to_remove);

            // info!("self.operations: {}", current_wave.operations);
            // panic!();

            if let Some(failed_at) = failed_at {
                // self.unpropagate(index, &to_remove);
                current_wave.unpropagate_v2(&propagation_backup);

                let failed_x = failed_at as isize % self.width;
                let failed_y = failed_at as isize / self.width;

                let failures_entry = failures.entry((failed_x, failed_y)).or_insert(0);

                *failures_entry += 1;

                if *failures_entry > 64 {
                    error!("NUKE");
                    failures.clear();

                    // nuke area
                    for dy in -nuke_radius..=nuke_radius {
                        for dx in -nuke_radius..=nuke_radius {
                            let nuke_x = failed_x + dx;
                            let nuke_y = failed_y + dy;

                            if nuke_x < 0
                                || nuke_x >= self.width
                                || nuke_y < 0
                                || nuke_y >= self.height
                            {
                                continue;
                            }

                            let nuke_index = (nuke_x + nuke_y * self.width) as usize;

                            current_wave.cells[nuke_index] = (*self.example_cell).clone();
                        }
                    }

                    self.normalize_support(
                        std::cmp::max(failed_x - (nuke_radius + 1), 0),
                        std::cmp::max(failed_y - (nuke_radius + 1), 0),
                        std::cmp::min(failed_x + (nuke_radius + 1), self.width - 1),
                        std::cmp::min(failed_y + (nuke_radius + 1), self.height - 1),
                    );

                    propagation_backups.clear();
                    indices.clear();

                    current_indices =
                        current_wave.get_entropy_indices_in_order(&mut rng, 100000)?;
                }

                continue;
            }

            if current_wave.is_done() {
                return anyhow::Ok(current_wave.clone());
            }

            propagation_backups.push_front(propagation_backup);
            // this would normally be a recursive call but I can't figure out how to increase the stack size in wasm.
            //waves.push_front(current_wave);
            // propagations.push_front(Propagation { index, to_remove });
            indices.push_front(current_indices);
            // current_wave = new_wave;
            current_indices = current_wave.get_entropy_indices_in_order(&mut rng, 2)?;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use quickcheck::TestResult;
    use quickcheck::{Arbitrary, Gen};
    use quickcheck_macros::quickcheck;
    use test_log::test;

    // #[test]
    // fn asfdasfasd() {
    //     #[rustfmt::skip]
    //     let rules = Rules::new(2, 3, &vec![
    //         1, 2,
    //         2, 1,
    //         1, 1], &HashSet::new(), 0);
    //     let wave = Wave2::new(3, 1, &rules);

    //     let mut to_remove = HashSet::new();
    //     to_remove.insert(1);

    //     let mut wave1 = wave.clone();
    //     wave1.propagate(0, &to_remove);
    //     assert_ne!(wave.cells, wave1.cells);
    //     wave1.unpropagate(0, &to_remove);
    //     assert_eq!(wave.cells, wave1.cells);
    // }

    // #[test]
    // fn dasfasdf() {
    //     #[rustfmt::skip]
    //     let rules = Rules::new(2, 3, &vec![
    //         1, 2,
    //         2, 1,
    //         1, 2], &HashSet::new(), 0);
    //     let wave = Wave2::new(2, 2, &rules);

    //     let mut to_remove = HashSet::new();
    //     to_remove.insert(1);

    //     let mut wave1 = wave.clone();
    //     wave1.propagate(0, &to_remove);
    //     assert_ne!(wave.cells, wave1.cells);
    //     wave1.unpropagate(0, &to_remove);
    //     assert_eq!(wave.cells, wave1.cells);
    // }

    // #[test]
    // fn dasfsdgagasdgasdf() {
    //     #[rustfmt::skip]
    //     let rules = Rules::new(2, 2, &vec![
    //         0, 0,
    //         1, 1], &HashSet::new(), 0);
    //     let wave = Wave2::new(2, 2, &rules);

    //     let mut to_remove = HashSet::new();
    //     to_remove.insert(1);

    //     let mut wave1 = wave.clone();
    //     wave1.propagate(2, &to_remove);
    //     assert_ne!(wave.cells, wave1.cells);
    //     wave1.unpropagate(2, &to_remove);
    //     assert_eq!(wave.cells, wave1.cells);
    // }

    // #[test]
    // fn dfgsdfgsdfs() {
    //     #[rustfmt::skip]
    //     let rules = Rules::new(2, 2, &vec![
    //         0, 1,
    //         1, 1], &HashSet::new(), 0);
    //     let wave = Wave2::new(2, 2, &rules);

    //     let mut to_remove = HashSet::new();
    //     to_remove.insert(1);

    //     let mut wave1 = wave.clone();
    //     wave1.propagate(3, &to_remove);
    //     assert_ne!(wave.cells, wave1.cells);
    //     wave1.unpropagate(3, &to_remove);
    //     assert_eq!(wave.cells, wave1.cells);
    // }

    // #[test]
    // fn sadafsag() {
    //     #[rustfmt::skip]
    //     let rules = Rules::new(3, 3, &vec![
    //         0, 0, 0,
    //         0, 2, 1,
    //         0, 0, 0], &HashSet::new(), 0);
    //     let wave = Wave2::new(3, 3, &rules);

    //     let mut to_remove = HashSet::new();
    //     to_remove.insert(0);

    //     let mut wave1 = wave.clone();
    //     wave1.propagate(7, &to_remove);
    //     assert_ne!(wave.cells, wave1.cells);
    //     wave1.unpropagate(7, &to_remove);
    //     assert_eq!(wave.cells, wave1.cells);
    // }

    // #[quickcheck]
    // fn fasdfasdf(index: ArbitraryIndex) {
    //     #[rustfmt::skip]
    //     let rules = Rules::new(2, 3, &vec![
    //         1, 2,
    //         2, 1,
    //         1, 2], &HashSet::new(), 0);
    //     let wave = Wave2::new(
    //         MAX_WAVE_DIMENSION as isize,
    //         MAX_WAVE_DIMENSION as isize,
    //         &rules,
    //     );

    //     let mut to_remove = HashSet::new();
    //     to_remove.insert(*wave.cells[index.index].iter().next().unwrap().0);

    //     let mut wave1 = wave.clone();
    //     wave1.propagate(index.index, &to_remove);
    //     assert_ne!(wave.cells, wave1.cells);
    //     wave1.unpropagate(index.index, &to_remove);
    //     assert_eq!(wave.cells, wave1.cells);
    // }

    // #[ignore]
    // #[quickcheck]
    // fn fasdfsadg(index: ArbitraryIndex, ruledata: Vec<u16>) -> TestResult {
    //     if ruledata.len() < MAX_WAVE_DIMENSION * MAX_WAVE_DIMENSION {
    //         return TestResult::discard();
    //     }

    //     let rules = Rules::new(
    //         MAX_WAVE_DIMENSION as isize,
    //         MAX_WAVE_DIMENSION as isize,
    //         &ruledata,
    //         &HashSet::new(),
    //         0,
    //     );
    //     let wave = Wave2::new(
    //         MAX_WAVE_DIMENSION as isize,
    //         MAX_WAVE_DIMENSION as isize,
    //         &rules,
    //     );

    //     let mut to_remove = HashSet::new();
    //     to_remove.insert(*wave.cells[index.index].iter().next().unwrap().0);

    //     let mut wave1 = wave.clone();
    //     wave1.propagate(index.index, &to_remove);
    //     assert_ne!(wave.cells, wave1.cells);
    //     wave1.unpropagate(index.index, &to_remove);
    //     assert_eq!(wave.cells, wave1.cells);

    //     TestResult::passed()
    // }

    // const MAX_WAVE_DIMENSION: usize = 2;

    // #[derive(Debug, Clone)]
    // struct ArbitraryIndex {
    //     index: usize,
    // }

    // impl Arbitrary for ArbitraryIndex {
    //     fn arbitrary(g: &mut Gen) -> ArbitraryIndex {
    //         ArbitraryIndex {
    //             index: usize::arbitrary(g) % (MAX_WAVE_DIMENSION * MAX_WAVE_DIMENSION),
    //         }
    //     }
    // }

    // #[derive(Debug, Clone)]
    // struct ArbitraryRuleData {
    //     ruledata: Vec<u16>,
    // }

    // impl Arbitrary for ArbitraryRuleData {
    //     fn arbitrary(g: &mut Gen) -> ArbitraryRuleData {
    //         let mut ruledata = vec![0u16; MAX_WAVE_DIMENSION * MAX_WAVE_DIMENSION];
    //         for v in ruledata.iter_mut() {
    //             *v = u16::arbitrary(g);
    //         }

    //         ArbitraryRuleData { ruledata: ruledata }
    //     }
    // }

    // #[quickcheck]
    // fn sdfsadfddd(index: ArbitraryIndex, ruledata: ArbitraryRuleData) -> TestResult {
    //     let rules = Rules::new(
    //         MAX_WAVE_DIMENSION as isize,
    //         MAX_WAVE_DIMENSION as isize,
    //         &ruledata.ruledata,
    //         &HashSet::new(),
    //         0,
    //     );
    //     let wave = Wave2::new(
    //         MAX_WAVE_DIMENSION as isize,
    //         MAX_WAVE_DIMENSION as isize,
    //         &rules,
    //         None,
    //     );

    //     let mut to_remove = HashSet::new();
    //     to_remove.insert(*wave.cells[index.index].iter().next().unwrap().0);

    //     let mut wave1 = wave.clone();
    //     let (_, pb) = wave1.propagate(index.index, &to_remove);
    //     assert_ne!(wave.cells, wave1.cells);
    //     wave1.unpropagate_v2(&pb);
    //     assert_eq!(wave.cells, wave1.cells);

    //     TestResult::passed()
    // }

    // #[derive(Debug, Clone)]
    // struct BoundedVec<const MIN_LEN: usize, const MAX_LEN: usize>(Vec<u16>);

    // impl<const MIN_LEN: usize, const MAX_LEN: usize> Arbitrary for BoundedVec<MIN_LEN, MAX_LEN> {
    //     fn arbitrary(g: &mut Gen) -> Self {
    //         let mut v = vec![0u16; usize::arbitrary(g) % (MAX_LEN - MIN_LEN) + MIN_LEN];

    //         for e in v.iter_mut() {
    //             *e = u16::arbitrary(g);
    //         }

    //         BoundedVec(v)
    //     }

    //     fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
    //         Box::new(BoundedVecShrinker {
    //             value: (*self).clone(),
    //         })
    //     }
    // }

    // struct BoundedVecShrinker<const MIN_LEN: usize, const MAX_LEN: usize> {
    //     value: BoundedVec<MIN_LEN, MAX_LEN>,
    // }

    // impl<const MIN_LEN: usize, const MAX_LEN: usize> Iterator for BoundedVecShrinker<MIN_LEN, MAX_LEN> {
    //     type Item = BoundedVec<MIN_LEN, MAX_LEN>;

    //     fn next(&mut self) -> Option<Self::Item> {
    //         if self.value.0.len() > MIN_LEN {
    //             if *self.value.0.last().unwrap() == 0 {
    //                 self.value.0.pop();
    //                 Some(self.value.clone())
    //             } else {
    //                 *self.value.0.last_mut().unwrap() /= 2;
    //                 Some(self.value.clone())
    //             }
    //         } else {
    //             if *self.value.0.last().unwrap() == 0 {
    //                 None
    //             } else {
    //                 *self.value.0.last_mut().unwrap() /= 2;
    //                 Some(self.value.clone())
    //             }
    //         }
    //     }
    // }

    #[derive(Debug, Clone, Copy)]
    struct BoundedInt<const LOWER: usize, const UPPER: usize>(usize);

    impl<const LOWER: usize, const UPPER: usize> Arbitrary for BoundedInt<LOWER, UPPER> {
        fn arbitrary(g: &mut Gen) -> Self {
            BoundedInt(usize::arbitrary(g) % (UPPER - LOWER) + LOWER)
        }

        fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
            Box::new(BoundedIntShrinker { value: *self })
        }
    }

    struct BoundedIntShrinker<const LOWER: usize, const UPPER: usize> {
        value: BoundedInt<LOWER, UPPER>,
    }

    impl<const LOWER: usize, const UPPER: usize> Iterator for BoundedIntShrinker<LOWER, UPPER> {
        type Item = BoundedInt<LOWER, UPPER>;

        fn next(&mut self) -> Option<Self::Item> {
            if self.value.0 > LOWER {
                self.value.0 -= 1;
                Some(self.value)
            } else {
                None
            }
        }
    }

    #[quickcheck]
    fn quickcheck_testcase(
        index: BoundedInt<0, { 4 * 4 }>,
        wave_width: BoundedInt<1, 4>,
        wave_height: BoundedInt<1, 4>,
        rule_width: BoundedInt<4, 8>,
        rule_height: BoundedInt<4, 8>,
        ruledata: Vec<u16>,
    ) -> TestResult {
        if ruledata.len() < rule_width.0 * rule_height.0 {
            return TestResult::discard();
        }

        if index.0 >= wave_width.0 * wave_height.0 {
            return TestResult::discard();
        }

        if rule_width.0 < wave_width.0 || rule_height.0 < wave_height.0 {
            return TestResult::discard();
        }

        let rules = Rules::new(
            rule_width.0 as isize,
            rule_height.0 as isize,
            &ruledata,
            &HashSet::new(),
            0,
        );
        let wave = Wave2::new(wave_width.0 as isize, wave_height.0 as isize, &rules, None);

        let mut to_remove = HashSet::new();

        assert!(wave.cells[index.0].len() > 0);
        to_remove.insert(*wave.cells[index.0].iter().next().unwrap().0);

        let mut new_wave = wave.clone();
        let (_, pb) = new_wave.propagate_remove(index.0, &to_remove);
        assert_ne!(wave.cells, new_wave.cells);
        new_wave.unpropagate_v2(&pb);
        assert_eq!(wave.cells, new_wave.cells);

        TestResult::passed()
    }

    #[test]
    fn asdasfasf() {
        #[rustfmt::skip]
        let rules = Rules::new(
            2,
            2,
            &vec![
                1, 2,
                3, 4
            ],
            &HashSet::new(),
            0,
        );
        let wave = Wave2::new(2, 2, &rules, None);

        let mut to_remove = HashSet::new();

        assert!(wave.cells[0].len() > 0);
        to_remove.insert(*wave.cells[0].iter().next().unwrap().0);

        let mut new_wave = wave.clone();
        let (_, pb) = new_wave.propagate_remove(0, &to_remove);
        assert_ne!(wave.cells, new_wave.cells);
        new_wave.unpropagate_v2(&pb);
        assert_eq!(wave.cells, new_wave.cells);
    }
}
