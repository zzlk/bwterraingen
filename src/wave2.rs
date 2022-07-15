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
use tracing::{debug, error, info, warn};

#[derive(Debug, Clone)]
pub struct Wave2 {
    pub width: isize,
    pub height: isize,
    cells: Vec<HashMap<u16, [i16; 4]>>,
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

                    if wave.propagate(index, &to_remove) == false {
                        panic!();
                    }
                }
            }
        }

        // shrink it to save memory
        for y in 0..height {
            for x in 0..width {
                let index = (x + y * width) as usize;
                let source_cell = &mut wave.cells[index];

                source_cell.shrink_to_fit();
            }
        }

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

    fn calculate_support_for_tile(&mut self, cell_index: usize, tile: u16) {
        // assert!(*self.cells[cell_index].get(&tile).unwrap() == [0, 0, 0, 0]);

        *self.cells[cell_index].get_mut(&tile).unwrap() = [0, 0, 0, 0];

        for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
            let (start_x, start_y) = (
                cell_index as isize % self.width,
                cell_index as isize / self.width,
            );
            assert!(start_x >= 0 && start_x < self.width);
            assert!(start_y >= 0 && start_y < self.height);

            let source_x = start_x - direction.0;
            let source_y = start_y - direction.1;

            if source_x < 0 || source_x >= self.width || source_y < 0 || source_y >= self.height {
                continue;
            }

            let source_index = (source_x + source_y * self.width) as usize;
            let source_cell = &mut self.cells[source_index];

            let mut to_do = Vec::new();
            for (source_tile, _) in &*source_cell {
                if let Some(rules) = self.rules.ruleset[ordinal].get(source_tile) {
                    if let Some(allowed_tile) = rules.get(&tile) {
                        to_do.push((allowed_tile, ordinal));
                    }
                }
            }

            for item in to_do {
                // debug!(
                //     "adding. start: {cell_index}, tile: {}, ordinal: {}",
                //     item.0, item.1
                // );
                if let Some(x) = self.cells[cell_index].get_mut(&item.0) {
                    x[item.1] += 1;
                }
            }
        }
    }

    pub fn unpropagate(&mut self, start: usize, tiles_removed: &HashSet<u16>) {
        // debug!("unpropagating");
        // self.print_wave();
        #[derive(Clone, Eq, PartialEq, Debug)]
        struct Node {
            target_index: usize,
            projected_possibilities: isize,
            tiles_inserted: HashSet<u16>,
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

        let mut tiles_added = HashSet::new();

        for tile in tiles_removed {
            // debug!("start: {start}, tile: {tile}");
            if self.cells[start].insert(*tile, [0, 0, 0, 0]).is_some() {
                panic!();
            } else {
                tiles_added.insert((start, *tile));
                // self.calculate_support_for_tile(start, *tile);
            }
        }

        let mut vec = Vec::<Node>::new();
        vec.push(Node {
            target_index: start,
            projected_possibilities: self.cells[start].len() as isize,
            tiles_inserted: tiles_removed.clone(),
        });

        while vec.len() > 0 {
            // self.print_wave();
            // info!("propagate step");
            let chosen = vec.pop().unwrap();
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

                // debug!("unpropagating. potentially incrementing counts. ordinal: {ordinal}, source: {source_x},{source_y}, target: {target_x},{target_y}");
                // debug!("cells before: {:?}", self.cells);

                // update support for surrounding cells based on newly re-inserted tiles
                let mut tiles_inserted = HashSet::new();
                for inserted_tile in &chosen.tiles_inserted {
                    if let Some(rule) = self.rules.ruleset[ordinal].get(inserted_tile) {
                        for allowed_tile in rule.clone() {
                            if !self.cells[target_index].contains_key(&allowed_tile) {
                                // debug!("inserting new tile");
                                tiles_inserted.insert(allowed_tile);

                                self.cells[target_index].insert(allowed_tile, [0, 0, 0, 0]);
                                tiles_added.insert((target_index, allowed_tile));

                                // add in support from existing tiles
                                // self.calculate_support_for_tile(target_index, allowed_tile);
                            } else {
                                // debug!("inserting new tile2");

                                self.cells[target_index].insert(allowed_tile, [0, 0, 0, 0]);
                                tiles_added.insert((target_index, allowed_tile));

                                // add in support from existing tiles
                                // self.calculate_support_for_tile(target_index, allowed_tile);
                                // let target_tile_possibilities =
                                //     self.cells[target_index].get_mut(&allowed_tile).unwrap();

                                // debug!(
                                //     "    unpropagating. incrementing. allowed_tile: {allowed_tile}"
                                // );
                                // target_tile_possibilities[ordinal] += 1;
                            }
                        }
                    }
                }

                // debug!("cells after: {:?}", self.cells);
                // info!("meme");

                if tiles_inserted.len() > 0 {
                    let mut was_found = false;
                    for i in &mut vec {
                        if i.target_index == target_index {
                            i.tiles_inserted.extend(tiles_inserted.clone());
                            i.projected_possibilities =
                                (self.cells[target_index].len() + i.tiles_inserted.len()) as isize;
                            was_found = true;
                            break;
                        }
                    }

                    if !was_found {
                        vec.push(Node {
                            target_index,
                            projected_possibilities: (self.cells[target_index].len()
                                + tiles_inserted.len())
                                as isize,
                            tiles_inserted,
                        });
                    }

                    vec.sort_by(|a, b| b.projected_possibilities.cmp(&a.projected_possibilities));
                }
            }
        }



        // remove added tiles without sufficient support
        loop {
            // info!("memes");

            // calculate support for all added tiles
            for (index, tile) in &tiles_added {
            // info!("memes2: {}", tiles_added.len());
            if self.cells[*index].contains_key(tile) {
                    self.calculate_support_for_tile(*index, *tile);
                }
            }
            
            let mut was_removed = false;
            for (index, tile) in &tiles_added {
            // info!("memes3");
            for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
                    let target_x = *index as isize % self.width;
                    let target_y = *index as isize / self.width;
                    assert!(target_x >= 0 && target_x < self.width);
                    assert!(target_y >= 0 && target_y < self.height);

                    let source_x = target_x - direction.0;
                    let source_y = target_y - direction.1;

                    if source_x < 0
                        || source_x >= self.width
                        || source_y < 0
                        || source_y >= self.height
                    {
                        continue;
                    }

                    // let source_index = (source_x + source_y * self.width) as usize;

                    let mut has_zero_support = true;

                    if let Some(cell) = self.cells[*index].get(tile) {
                        has_zero_support = has_zero_support && cell[ordinal] == 0;
                    } else {
                        has_zero_support = true;
                    }

                    if has_zero_support {
                        if self.cells[*index].remove(tile).is_some() {
                            was_removed = true;
                        }
                    }
                }
            }

            if was_removed {
                continue;
            }

            break;
        }
    }

    pub fn propagate(&mut self, start: usize, to_remove: &HashSet<u16>) -> bool {
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

        for tile in to_remove {
            assert!(self.cells[start].contains_key(tile));
        }

        let mut vec = Vec::<Node>::new();

        vec.push(Node {
            target_index: start,
            projected_possibilities: self.cells[start].len() as isize,
            to_remove: to_remove.clone(),
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

    pub fn choose_removal_set(&self, mut rng: &mut ThreadRng, index: usize) -> HashSet<u16> {
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
        self.print_wave();

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
                // unpropagate?
                current_wave = waves.pop_front().unwrap();
                // let propagation = waves.pop_front().unwrap();
                warn!("backtrack");
                // self.unpropagate(waves.index, &propagation.to_remove);
                continue;
            }

            let (index, _pop_cnt) = current_index.unwrap();

            if Instant::now().duration_since(last_time).as_millis() as u32 > update_interval {
                last_time = Instant::now();
                update(&current_wave);
            }

            let mut new_wave = current_wave.clone();
            let to_remove = new_wave.choose_removal_set(&mut rng, index);

            if !new_wave.propagate(index, &to_remove) {
                // self.unpropagate(index, &to_remove);
                continue;
            }

            if new_wave.is_done() {
                return anyhow::Ok(new_wave.clone());
            }

            // this would normally be a recursive call but I can't figure out how to increase the stack size in wasm.
            waves.push_front(current_wave);
            // propagations.push_front(Propagation { index, to_remove });
            indices.push_front(current_indices);
            current_wave = new_wave;
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

    #[test]
    fn asfdasfasd() {
        #[rustfmt::skip]
        let rules = Rules::new(2, 3, &vec![
            1, 2, 
            2, 1, 
            1, 1], &HashSet::new(), 0);
        let wave = Wave2::new(3, 1, &rules);

        let mut to_remove = HashSet::new();
        to_remove.insert(1);

        let mut wave1 = wave.clone();
        wave1.propagate(0, &to_remove);
        assert_ne!(wave.cells, wave1.cells);
        wave1.unpropagate(0, &to_remove);
        assert_eq!(wave.cells, wave1.cells);
    }

    #[test]
    fn dasfasdf() {
        #[rustfmt::skip]
        let rules = Rules::new(2, 3, &vec![
            1, 2, 
            2, 1, 
            1, 2], &HashSet::new(), 0);
        let wave = Wave2::new(2, 2, &rules);

        let mut to_remove = HashSet::new();
        to_remove.insert(1);

        let mut wave1 = wave.clone();
        wave1.propagate(0, &to_remove);
        assert_ne!(wave.cells, wave1.cells);
        wave1.unpropagate(0, &to_remove);
        assert_eq!(wave.cells, wave1.cells);
    }

    #[test]
    fn dasfsdgagasdgasdf() {
        #[rustfmt::skip]
        let rules = Rules::new(2, 2, &vec![
            0, 0,
            1, 1], &HashSet::new(), 0);
        let wave = Wave2::new(2, 2, &rules);

        let mut to_remove = HashSet::new();
        to_remove.insert(1);

        let mut wave1 = wave.clone();
        wave1.propagate(2, &to_remove);
        assert_ne!(wave.cells, wave1.cells);
        wave1.unpropagate(2, &to_remove);
        assert_eq!(wave.cells, wave1.cells);
    }

    #[test]
    fn dfgsdfgsdfs() {
        #[rustfmt::skip]
        let rules = Rules::new(2, 2, &vec![
            0, 1,
            1, 1], &HashSet::new(), 0);
        let wave = Wave2::new(2, 2, &rules);

        let mut to_remove = HashSet::new();
        to_remove.insert(1);

        let mut wave1 = wave.clone();
        wave1.propagate(3, &to_remove);
        assert_ne!(wave.cells, wave1.cells);
        wave1.unpropagate(3, &to_remove);
        assert_eq!(wave.cells, wave1.cells);
    }

    #[test]
    fn sadafsag() {
        #[rustfmt::skip]
        let rules = Rules::new(3, 3, &vec![
            0, 0, 0,
            0, 2, 1,
            0, 0, 0], &HashSet::new(), 0);
        let wave = Wave2::new(3, 3, &rules);

        let mut to_remove = HashSet::new();
        to_remove.insert(0);

        let mut wave1 = wave.clone();
        wave1.propagate(7, &to_remove);
        assert_ne!(wave.cells, wave1.cells);
        wave1.unpropagate(7, &to_remove);
        assert_eq!(wave.cells, wave1.cells);
    }

    const MAX_WAVE_DIMENSION: usize = 18;

    #[derive(Debug, Clone)]
    struct ArbitraryIndex {
        index: usize,
    }

    impl Arbitrary for ArbitraryIndex {
        fn arbitrary(g: &mut Gen) -> ArbitraryIndex {
            ArbitraryIndex {
                index: usize::arbitrary(g) % (MAX_WAVE_DIMENSION * MAX_WAVE_DIMENSION),
            }
        }
    }

    #[quickcheck]
    fn fasdfasdf(index: ArbitraryIndex) {
        #[rustfmt::skip]
        let rules = Rules::new(2, 3, &vec![
            1, 2, 
            2, 1, 
            1, 2], &HashSet::new(), 0);
        let wave = Wave2::new(
            MAX_WAVE_DIMENSION as isize,
            MAX_WAVE_DIMENSION as isize,
            &rules,
        );

        let mut to_remove = HashSet::new();
        to_remove.insert(*wave.cells[index.index].iter().next().unwrap().0);

        let mut wave1 = wave.clone();
        wave1.propagate(index.index, &to_remove);
        assert_ne!(wave.cells, wave1.cells);
        wave1.unpropagate(index.index, &to_remove);
        assert_eq!(wave.cells, wave1.cells);
    }

    #[ignore]
    #[quickcheck]
    fn fasdfsadg(index: ArbitraryIndex, ruledata: Vec<u16>) -> TestResult {
        if ruledata.len() < MAX_WAVE_DIMENSION * MAX_WAVE_DIMENSION {
            return TestResult::discard();
        }

        let rules = Rules::new(
            MAX_WAVE_DIMENSION as isize,
            MAX_WAVE_DIMENSION as isize,
            &ruledata,
            &HashSet::new(),
            0,
        );
        let wave = Wave2::new(
            MAX_WAVE_DIMENSION as isize,
            MAX_WAVE_DIMENSION as isize,
            &rules,
        );

        let mut to_remove = HashSet::new();
        to_remove.insert(*wave.cells[index.index].iter().next().unwrap().0);

        let mut wave1 = wave.clone();
        wave1.propagate(index.index, &to_remove);
        assert_ne!(wave.cells, wave1.cells);
        wave1.unpropagate(index.index, &to_remove);
        assert_eq!(wave.cells, wave1.cells);

        TestResult::passed()
    }
}
