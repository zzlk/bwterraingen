use crate::bitset::{BitSet, BitSetIterator};
use crate::rules::Rules;
use crate::{DIRECTIONS, MAX_TILE_BITS, MAX_TILE_IDS};
use anyhow::Result;
use hashbrown::{HashMap, HashSet};
use instant::Instant;
use rand::distributions::Uniform;
use rand::prelude::ThreadRng;
use rand::prelude::{Distribution, SliceRandom};
use std::cmp::{self, Ordering};
use std::collections::VecDeque;
use std::rc::Rc;
use tracing::{debug, error, info};

#[derive(Debug)]
struct FlatRules {
    ruleset: [Vec<Option<Vec<u16>>>; 4],
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Cell {
    stuff: [[isize; 4]; MAX_TILE_IDS],
    active: BitSet<MAX_TILE_BITS>,
    cached_len: usize,
}

impl Cell {
    fn new() -> Cell {
        Cell {
            stuff: [[0isize; 4]; MAX_TILE_IDS],
            active: BitSet::new(),
            cached_len: 0,
        }
    }

    fn get(&self, tile: usize) -> &[isize; 4] {
        &self.stuff[tile]
    }

    fn get_mut(&mut self, tile: usize) -> &mut [isize; 4] {
        &mut self.stuff[tile]
    }

    fn decrement_unchecked(&mut self, tile: usize, ordinal: usize) -> bool {
        unsafe {
            self.stuff.get_unchecked_mut(tile)[ordinal] -= 1;
            self.stuff.get_unchecked_mut(tile)[ordinal] == 0
        }
    }

    fn increment_unchecked(&mut self, tile: usize, ordinal: usize) {
        unsafe {
            self.stuff.get_unchecked_mut(tile)[ordinal] += 1;
        }
    }

    fn deactivate(&mut self, tile: usize) {
        if self.active.remove(tile) {
            self.cached_len -= 1;
        }
    }

    fn activate(&mut self, tile: usize) {
        if self.active.insert(tile) {
            self.cached_len += 1;
        }
    }

    fn is_active(&self, tile: usize) -> bool {
        self.active.get(tile)
    }

    fn set(&mut self, tile: usize, v: [isize; 4]) {
        self.stuff[tile] = v;
    }

    fn len(&self) -> usize {
        self.cached_len
    }

    fn get_singular(&self) -> u16 {
        self.stuff
            .iter()
            .enumerate()
            .filter(|x| self.is_active(x.0))
            .map(|x| (x.0 as u16, x.1))
            .next()
            .unwrap()
            .0
    }

    fn iter(&self) -> BitSetIterator<MAX_TILE_BITS> {
        self.active.iter()
    }
}

// impl<'a> IntoIterator for &'a Cell {
//     type Item = (u16, [isize; 4]);
//     type IntoIter = CellIterator<'a>;

//     fn into_iter(self) -> Self::IntoIter {
//         CellIterator {
//             tile: 0,
//             parent: self,
//         }
//     }
// }

// struct CellIterator<'a> {
//     tile: usize,
//     parent: &'a Cell,
// }

// impl<'a> Iterator for CellIterator<'a> {
//     type Item = (u16, [isize; 4]);

//     fn next(&mut self) -> Option<Self::Item> {
//         while self.tile < self.parent.stuff.len() {
//             self.tile += 1;

//             if self.parent.is_active(self.tile - 1) {
//                 return Some(((self.tile - 1) as u16, self.parent.stuff[self.tile - 1]));
//             }
//         }

//         None
//     }
// }

#[derive(Debug, Clone)]
pub struct Wave2 {
    pub width: isize,
    pub height: isize,
    cells: Vec<Cell>,
    rules: Rc<FlatRules>,
    inverse_mapping: Rc<HashMap<u16, u16>>,
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
            vec![(); MAX_TILE_IDS].into_iter().map(|_| None).collect(),
            vec![(); MAX_TILE_IDS].into_iter().map(|_| None).collect(),
            vec![(); MAX_TILE_IDS].into_iter().map(|_| None).collect(),
            vec![(); MAX_TILE_IDS].into_iter().map(|_| None).collect(),
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

            // add tiles from template map
            if let Some((template_map, _mask_tile)) = &template_map_and_mask_tile {
                for tile in template_map {
                    if !mapping.contains_key(&tile) {
                        inverse_mapping.insert(mapping.len() as u16, *tile);
                        mapping.insert(*tile, mapping.len() as u16);
                    }
                }
            }

            // convert rules to dense format
            for (ordinal, _) in DIRECTIONS.iter().enumerate() {
                let old_rule = &rules.ruleset[ordinal];
                let new_rule = &mut new_rules[ordinal];

                let mut rule: Vec<_> = vec![(); MAX_TILE_IDS].into_iter().map(|_| None).collect();
                for (tile, allowed_tiles) in old_rule {
                    let mut allowed_tiles_remapped = HashSet::new();
                    for allowed_tile in allowed_tiles {
                        allowed_tiles_remapped.insert(mapping[allowed_tile]);
                    }
                    let mut v: Vec<_> = allowed_tiles_remapped.drain().collect();
                    v.sort_unstable();
                    rule[mapping[tile] as usize] = Some(v);
                }

                *new_rule = rule;
            }
        }

        info!(
            "total unique tiles: {}",
            new_rules[0].iter().filter(|x| x.is_some()).count()
        );

        let rules = FlatRules { ruleset: new_rules };

        // initialize all cells in the map to be able to be every possible tile.
        let mut example_cell = Cell::new();

        // Calculate the kind of 'all-tiles' cell
        for tile in inverse_mapping.keys() {
            example_cell.set(*tile as usize, [0, 0, 0, 0]);
            example_cell.activate(*tile as usize);
        }

        let mut wave = Wave2 {
            width: width,
            height: height,
            cells: vec![example_cell.clone(); (width * height) as usize], //Vec::with_capacity((width * height) as usize),
            rules: Rc::new(rules),
            inverse_mapping: Rc::new(inverse_mapping),
        };

        // Deactivate masked out tiles.
        if let Some((template_map, mask_tile)) = &template_map_and_mask_tile {
            for y in 0..height {
                for x in 0..width {
                    let index = (x + y * width) as usize;

                    if template_map[index] != *mask_tile {
                        let other_tiles: Vec<_> = wave.cells[index]
                            .iter()
                            .filter(|x| *x != mapping[&template_map[index]] as usize)
                            .collect();
                        for tile in other_tiles {
                            wave.cells[index].deactivate(tile as usize);
                        }
                    }
                }
            }
        }

        // calculate support of every tile
        {
            let all_tiles = wave.inverse_mapping.keys().cloned().collect();
            for target_y in 0..wave.height {
                for target_x in 0..wave.width {
                    let index = (target_x + target_y * wave.width) as usize;

                    wave.calculate_support(index, &all_tiles);
                }
            }
        }

        // find cells with insufficient support, ignore template tiles
        let mut unsupported_tiles = HashMap::new();
        for target_y in 0..wave.height {
            for target_x in 0..wave.width {
                let index = (target_x + target_y * wave.width) as usize;

                if let Some((template_map, mask_tile)) = &template_map_and_mask_tile {
                    if template_map[index] != *mask_tile {
                        println!("memes");
                        continue;
                    }
                }

                for tile in wave.cells[index].iter() {
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

                        if wave.cells[index].get(tile)[ordinal] <= 0 {
                            unsupported_tiles
                                .entry(index)
                                .or_insert(HashSet::new())
                                .insert(tile as u16);
                        }
                    }
                }
            }
        }

        // remove unsupported tiles
        for (index, tiles) in unsupported_tiles {
            let (contradiction, _) = wave.propagate_remove(index, &tiles);
            if contradiction {
                panic!("wave is unsatisfiable.");
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
            if v.len() == 1 {
                ret.push(self.inverse_mapping[&(v.get_singular())]);
            } else {
                ret.push(15);
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

    fn calculate_support(&mut self, index: usize, tiles: &HashSet<u16>) {
        let target_x = index as isize % self.width;
        let target_y = index as isize / self.width;
        let target_index = (target_x + target_y * self.width) as usize;

        for tile in tiles {
            self.cells[target_index].set(*tile as usize, [0, 0, 0, 0]);
        }

        for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
            let source_x = target_x - direction.0;
            let source_y = target_y - direction.1;

            if source_x < 0 || source_x >= self.width || source_y < 0 || source_y >= self.height {
                continue;
            }

            let source_index = (source_x + source_y * self.width) as usize;

            let source_tiles: Vec<_> = self.cells[source_index].iter().collect();
            for source_tile in source_tiles {
                if let Some(rule) = &self.rules.ruleset[ordinal][source_tile as usize] {
                    for allowed_tile in rule {
                        if tiles.contains(allowed_tile) {
                            self.cells[target_index].get_mut(*allowed_tile as usize)[ordinal] += 1;
                        }
                    }
                }
            }
        }
    }

    pub fn propagate_add_v2(&mut self, deactivations: &Vec<(usize, u16)>) {
        for (index, tile) in deactivations {
            assert!(!self.cells[*index].is_active(*tile as usize));
            self.cells[*index].activate(*tile as usize);

            let source_x = *index as isize % self.width;
            let source_y = *index as isize / self.width;

            for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
                let target_x = source_x + direction.0;
                let target_y = source_y + direction.1;

                if target_x < 0 || target_x >= self.width || target_y < 0 || target_y >= self.height
                {
                    continue;
                }

                let target_index = (target_x + target_y * self.width) as usize;

                if let Some(ruleset) = &self.rules.ruleset[ordinal][*tile as usize] {
                    for allowed_tile in ruleset {
                        self.cells[target_index]
                            .increment_unchecked(*allowed_tile as usize, ordinal);
                    }
                }
            }
        }
    }

    pub fn propagate_remove(
        &mut self,
        start: usize,
        to_remove: &HashSet<u16>,
    ) -> (bool, Vec<(usize, u16)>) {
        #[derive(Clone, Eq, PartialEq, Debug)]
        struct Node {
            target_index: usize,
            removed: Vec<u16>,
        }

        impl Ord for Node {
            fn cmp(&self, other: &Self) -> Ordering {
                other.target_index.cmp(&self.target_index)
            }
        }

        impl PartialOrd for Node {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        let mut contradiction = false;

        let rules = self.rules.clone();

        let mut pq = Vec::<Node>::new();

        let mut backup = Vec::new();

        let mut removed = Vec::new();
        for tile in to_remove {
            if self.cells[start].is_active(*tile as usize) {
                backup.push((start, *tile));
                self.cells[start].deactivate(*tile as usize);
                removed.push(*tile);
            }
        }

        if self.cells[start].len() == 0 {
            contradiction = true;
        }

        pq.push(Node {
            target_index: start,
            removed: removed.clone(),
        });

        while pq.len() > 0 {
            let chosen = pq.pop().unwrap();
            let current_index = chosen.target_index;

            let (source_x, source_y) = (
                current_index as isize % self.width,
                current_index as isize / self.width,
            );
            assert!(source_x >= 0 && source_x < self.width);
            assert!(source_y >= 0 && source_y < self.height);

            // chosen.removed.sort_unstable();
            // self.print_wave();
            // info!(
            //     "source: {source_x},{source_y}, removed: {:?}",
            //     chosen.removed,
            // );

            for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
                let target_x = source_x + direction.0;
                let target_y = source_y + direction.1;

                if target_x < 0 || target_x >= self.width || target_y < 0 || target_y >= self.height
                {
                    continue;
                }

                let target_index = (target_x + target_y * self.width) as usize;
                let target_cell = &mut self.cells[target_index];

                let mut newly_removed_tiles = Vec::new();
                let mut bs = BitSet::<MAX_TILE_BITS>::new();
                for removed_tile in &chosen.removed {
                    if !bs.insert(*removed_tile as usize) {
                        continue;
                    }

                    if let Some(rule) = &rules.ruleset[ordinal][*removed_tile as usize] {
                        for allowed_tile in rule {
                            let support_went_to_zero =
                                target_cell.decrement_unchecked(*allowed_tile as usize, ordinal);

                            if support_went_to_zero {
                                if target_cell.is_active(*allowed_tile as usize) {
                                    target_cell.deactivate(*allowed_tile as usize);
                                    backup.push((target_index, *allowed_tile));
                                    newly_removed_tiles.push(*allowed_tile);

                                    if target_cell.len() == 0 {
                                        contradiction = true;
                                    }
                                }
                            }
                        }
                    }
                }

                if newly_removed_tiles.len() > 0 {
                    let mut was_found = false;
                    for i in &mut pq {
                        if i.target_index == target_index {
                            i.removed.extend(&newly_removed_tiles);
                            was_found = true;
                            break;
                        }
                    }

                    if !was_found {
                        pq.push(Node {
                            target_index,
                            removed: newly_removed_tiles,
                        });
                    }

                    pq.sort();
                }
            }
        }

        (contradiction, backup)
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
        entropies.sort_unstable_by_key(|x| (x.1 as isize));

        entropies.truncate(cmp::max(2, at_least));

        entropies.sort_unstable_by_key(|x| -(x.1 as isize));

        anyhow::Ok(entropies)
    }

    pub fn choose_removal_set(&self, mut rng: &mut ThreadRng, index: usize) -> HashSet<u16> {
        if self.cells[index].len() <= 1 {
            panic!("can't collapse this one.");
        }

        let chosen_index = Uniform::from(0..self.cells[index].len()).sample(&mut rng);

        let mut hs = HashSet::with_capacity(self.cells[index].len() - 1);

        hs.extend(
            self.cells[index]
                .iter()
                .enumerate()
                .filter(|(index, _)| *index != chosen_index)
                .map(|(_, b)| b as u16),
        );

        hs
    }

    pub fn logical_conclusion<F: Fn(&Wave2)>(
        &mut self,
        update: &F,
        update_interval: u32,
        _nuke_radius: isize,
    ) -> Result<Wave2> {
        update(&self);

        self.cells.shrink_to_fit();

        let mut rng = rand::thread_rng();

        let mut deactivations = VecDeque::new();
        let mut indices = VecDeque::new();

        let mut current_wave = self.clone();
        let mut current_indices;

        let mut last_time = Instant::now();

        let mut failures_at_current_max_depth = 0;
        let mut depth = 0;
        let mut max_depth = 0;
        let mut times_nuked_at_current_max_depth = 0;

        current_indices = current_wave.get_entropy_indices_in_order(&mut rng, 100000)?;

        debug!("start main loop");

        loop {
            let current_index = current_indices.pop();

            if current_index == None {
                if deactivations.len() == 0 || indices.len() == 0 {
                    error!("No way to unpropagate.");
                    return anyhow::Ok(current_wave);
                }

                let deactivated = deactivations.pop_front().unwrap();
                current_wave.propagate_add_v2(&deactivated);
                // current_wave.unpropagate_v2(&propagation_backups.pop_front().unwrap());
                current_indices = indices.pop_front().unwrap();
                continue;
            }

            let (index, _pop_cnt) = current_index.unwrap();

            if Instant::now().duration_since(last_time).as_millis() as u32 > update_interval {
                last_time = Instant::now();
                update(&current_wave);
            }

            let to_remove = current_wave.choose_removal_set(&mut rng, index);

            let (contradiction, deactivated) = current_wave.propagate_remove(index, &to_remove);

            if contradiction {
                info!("meme");
                current_wave.propagate_add_v2(&deactivated);
                // current_wave.unpropagate_v2(&propagation_backup);
                depth -= 1;

                // let failed_x = failed_at as isize % self.width;
                // let failed_y = failed_at as isize / self.width;

                // let failures_entry = failures.entry((failed_x, failed_y)).or_insert(0);

                failures_at_current_max_depth += 1;

                if failures_at_current_max_depth > 32 {
                    failures_at_current_max_depth = 0;
                    times_nuked_at_current_max_depth += 1;

                    error!("NUKE");

                    for _ in 0..std::cmp::min(
                        times_nuked_at_current_max_depth * times_nuked_at_current_max_depth,
                        deactivations.len(),
                    ) {
                        depth -= 1;
                        let deactivated = deactivations.pop_front().unwrap();
                        current_wave.propagate_add_v2(&deactivated);
                        // current_wave.unpropagate_v2(&propagation_backups.pop_front().unwrap());
                        indices.pop_front().unwrap();
                    }

                    current_indices = current_wave.get_entropy_indices_in_order(&mut rng, 10)?;
                }
                continue;
            }

            if current_wave.is_done() {
                return anyhow::Ok(current_wave);
            }

            depth += 1;
            if depth > max_depth {
                failures_at_current_max_depth = 0;
                times_nuked_at_current_max_depth = 0;
                max_depth = depth;
            }

            deactivations.push_front(deactivated);
            indices.push_front(current_indices);
            current_indices = current_wave.get_entropy_indices_in_order(&mut rng, 2)?;

            indices.shrink_to_fit();
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

    #[test]
    fn basic_1x1() {
        #[rustfmt::skip]
        let rules = Rules::new(
            2,
            2,
            &vec![
                0, 0,
                0, 0   
            ],
            &HashSet::new(),
            0,
        );
        let wave = Wave2::new(1, 1, &rules, None);

        assert_eq!(*wave.cells[0].get(0), [0, 0, 0, 0]);
    }

    #[test]
    fn basic_1x2() {
        #[rustfmt::skip]
        let rules = Rules::new(
            2,
            2,
            &vec![
                0, 0,
                0, 0,
            ],
            &HashSet::new(),
            0,
        );
        let wave = Wave2::new(1, 2, &rules, None);

        assert_eq!(wave.cells.len(), 2);
        assert_eq!(*wave.cells[0].get(0), [1, 0, 0, 0]);
        assert_eq!(*wave.cells[1].get(0), [0, 0, 1, 0]);
    }

    #[test]
    fn basic_2x2() {
        #[rustfmt::skip]
        let rules = Rules::new(
            2,
            2,
            &vec![
                0, 0,
                0, 0,
            ],
            &HashSet::new(),
            0,
        );
        let wave = Wave2::new(2, 2, &rules, None);

        assert_eq!(*wave.cells[0].get(0), [1, 0, 0, 1]);
        assert_eq!(*wave.cells[1].get(0), [1, 1, 0, 0]);
        assert_eq!(*wave.cells[2].get(0), [0, 0, 1, 1]);
        assert_eq!(*wave.cells[3].get(0), [0, 1, 1, 0]);
    }

    #[quickcheck]
    fn quickcheck_testcase(
        index: BoundedInt<0, { 4 * 4 }>,
        wave_width: BoundedInt<1, 3>,
        wave_height: BoundedInt<1, 3>,
        rule_width: BoundedInt<3, 8>,
        rule_height: BoundedInt<3, 8>,
        ruledata: Vec<BoundedInt<0, 4>>,
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
            &ruledata.iter().map(|x| x.0 as u16).collect(),
            &HashSet::new(),
            0,
        );
        let wave = Wave2::new(wave_width.0 as isize, wave_height.0 as isize, &rules, None);

        let mut to_remove = HashSet::new();

        assert!(wave.cells[index.0].len() > 0);
        to_remove.insert(wave.cells[index.0].iter().next().unwrap() as u16);

        let mut new_wave = wave.clone();
        // let (s, pb) = new_wave.propagate_remove(index.0, &to_remove);
        let (_contradiction, deactivations) = new_wave.propagate_remove(index.0, &to_remove);
        // if s.is_none() {
        // assert_ne!(wave.cells, new_wave.cells);
        // new_wave.unpropagate_v2(&pb);
        // assert_eq!(wave.cells, new_wave.cells);
        new_wave.propagate_add_v2(&deactivations);
        assert_eq!(wave.cells, new_wave.cells);
        // }

        TestResult::passed()
    }

    #[test]
    fn sadfsadfsdf() {
        #[rustfmt::skip]
        let rules = Rules::new(
            2,
            2,
            &vec![
                0, 1,
                2, 3,
            ],
            &HashSet::new(),
            0,
        );

        #[rustfmt::skip]
        let wave = Wave2::new(2 as isize, 2 as isize, &rules, Some((vec![
            4, 1,
            2, 3], 4)));

        let mut to_remove = HashSet::new();

        let remove_index = 0;

        assert!(wave.cells[remove_index].len() > 0);
        to_remove.insert(wave.cells[remove_index].iter().next().unwrap() as u16);

        let mut new_wave = wave.clone();
        // let (s, pb) = new_wave.propagate_remove(index.0, &to_remove);
        let (_contradiction, deactivations) = new_wave.propagate_remove(remove_index, &to_remove);
        // if s.is_none() {
        // assert_ne!(wave.cells, new_wave.cells);
        // new_wave.unpropagate_v2(&pb);
        // assert_eq!(wave.cells, new_wave.cells);
        new_wave.propagate_add_v2(&deactivations);
        assert_eq!(wave.cells, new_wave.cells);
        // }
    }
}
