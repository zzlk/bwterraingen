use crate::bitset::BitSet;
use crate::rules::Rules;
use crate::{DIRECTIONS, MAX_TILE_BITS, MAX_TILE_IDS};
use anyhow::Result;
use instant::Instant;
use rand::distributions::Uniform;
use rand::prelude::ThreadRng;
use rand::prelude::{Distribution, SliceRandom};
use std::cmp::{self, Ordering};
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;
use tracing::{debug, error, info};

#[derive(Debug)]
struct FlatRules {
    ruleset: [Vec<Option<Vec<u16>>>; 4],
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Cell {
    stuff: [Option<[i16; 4]>; MAX_TILE_IDS],
    cached_len: usize,
}

impl Cell {
    fn new() -> Cell {
        Cell {
            stuff: [Option::<[i16; 4]>::None; MAX_TILE_IDS],
            cached_len: 0,
        }
    }

    fn get(&self, index: usize) -> &Option<[i16; 4]> {
        &self.stuff[index]
    }

    fn get_mut(&mut self, index: usize) -> &mut Option<[i16; 4]> {
        unsafe { self.stuff.get_unchecked_mut(index) }
    }

    fn get_unchecked_mut(&mut self, index: usize) -> &mut Option<[i16; 4]> {
        unsafe { self.stuff.get_unchecked_mut(index) }
    }

    fn remove(&mut self, index: usize) {
        if self.stuff[index].is_some() {
            self.cached_len -= 1;
            self.stuff[index] = None;
        }
    }

    fn set(&mut self, index: usize, v: [i16; 4]) {
        if self.stuff[index].is_none() {
            self.cached_len += 1;
        }
        self.stuff[index] = Some(v);
    }

    fn get_singular(&self) -> u16 {
        self.stuff
            .iter()
            .enumerate()
            .filter_map(|x| x.1.as_ref().map(|y| (x.0 as u16, y)))
            .next()
            .unwrap()
            .0
    }

    fn iter(&self) -> CellIterator {
        self.into_iter()
    }
}

impl<'a> IntoIterator for &'a Cell {
    type Item = (u16, [i16; 4]);
    type IntoIter = CellIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        CellIterator {
            index: 0,
            parent: self,
        }
    }
}

struct CellIterator<'a> {
    index: usize,
    parent: &'a Cell,
}

impl<'a> Iterator for CellIterator<'a> {
    type Item = (u16, [i16; 4]);

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < self.parent.stuff.len() {
            self.index += 1;

            if let Some(support) = self.parent.stuff[self.index - 1] {
                return Some(((self.index - 1) as u16, support));
            }
        }

        None
    }
}

#[derive(Debug, Clone)]
pub struct Wave2 {
    pub width: isize,
    pub height: isize,
    cells: Vec<Cell>,
    rules: Rc<FlatRules>,
    example_cell: Rc<Cell>,
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

        // Calculate the kind of 'maximum' cell
        for (ordinal, _) in DIRECTIONS.iter().enumerate() {
            for (_, allowed_tiles) in rules.ruleset[ordinal]
                .iter()
                .enumerate()
                .filter_map(|x| x.1.as_ref().map(|y| (x.0 as u16, y)))
            {
                for allowed_tile in allowed_tiles {
                    if example_cell.get(*allowed_tile as usize).is_none() {
                        example_cell.set(*allowed_tile as usize, [0, 0, 0, 0]);
                    }

                    example_cell
                        .get_mut(*allowed_tile as usize)
                        .as_mut()
                        .unwrap()[ordinal] += 1;
                }
            }
        }

        let mut wave = Wave2 {
            width: width,
            height: height,
            cells: vec![example_cell.clone(); (width * height) as usize], //Vec::with_capacity((width * height) as usize),
            rules: Rc::new(rules),
            example_cell: Rc::new(example_cell),
            inverse_mapping: Rc::new(inverse_mapping),
        };

        if let Some((template_map, mask_tile)) = &template_map_and_mask_tile {
            for y in 0..height {
                for x in 0..width {
                    let index = (x + y * width) as usize;

                    if template_map[index] != *mask_tile {
                        let mut to_remove: HashSet<_> =
                            wave.inverse_mapping.keys().cloned().collect();

                        assert!(to_remove.remove(&mapping[&template_map[index]]));

                        wave.propagate_remove(index, &to_remove);
                    }
                }
            }
        }

        // remove tiles with insufficient support
        loop {
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
                                to_remove.insert(tile);
                            }
                        }
                    }

                    if !to_remove.is_empty() {
                        was_empty = false;
                        for tile in to_remove {
                            wave.cells[index].remove(tile as usize);

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
                                            wave.cells[target_index].get_mut(*allowed_tile as usize)
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
                    self.cells[(x + y * self.width) as usize].cached_len
                );
            }
            info!("{}", output);
        }
    }

    pub fn render(&self) -> Vec<u16> {
        let mut ret = Vec::new();
        for v in &self.cells {
            if v.cached_len == 1 {
                ret.push(self.inverse_mapping[&(v.get_singular())]);
            } else {
                ret.push(15);
            }
        }
        ret
    }

    pub fn is_done(&self) -> bool {
        for v in &self.cells {
            if v.cached_len != 1 {
                return false;
            }
        }

        true
    }

    pub fn unpropagate_v2(&mut self, propagation_backups: &HashMap<(usize, u16), [i16; 4]>) {
        for ((index, tile), pb) in propagation_backups {
            self.cells[*index].set(*tile as usize, *pb);
        }
    }

    pub fn propagate_remove(
        &mut self,
        start: usize,
        to_remove: &HashSet<u16>,
    ) -> (Option<usize>, HashMap<(usize, u16), [i16; 4]>) {
        #[derive(Clone, Eq, PartialEq, Debug)]
        struct Node {
            target_index: usize,
            removed: Vec<u16>,
        }

        impl Ord for Node {
            fn cmp(&self, other: &Self) -> Ordering {
                self.target_index.cmp(&other.target_index)
            }
        }

        impl PartialOrd for Node {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        let rules = self.rules.clone();

        let mut pq = Vec::<Node>::new();

        let mut backup = HashMap::with_capacity(to_remove.len() * 10);

        let mut removed = Vec::new();
        for tile in to_remove {
            if self.cells[start].get(*tile as usize).is_some() {
                if !backup.contains_key(&(start, *tile)) {
                    backup.insert(
                        (start, *tile),
                        self.cells[start].get(*tile as usize).unwrap(),
                    );
                }
                self.cells[start].remove(*tile as usize);
                removed.push(*tile);
            }
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
                        // info!(
                        //     "target_x: {}, target_y: {}, ordinal: {}, &chosen.removed: {}, rule.len: {}, target_cell.len(): {}",
                        //     target_x,
                        //     target_y,
                        //     ordinal,
                        //     chosen.removed.len(),
                        //     rule.len(),
                        //     target_cell.cached_len
                        // );
                        for allowed_tile in rule {
                            let m = target_cell.get_unchecked_mut(*allowed_tile as usize);
                            if m.is_none() {
                                continue;
                            }

                            if let &mut Some(ref mut support) = m {
                                backup
                                    .entry((target_index, *allowed_tile))
                                    .or_insert(*support);

                                support[ordinal] -= 1;

                                if support[ordinal] <= 0 {
                                    target_cell.remove(*allowed_tile as usize);
                                    newly_removed_tiles.push(*allowed_tile);

                                    if target_cell.cached_len == 0 {
                                        backup.shrink_to_fit();
                                        return (Some(target_index), backup);
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

        // info!("post-propagate");
        // self.print_wave();

        backup.shrink_to_fit();
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
            .map(|x| x.cached_len)
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
        if self.cells[index].cached_len <= 1 {
            panic!("can't collapse this one.");
        }

        let chosen_index = Uniform::from(0..self.cells[index].cached_len).sample(&mut rng);

        let mut hs = HashSet::with_capacity(self.cells[index].cached_len - 1);

        hs.extend(
            self.cells[index]
                .iter()
                .enumerate()
                .filter(|(index, _)| *index != chosen_index)
                .map(|(_, b)| b.0),
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

        let mut propagation_backups = VecDeque::new();
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
            while propagation_backups.len() > 400 {
                indices.pop_back();
                propagation_backups.pop_back();
            }

            let current_index = current_indices.pop();

            if current_index == None {
                if propagation_backups.len() == 0 || indices.len() == 0 {
                    error!("No way to unpropagate.");
                    return anyhow::Ok(current_wave.clone());
                }

                current_wave.unpropagate_v2(&propagation_backups.pop_front().unwrap());
                current_indices = indices.pop_front().unwrap();
                continue;
            }

            let (index, _pop_cnt) = current_index.unwrap();

            if Instant::now().duration_since(last_time).as_millis() as u32 > update_interval {
                last_time = Instant::now();
                update(&current_wave);
            }

            let to_remove = current_wave.choose_removal_set(&mut rng, index);

            let (failed_at, propagation_backup) = current_wave.propagate_remove(index, &to_remove);

            if let Some(failed_at) = failed_at {
                // self.unpropagate(index, &to_remove);
                current_wave.unpropagate_v2(&propagation_backup);
                depth -= 1;

                // let failed_x = failed_at as isize % self.width;
                // let failed_y = failed_at as isize / self.width;

                // let failures_entry = failures.entry((failed_x, failed_y)).or_insert(0);

                failures_at_current_max_depth += 1;

                if failures_at_current_max_depth > 128 {
                    failures_at_current_max_depth = 0;
                    times_nuked_at_current_max_depth += 1;

                    error!("NUKE");

                    for _ in 0..std::cmp::min(
                        times_nuked_at_current_max_depth * times_nuked_at_current_max_depth,
                        propagation_backups.len(),
                    ) {
                        depth -= 1;
                        current_wave.unpropagate_v2(&propagation_backups.pop_front().unwrap());
                        indices.pop_front().unwrap();
                    }

                    current_indices = current_wave.get_entropy_indices_in_order(&mut rng, 10)?;
                }
                continue;
            }

            if current_wave.is_done() {
                return anyhow::Ok(current_wave.clone());
            }

            depth += 1;
            if depth > max_depth {
                failures_at_current_max_depth = 0;
                times_nuked_at_current_max_depth = 0;
                max_depth = depth;
            }

            propagation_backups.push_front(propagation_backup);
            indices.push_front(current_indices);
            current_indices = current_wave.get_entropy_indices_in_order(&mut rng, 2)?;

            indices.shrink_to_fit();
            propagation_backups.shrink_to_fit();
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

        assert!(wave.cells[index.0].cached_len > 0);
        to_remove.insert(wave.cells[index.0].iter().next().unwrap().0 as u16);

        let mut new_wave = wave.clone();
        let (s, pb) = new_wave.propagate_remove(index.0, &to_remove);
        if s.is_none() {
            assert_ne!(wave.cells, new_wave.cells);
            new_wave.unpropagate_v2(&pb);
            assert_eq!(wave.cells, new_wave.cells);
        }

        TestResult::passed()
    }
}
