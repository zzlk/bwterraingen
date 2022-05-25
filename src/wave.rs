use crate::{rules::Rules, BitSet};
use crate::{DIRECTIONS, N};
use anyhow::anyhow;
use anyhow::Result;
use cached::proc_macro::cached;
use rand::distributions::Uniform;
use rand::prelude::ThreadRng;
use rand::prelude::{Distribution, SliceRandom};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) struct Wave {
    pub(crate) width: isize,
    pub(crate) height: isize,
    pub(crate) array: Vec<BitSet<N>>,
    pub(crate) rules: Rc<[HashMap<usize, BitSet<N>>; 4]>,
    pub(crate) inverse_mapping: Rc<HashMap<usize, u16>>,
}

#[cached(key = "(BitSet<N>, usize)", convert = r#"{ (bitset, _ordinal) }"#)]
fn get_allowed_rules(
    bitset: BitSet<N>,
    _ordinal: usize,
    rules: &HashMap<usize, BitSet<N>>,
) -> BitSet<N> {
    let mut allowed = BitSet::new();
    for x in bitset.iter().filter_map(|x| rules.get(&x)) {
        allowed.union(x);
    }
    allowed
}

impl Wave {
    pub(crate) fn new(width: usize, height: usize, rules: &Rules) -> Wave {
        // remap rules so they are not sparse
        let mut all_tiles = BitSet::<N>::new();
        let mut mapping = HashMap::new();
        let mut inverse_mapping = HashMap::new();

        let mut new_rules = [
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        ];

        // build up mapping tables, this should contain all keys.
        for (ordinal, _) in DIRECTIONS.iter().enumerate() {
            for &key in rules.rules[ordinal].keys() {
                if !mapping.contains_key(&key) {
                    inverse_mapping.insert(mapping.len(), key);
                    mapping.insert(key, mapping.len());
                }
            }
        }

        // convert it to dense format
        for (ordinal, _) in DIRECTIONS.iter().enumerate() {
            let old_rule = &rules.rules[ordinal];
            let new_rule = &mut new_rules[ordinal];

            let mut rule = HashMap::new(); // HashMap<usize, BitSet<N>>
            for (tile, allowed_tiles) in old_rule {
                let mut allowed_tiles_remapped = BitSet::<N>::new();
                for allowed_tile in allowed_tiles {
                    all_tiles.set(mapping[allowed_tile]);
                    allowed_tiles_remapped.set(mapping[allowed_tile]);
                }
                rule.insert(mapping[tile], allowed_tiles_remapped);
            }

            *new_rule = rule;
        }

        let mut wave = Wave {
            width: width as isize,
            height: height as isize,
            array: vec![all_tiles; width * height],
            rules: Rc::new(new_rules),
            inverse_mapping: Rc::new(inverse_mapping),
        };

        wave.constrain();

        wave
    }

    pub(crate) fn render(&self) -> Vec<u16> {
        let mut ret = Vec::new();
        for v in &self.array {
            if v.pop_cnt() > 1 || v.pop_cnt() == 0 {
                ret.push(self.inverse_mapping[&0]);
            } else {
                ret.push(self.inverse_mapping[&v.iter().next().unwrap()]);
            }
        }
        ret
    }

    pub(crate) fn is_done(&self) -> bool {
        for v in &self.array {
            if v.pop_cnt() != 1 {
                return false;
            }
        }

        true
    }

    pub(crate) fn constrain(&mut self) -> bool {
        for y in 0..self.height {
            for x in 0..self.width {
                if !self.propagate((x + y * self.width) as usize) {
                    return false;
                }
            }
        }

        true
    }

    pub(crate) fn propagate(&mut self, start: usize) -> bool {
        let mut stack = Vec::new();
        stack.push(start);

        while stack.len() > 0 {
            let chosen_index = stack.pop().unwrap();

            for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
                let (target_x, target_y) = (
                    chosen_index as isize % self.width + direction.0,
                    chosen_index as isize / self.width + direction.1,
                );

                // check if neighbor is outside the bounds.
                if target_x < 0 || target_x >= self.width || target_y < 0 || target_y >= self.height
                {
                    continue;
                }

                let target = (target_x + target_y * self.width) as usize;

                let rules = &self.rules[ordinal];

                // let mut allowed = BitSet::new();
                // for x in self.array[chosen_index]
                //     .iter()
                //     .filter_map(|x| rules.get(&x))
                // {
                //     allowed.union(x);
                // }
                let allowed = get_allowed_rules(self.array[chosen_index], ordinal, &rules);

                let old_len = self.array[target].pop_cnt();
                self.array[target].intersect(&allowed);
                let new_len = self.array[target].pop_cnt();

                if new_len == 0 {
                    // info!("ordinal: {ordinal}, chosen_index: {chosen_index}");
                    return false;
                }

                if new_len < old_len {
                    stack.push(target);
                }
            }
        }

        true
    }

    pub(crate) fn get_entropy_indices_in_order(
        &mut self,
        mut rng: &mut ThreadRng,
    ) -> Result<Vec<(usize, usize)>> {
        // TODO: use real entropy instead of number of possibilities. Requires tile weights.
        let entropies = self
            .array
            .iter()
            .map(|x| x.pop_cnt())
            .enumerate()
            .filter(|&x| x.1 > 1)
            .collect::<Vec<(usize, usize)>>();

        anyhow::ensure!(entropies.len() >= 1);

        let minimum_entropy = entropies
            .iter()
            .map(|x| x.1)
            .min()
            .ok_or(anyhow!("something"))?;
        let mut entropies: Vec<_> = entropies
            .into_iter()
            .filter(|v| v.1 <= minimum_entropy)
            .collect();

        entropies.shuffle(&mut rng);

        entropies.sort_unstable_by_key(|x| x.1);

        // info!("entropies: {entropies:?}");

        anyhow::Ok(entropies)
    }

    pub(crate) fn collapse_index(&mut self, mut rng: &mut ThreadRng, index: usize) {
        let chosen_bit = Uniform::from(0..self.array[index].pop_cnt()).sample(&mut rng);
        self.array[index].clear_all_except_nth_set_bit(chosen_bit);
    }
}
