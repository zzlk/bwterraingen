use crate::{rules::Rules, BitSet};
use crate::{DIRECTIONS, N};
use anyhow::anyhow;
use anyhow::Result;
use cached::proc_macro::cached;
use rand::distributions::Uniform;
use rand::prelude::ThreadRng;
use rand::prelude::{Distribution, SliceRandom};
use std::collections::HashMap;
use tracing::info;

#[derive(Debug, Clone)]
pub(crate) struct Wave {
    pub(crate) width: isize,
    pub(crate) height: isize,
    pub(crate) array: Vec<BitSet<N>>,
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
    pub(crate) fn new(width: usize, height: usize, all_tiles: &BitSet<N>) -> Wave {
        Wave {
            width: width as isize,
            height: height as isize,
            array: vec![all_tiles.clone(); width * height],
        }
    }

    pub(crate) fn render(&self) -> Vec<usize> {
        let mut ret = Vec::new();
        for v in &self.array {
            if v.pop_cnt() > 1 || v.pop_cnt() == 0 {
                ret.push(0);
            } else {
                ret.push(v.iter().next().unwrap());
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

    pub(crate) fn constrain(&mut self, rules: &Rules) -> bool {
        for y in 0..self.height {
            for x in 0..self.width {
                if !self.propagate(rules, (x + y * self.width) as usize) {
                    return false;
                }
            }
        }

        true
    }

    pub(crate) fn propagate(&mut self, rules: &Rules, start: usize) -> bool {
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

                let rules = &rules.rules[ordinal];

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
        let mut entropies = self
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

    pub(crate) fn collapse_random(&mut self, mut rng: &mut ThreadRng) -> usize {
        let index = Uniform::from(0..self.array.len()).sample(&mut rng);
        self.collapse_index(rng, index);
        index
    }

    // pub(crate) fn find_and_collapse_random_lowest_entropy_indices(
    //     &mut self,
    //     mut rng: &mut ThreadRng,
    // ) -> Result<()> {
    //     // TODO: use real entropy instead of number of possibilities. Requires tile weights.
    //     let entropies = self
    //         .array
    //         .iter()
    //         .map(|x| x.pop_cnt())
    //         .enumerate()
    //         .filter(|&x| x.1 > 1)
    //         .collect::<Vec<(usize, usize)>>();

    //     anyhow::ensure!(entropies.len() >= 1);
    //     // info!("entropies: {entropies:?}");

    //     let minimum_entropy = entropies
    //         .iter()
    //         .map(|x| x.1)
    //         .min()
    //         .ok_or(anyhow!("something"))?;
    //     let minimum_entropies: Vec<_> = entropies
    //         .into_iter()
    //         .filter(|v| v.1 <= minimum_entropy)
    //         .map(|x| x.0)
    //         .collect();

    //     anyhow::ensure!(minimum_entropies.len() > 0);
    //     // info!("minimum_entropy_indices: {minimum_entropies:?}");

    //     let chosen_index =
    //         minimum_entropies[Uniform::from(0..minimum_entropies.len()).sample(&mut rng)];
    //     // info!("chosen_index: {chosen_index:?}");

    //     let chosen_bit = Uniform::from(0..self.array[chosen_index].pop_cnt()).sample(&mut rng);
    //     // info!("chosen_bit: {chosen_bit:?}");

    //     // collapse random tile to something that is possible
    //     self.array[chosen_index].clear_all_except_nth_set_bit(chosen_bit);

    //     anyhow::Ok(())
    // }
}
