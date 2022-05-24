use crate::{bitset::BitSet, DIRECTIONS, N};
use std::collections::{HashMap, HashSet};
use tracing::info;

#[derive(Clone, Debug)]
pub(crate) struct Rules {
    pub(crate) rules: [HashMap<usize, BitSet<N>>; 4],
}

impl Rules {
    pub(crate) fn new(
        width: isize,
        height: isize,
        tiles: &Vec<usize>,
        banned_tiles: &HashSet<usize>,
    ) -> Rules {
        let mut rules = [
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        ];

        for y in 0..height {
            for x in 0..width {
                for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
                    let target = (x + direction.0, y + direction.1);
                    // check if neighbor is outside the bounds.
                    if target.0 < 0 || target.0 >= width || target.1 < 0 || target.1 >= height {
                        continue;
                    }

                    let current_tile = tiles[(x + y * width) as usize];
                    let adjacent_tile = tiles[(target.0 + target.1 * width) as usize];

                    if banned_tiles.contains(&current_tile) || banned_tiles.contains(&adjacent_tile)
                    {
                        continue;
                    }

                    rules[ordinal]
                        .entry(current_tile)
                        .or_insert(BitSet::<N>::new())
                        .set(adjacent_tile);
                }
            }
        }

        Rules { rules }
    }
}
