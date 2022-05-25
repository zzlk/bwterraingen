use crate::DIRECTIONS;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
pub(crate) struct Rules {
    pub(crate) rules: [HashMap<u16, HashSet<u16>>; 4],
}

impl Rules {
    pub(crate) fn new(
        width: isize,
        height: isize,
        tiles: &Vec<u16>,
        banned_tiles: &HashSet<u16>,
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
                    // TODO: extend the DIRECTIONS concept to allow abritary direction offsets from current tile.
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
                        .or_insert(HashSet::new())
                        .insert(adjacent_tile);
                }
            }
        }

        Rules { rules }
    }
}
