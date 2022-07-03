use crate::DIRECTIONS;
use anyhow::Result;
use serde::Deserialize;
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use tracing::info;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Rules {
    pub(crate) ruleset: [HashMap<u16, HashSet<u16>>; 4],
    pub era: u16,
}

impl Rules {
    pub fn new(
        width: isize,
        height: isize,
        tiles: &Vec<u16>,
        banned_tiles: &HashSet<u16>,
        era: u16,
    ) -> Rules {
        let mut ruleset = [
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

                    ruleset[ordinal]
                        .entry(current_tile)
                        .or_insert(HashSet::new())
                        .insert(adjacent_tile);
                }
            }
        }

        Rules { ruleset, era }
    }

    pub fn combine(&self, other: &Rules) -> Result<Rules> {
        anyhow::ensure!(self.era == other.era);

        let mut new_ruleset = [
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        ];

        for (ordinal, rules) in self.ruleset.iter().enumerate() {
            let new_rules = &mut new_ruleset[ordinal];

            for (&rule, allowed_tiles) in rules {
                new_rules
                    .entry(rule)
                    .or_insert(HashSet::new())
                    .extend(allowed_tiles.iter());
            }
        }

        for (ordinal, rules) in other.ruleset.iter().enumerate() {
            let new_rules = &mut new_ruleset[ordinal];

            for (&rule, allowed_tiles) in rules {
                new_rules
                    .entry(rule)
                    .or_insert(HashSet::new())
                    .extend(allowed_tiles.iter());
            }
        }

        anyhow::Ok(Rules {
            ruleset: new_ruleset,
            era: self.era,
        })
    }

    pub fn print_rules(&self) {
        for (ordinal, direction) in ["North", "East", "South", "West"].iter().enumerate() {
            let rule = &self.ruleset[ordinal];
            info!("{direction:8} : {rule:?}");
        }
    }
}
