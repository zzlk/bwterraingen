mod bitset;
mod chk;
mod render;
mod rules;
mod wasm;
mod wave;

pub(crate) const DIRECTIONS: [(isize, isize); 4] = [(0, -1), (1, 0), (0, 1), (-1, 0)];
const N: usize = 96;

pub use chk::create_chk_from_wave;
pub use chk::get_rules_from_chk;
pub use rules::Rules;
pub use wave::process_wave;
pub use wave::Wave;

pub use wasm::do_map;
pub use wasm::get_rules;
pub use wasm::main;
