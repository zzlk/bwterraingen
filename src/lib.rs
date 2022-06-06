mod bitset;
mod chk;
mod render;
mod rules;
mod wave;

pub(crate) const DIRECTIONS: [(isize, isize); 4] = [(0, -1), (1, 0), (0, 1), (-1, 0)];
const N: usize = 96;

pub use chk::create_chk_from_wave;
pub use chk::get_rules_from_chk;
pub use rules::Rules;
pub use wave::Wave;

pub use render::render;
