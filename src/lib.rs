mod bitset;
mod chk;
mod rules;
mod wave;

pub(crate) const DIRECTIONS: [(isize, isize); 4] = [(0, -1), (1, 0), (0, 1), (-1, 0)];
const N: usize = 5000 / (std::mem::size_of::<usize>() * 8);

pub use chk::create_chk_from_wave;
pub use chk::get_dim_from_chk;
pub use chk::get_list_of_unique_tiles_from_chk;
pub use chk::get_rules_from_chk;
pub use rules::Rules;
pub use wave::Wave;
