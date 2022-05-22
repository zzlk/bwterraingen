use anyhow::anyhow;
use anyhow::Result;
use rand::distributions::Uniform;
use rand::prelude::Distribution;
use rand::prelude::SliceRandom;
use rand::prelude::ThreadRng;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::env;
use tracing::error;
use tracing::field;
use tracing::info;
use tracing_log::LogTracer;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::EnvFilter;

fn setup_logging() -> Result<()> {
    // enable console_subcriber only in debug build because it consumes so much memory it breaks the server
    if cfg!(debug_assertions) {
        //console_subscriber::init();
    }

    LogTracer::init().expect("Failed to set logger");

    let filter = EnvFilter::from_default_env();
    let subscriber = tracing_subscriber::fmt()
        // filter spans/events with level TRACE or higher.
        .with_env_filter(filter)
        .with_span_events(FmtSpan::CLOSE)
        .with_file(true)
        .with_line_number(true)
        // build but do not install the subscriber.
        .finish();

    tracing::subscriber::set_global_default(subscriber)?;

    anyhow::Ok(())
}

fn main() -> Result<()> {
    setup_logging()?;

    let args: Vec<String> = env::args().collect();
    anyhow::ensure!(args.len() >= 1);

    let filename = &args[1];

    let output_width = (&args[2]).parse::<usize>()?;
    let output_height = (&args[3]).parse::<usize>()?;

    info!("filename: {filename}");

    let chk = bwmap::get_chk_from_mpq_filename(filename.clone())?;
    let raw_chunks = bwmap::parse_chk(chk.as_slice());
    let merged_chunks = bwmap::merge_raw_chunks(raw_chunks.as_slice());
    let parsed_chunks = bwmap::parse_merged_chunks(&merged_chunks)?;

    let dim = {
        if let bwmap::ParsedChunk::DIM(r) = parsed_chunks
            .get(&bwmap::ChunkName::DIM)
            .ok_or(anyhow!("could't get dim section"))?
        {
            r
        } else {
            anyhow::bail!("couldn't do something with dim...")
        }
    };

    let era = {
        if let bwmap::ParsedChunk::ERA(r) = parsed_chunks
            .get(&bwmap::ChunkName::ERA)
            .ok_or(anyhow!("could't get era section"))?
        {
            r
        } else {
            anyhow::bail!("couldn't do something with era...")
        }
    };

    let mtxm = {
        if let bwmap::ParsedChunk::MTXM(r) = parsed_chunks
            .get(&bwmap::ChunkName::MTXM)
            .ok_or(anyhow!("could't get mtxm section"))?
        {
            r
        } else {
            anyhow::bail!("couldn't do something with mtxm...")
        }
    };

    let width = *dim.width as usize;
    let height = *dim.height as usize;

    let mut north_rules = std::collections::HashMap::<u16, HashSet<u16>>::new();
    let mut south_rules = std::collections::HashMap::<u16, HashSet<u16>>::new();
    let mut east_rules = std::collections::HashMap::<u16, HashSet<u16>>::new();
    let mut west_rules = std::collections::HashMap::<u16, HashSet<u16>>::new();

    print_map(&mtxm.data, width, height);

    for x in 0..width {
        for y in 0..height {
            let current_tile = mtxm.data[x + y * width];
            if current_tile < 16 {
                continue;
            }

            if y < height - 1 {
                let adjacent = mtxm.data[x + (y + 1) * width];
                if adjacent >= 16 {
                    north_rules
                        .entry(current_tile)
                        .or_insert(HashSet::new())
                        .insert(adjacent);
                }
            }

            if y > 0 {
                let adjacent = mtxm.data[x + (y - 1) * width];
                if adjacent >= 16 {
                    south_rules
                        .entry(current_tile)
                        .or_insert(HashSet::new())
                        .insert(adjacent);
                }
            }

            if x > 0 {
                let adjacent = mtxm.data[x - 1 + y * width];
                if adjacent >= 16 {
                    west_rules
                        .entry(current_tile)
                        .or_insert(HashSet::new())
                        .insert(adjacent);
                }
            }

            if x < width - 1 {
                let adjacent = mtxm.data[x + 1 + y * width];
                if adjacent >= 16 {
                    east_rules
                        .entry(current_tile)
                        .or_insert(HashSet::new())
                        .insert(adjacent);
                }
            }
        }
    }

    info!(
        "rule-lengths: {}, {}, {}, {}",
        north_rules.len(),
        east_rules.len(),
        south_rules.len(),
        west_rules.len()
    );

    let map = gen_map(
        (&north_rules, &east_rules, &south_rules, &west_rules),
        output_width,
        output_height,
    )?;

    print_map(&map, output_width, output_height);

    let mut bytes = include_bytes!("template.chk").to_vec();

    bytes.extend_from_slice(b"MASK");
    bytes.extend_from_slice(
        ((output_width * output_height) as u32)
            .to_le_bytes()
            .as_slice(),
    );
    for _ in 0..(output_width * output_height) {
        bytes.extend_from_slice(0u8.to_le_bytes().as_slice());
    }

    bytes.extend_from_slice(b"ERA ");
    bytes.extend_from_slice(2u32.to_le_bytes().as_slice());
    bytes.extend_from_slice((*era.tileset as u16).to_le_bytes().as_slice());

    bytes.extend_from_slice(b"DIM ");
    bytes.extend_from_slice(4u32.to_le_bytes().as_slice());
    bytes.extend_from_slice((output_width as u16).to_le_bytes().as_slice());
    bytes.extend_from_slice((output_height as u16).to_le_bytes().as_slice());

    bytes.extend_from_slice(b"MTXM");
    bytes.extend_from_slice(((map.len() * 2) as u32).to_le_bytes().as_slice());
    for word in map.iter() {
        bytes.extend_from_slice((*word as u16).to_le_bytes().as_slice());
    }

    bytes.extend_from_slice(b"TILE");
    bytes.extend_from_slice(((map.len() * 2) as u32).to_le_bytes().as_slice());
    for word in map.iter() {
        bytes.extend_from_slice((*word as u16).to_le_bytes().as_slice());
    }

    std::fs::write("/home/stan/Downloads/out.chk", bytes)?;

    anyhow::Ok(())
}

fn print_map(map: &Vec<u16>, width: usize, height: usize) {
    // 16 is creep, 64 is high dirt. 96 is water. 128 is yellow-grass
    info!("map:");
    for y in 0..height {
        let mut output = String::new();
        for x in 0..width {
            output = format!("{}{:5}", output, map[x + width * y]);
        }
        info!("        {}", output);
    }
}

fn print_wave(wave: &Vec<HashSet<u16>>, width: usize, height: usize) {
    info!("wave:");
    for y in 0..height {
        let mut output = String::new();
        for x in 0..width {
            output = format!("{}{:5}", output, wave[x + width * y].len());
        }
        info!("        {}", output);
    }
}

#[inline(never)]
fn gen_map(
    rules: (
        &HashMap<u16, HashSet<u16>>,
        &HashMap<u16, HashSet<u16>>,
        &HashMap<u16, HashSet<u16>>,
        &HashMap<u16, HashSet<u16>>,
    ),
    width: usize,
    height: usize,
) -> Result<Vec<u16>> {
    let mut rng = rand::thread_rng();

    let mut all_possible_tiles = HashSet::new();

    all_possible_tiles = all_possible_tiles
        .union(&rules.0.keys().copied().collect::<HashSet<u16>>())
        .copied()
        .collect();
    all_possible_tiles = all_possible_tiles
        .union(&rules.1.keys().copied().collect::<HashSet<u16>>())
        .copied()
        .collect();
    all_possible_tiles = all_possible_tiles
        .union(&rules.2.keys().copied().collect::<HashSet<u16>>())
        .copied()
        .collect();
    all_possible_tiles = all_possible_tiles
        .union(&rules.3.keys().copied().collect::<HashSet<u16>>())
        .copied()
        .collect();

    let mut wave = vec![all_possible_tiles; width * height];

    let mut i = 0;
    loop {
        info!("iteration: {i}");
        i += 1;
        wave = match collapse_one(wave.clone(), width, height, &mut rng, rules)? {
            CollapseResult::Next(wave) => wave,
            CollapseResult::Contradiction => {
                error!("Contradiction");
                wave
            }
            CollapseResult::Complete(wave) => {
                info!("Chosen map:");
                print_wave(&wave, width, height);

                // finally collapse wave out of hashset into just regular thingy
                let ret: Vec<_> = wave
                    .into_iter()
                    .map(|x| x.into_iter().next().unwrap())
                    .collect();

                return anyhow::Ok(ret);
            }
        };
    }
}

enum CollapseResult {
    Next(Vec<HashSet<u16>>),
    Contradiction,
    Complete(Vec<HashSet<u16>>),
}

#[inline(never)]
fn remove_all_except_one_from_set(mut rng: &mut ThreadRng, set: &HashSet<u16>) -> HashSet<u16> {
    let vec = Vec::from_iter(set.iter().copied());
    let mut ret = HashSet::new();

    ret.insert(*vec.choose(&mut rng).unwrap());

    ret
}

#[inline(never)]
fn collapse_one(
    mut wave: Vec<HashSet<u16>>,
    width: usize,
    height: usize,
    mut rng: &mut ThreadRng,
    rules: (
        &HashMap<u16, HashSet<u16>>,
        &HashMap<u16, HashSet<u16>>,
        &HashMap<u16, HashSet<u16>>,
        &HashMap<u16, HashSet<u16>>,
    ),
) -> Result<CollapseResult> {
    // print_wave(&wave, width, height);

    // find lowest entropy
    let entropies = wave
        .iter()
        .map(|x| x.len())
        .enumerate()
        .filter(|&x| x.1 > 1)
        .collect::<Vec<(usize, usize)>>();

    if entropies.len() == 0 {
        return anyhow::Ok(CollapseResult::Complete(wave));
    }

    // info!("entropies.len(): {}", entropies.len());
    // info!("entropies: {entropies:?}");

    let minimum_entropy = entropies
        .iter()
        .map(|x| x.1)
        .min()
        .ok_or(anyhow!("something"))?;
    let minimum_entropies: Vec<_> = entropies
        .into_iter()
        .filter(|v| v.1 <= minimum_entropy)
        .map(|x| x.0)
        .collect();

    // info!("minimum_entropies: {:?}", minimum_entropies);
    anyhow::ensure!(minimum_entropies.len() > 0);

    let chosen_index =
        minimum_entropies[Uniform::from(0..minimum_entropies.len()).sample(&mut rng)];

    // collapse random tile to something that is possible
    wave[chosen_index] = remove_all_except_one_from_set(&mut rng, &wave[chosen_index]);

    // propagate collapse
    let mut queue = VecDeque::<usize>::new();

    queue.push_back(chosen_index);

    let span = tracing::trace_span!(
        "while-loop",
        iterations = field::Empty,
        west_iterations = field::Empty
    );
    let _enter = span.enter();

    let mut iterations = 0;
    let mut west_iterations = 0;
    while queue.len() > 0 {
        iterations += 1;
        // let span = tracing::trace_span!("queue-single");
        // let _enter = span.enter();

        // info!("{}", queue.len());
        let chosen_index = queue.pop_front().unwrap();
        // info!("chosen_index: {chosen_index}");
        // info!("wave[chosen_index]: {:?}", wave[chosen_index]);

        // west
        if chosen_index % width > 0 {
            west_iterations += 1;

            // let span = tracing::trace_span!("west");
            // let _enter = span.enter();

            let target = chosen_index - 1;
            let rules = rules.3; // NESW

            let mut allowed = HashSet::new();
            for x in wave[chosen_index].iter().filter_map(|x| rules.get(x)) {
                allowed.extend(x);
            }

            let new: HashSet<u16> = wave[target].intersection(&allowed).cloned().collect(); // intersection not difference
            let new_len = new.len();
            if new_len == 0 {
                return anyhow::Ok(CollapseResult::Contradiction);
            }

            let old_len = wave[target].len();

            wave[target] = new;

            if new_len < old_len {
                queue.push_back(target);
            }
        }

        // east
        if chosen_index % width < width - 1 {
            let target = chosen_index + 1;
            let rules = rules.1; // NESW

            let mut allowed = HashSet::new();
            for x in wave[chosen_index].iter().filter_map(|x| rules.get(x)) {
                allowed.extend(x);
            }

            let new: HashSet<_> = wave[target].intersection(&allowed).cloned().collect();
            let new_len = new.len();
            if new_len == 0 {
                return anyhow::Ok(CollapseResult::Contradiction);
            }

            let old_len = wave[target].len();

            wave[target] = new;

            if new_len < old_len {
                queue.push_back(target);
            }
        }

        // south
        if chosen_index / width > 0 {
            let target = chosen_index - width;
            let rules = rules.2; // NESW

            let mut allowed = HashSet::new();
            for x in wave[chosen_index].iter().filter_map(|x| rules.get(x)) {
                allowed.extend(x);
            }

            let new: HashSet<u16> = wave[target].intersection(&allowed).cloned().collect();
            let new_len = new.len();
            if new_len == 0 {
                return anyhow::Ok(CollapseResult::Contradiction);
            }

            let old_len = wave[target].len();

            wave[target] = new;

            if new_len < old_len {
                queue.push_back(target);
            }
        }

        // north
        if chosen_index / width < height - 1 {
            let target = chosen_index + width;
            let rules = rules.0; // NESW

            let mut allowed = HashSet::new();
            for x in wave[chosen_index].iter().filter_map(|x| rules.get(x)) {
                allowed.extend(x);
            }

            let new: HashSet<u16> = wave[target].intersection(&allowed).cloned().collect();
            let new_len = new.len();
            if new_len == 0 {
                return anyhow::Ok(CollapseResult::Contradiction);
            }

            let old_len = wave[target].len();

            wave[target] = new;

            if new_len < old_len {
                queue.push_back(target);
            }
        }
    }

    span.record("iterations", &iterations);
    span.record("west_iterations", &west_iterations);

    anyhow::Ok(CollapseResult::Next(wave))
}

const DIRECTIONS: [(isize, isize); 4] = [(0, -1), (1, 0), (0, -1), (-1, 0)];

const N: usize = 128;

struct Rules {
    rules: [HashMap<usize, BitSet<N>>; 4],
}

impl Rules {
    fn new(width: isize, height: isize, tiles: Vec<usize>) -> Rules {
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
                    if target.0 < 0 || target.0 > width || target.1 < 0 || target.1 > height {
                        continue;
                    }

                    let current_tile = tiles[(x + y * width) as usize];
                    let adjacent_tile = tiles[(target.0 + target.1 * width) as usize];

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

struct Wave<T: Copy + std::hash::Hash + Eq> {
    width: isize,
    height: isize,
    mapping: HashMap<T, usize>,
    array: Vec<BitSet<N>>,
}

impl<T: Copy + std::hash::Hash + Eq> Wave<T> {
    fn new(width: usize, height: usize, all_tiles: &Vec<T>) -> Wave<T> {
        let mut mapping = HashMap::new();
        let mut bitset = BitSet::<N>::new();
        for v in all_tiles {
            if let None = mapping.get(v) {
                bitset.set(mapping.len());
                mapping.insert(*v, mapping.len());
            }
        }

        Wave {
            width: width as isize,
            height: height as isize,
            mapping: mapping,
            array: vec![bitset; width * height],
        }
    }

    fn propagate(&mut self, rules: &Rules, start: (isize, isize)) -> bool {
        let mut stack = Vec::new();
        stack.push(start);

        while stack.len() > 0 {
            let chosen_index = stack.pop().unwrap();

            for (ordinal, direction) in DIRECTIONS.iter().enumerate() {
                let target = (chosen_index.0 + direction.0, chosen_index.1 + direction.1);

                // check if neighbor is outside the bounds.
                if target.0 < 0 || target.0 > self.width || target.1 < 0 || target.1 > self.height {
                    continue;
                }

                let rules = &rules.rules[ordinal];

                let mut allowed = BitSet::new();
                for x in self.array[(chosen_index.0 + chosen_index.1 * self.height) as usize]
                    .iter()
                    .filter_map(|x| rules.get(&x))
                {
                    allowed.union(x);
                }

                let old_len = self.array[(target.0 + target.1 * self.width) as usize].pop_cnt();
                self.array[(target.0 + target.1 * self.width) as usize].intersect(&allowed); // intersection not difference
                let new_len = self.array[(target.0 + target.1 * self.width) as usize].pop_cnt();

                if new_len == 0 {
                    return false;
                }

                if new_len < old_len {
                    stack.push(target);
                }
            }
        }
        true
    }

    fn find_and_collapse_random_lowest_entropy_indices(
        &mut self,
        mut rng: &mut ThreadRng,
    ) -> Result<()> {
        // TODO: use real entropy instead of number of possibilities. Requires tile weights.
        let entropies = self
            .array
            .iter()
            .map(|x| x.pop_cnt())
            .enumerate()
            .filter(|&x| x.1 > 1)
            .collect::<Vec<(usize, usize)>>();

        anyhow::ensure!(entropies.len() > 1);

        let minimum_entropy = entropies
            .iter()
            .map(|x| x.1)
            .min()
            .ok_or(anyhow!("something"))?;
        let minimum_entropies: Vec<_> = entropies
            .into_iter()
            .filter(|v| v.1 <= minimum_entropy)
            .map(|x| x.0)
            .collect();

        anyhow::ensure!(minimum_entropies.len() > 0);

        let chosen_index =
            minimum_entropies[Uniform::from(0..minimum_entropies.len()).sample(&mut rng)];

        let chosen_bit = Uniform::from(0..self.array[chosen_index].pop_cnt()).sample(&mut rng);

        // collapse random tile to something that is possible
        self.array[chosen_index].clear_all_except_nth_set_bit(chosen_bit);

        anyhow::Ok(())
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct BitSet<const N: usize> {
    bits: [usize; N],
}

impl<const N: usize> BitSet<N> {
    fn new() -> BitSet<N> {
        BitSet { bits: [0; N] }
    }

    fn set(&mut self, offset: usize) {
        self.bits[offset / std::mem::size_of::<usize>()] |=
            1 << offset % std::mem::size_of::<usize>();
    }

    fn reset(&mut self, offset: usize) {
        self.bits[offset / std::mem::size_of::<usize>()] &=
            !(1 << offset % std::mem::size_of::<usize>());
    }

    fn pop_cnt(&self) -> usize {
        let mut count = 0;
        for i in 0..self.bits.len() {
            count += self.bits[i].count_ones();
        }

        count as usize
    }

    fn intersect(&mut self, other: &BitSet<N>) {
        for i in 0..self.bits.len() {
            self.bits[i] &= other.bits[i];
        }
    }

    fn union(&mut self, other: &BitSet<N>) {
        for i in 0..self.bits.len() {
            self.bits[i] |= other.bits[i];
        }
    }

    fn clear_all_except_nth_set_bit(&mut self, mut nth_bit: usize) {
        let mut index = 0;
        loop {
            if self.bits[index].count_ones() as usize > nth_bit {
                nth_bit -= self.bits[index].count_ones() as usize;
                break;
            }
            index += 1;
        }

        for i in 0..std::mem::size_of::<usize>() * 8 {
            if self.bits[index] & (1 << i) == (1 << i) {
                nth_bit -= 1;
            }

            if nth_bit == 0 {
                index = index * std::mem::size_of::<usize>() * 8 + i;
                break;
            }
        }

        let new = [0; N];
        self.bits = new;

        self.set(index);
    }

    fn iter(&self) -> BitSetIterator<N> {
        self.into_iter()
    }
}

impl<'a, const N: usize> IntoIterator for &'a BitSet<N> {
    type Item = usize;
    type IntoIter = BitSetIterator<'a, N>;

    fn into_iter(self) -> Self::IntoIter {
        BitSetIterator {
            bitset: self,
            index: 0,
        }
    }
}

pub struct BitSetIterator<'a, const N: usize> {
    bitset: &'a BitSet<N>,
    index: usize,
}

impl<'a, const N: usize> Iterator for BitSetIterator<'a, N> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        for word_index in self.index / 8..self.bitset.bits.len() {
            for i in
                self.index % (std::mem::size_of::<usize>() * 8)..std::mem::size_of::<usize>() * 8
            {
                if self.bitset.bits[word_index] & (1 << i) == (1 << i) {
                    return Some(word_index * 8 + i);
                }
            }
        }

        None
    }
}

#[cfg(test)]
mod test {
    use super::BitSet;

    #[test]
    fn test_bitset() {
        let mut x = BitSet::<1>::new();
        let mut y = BitSet::<1>::new();

        x.set(0);
        y.set(0);

        assert_eq!(x, y);
    }
}
