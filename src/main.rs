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

mod bitset;
mod rules;
mod wave;

use bitset::BitSet;
use rules::Rules;
use std::time::Instant;
use tracing::instrument;
use wave::Wave;

const DIRECTIONS: [(isize, isize); 4] = [(0, -1), (1, 0), (0, 1), (-1, 0)];

const N: usize = 48;

fn print_map(map: &Vec<usize>, width: isize, height: isize) {
    // 16 is creep, 64 is high dirt. 96 is water. 128 is yellow-grass
    info!("map:");
    for y in 0..height {
        let mut output = String::new();
        for x in 0..width {
            output = format!("{}{:5}", output, map[(x + y * width) as usize]);
        }
        info!("        {}", output);
    }
}

fn print_wave(wave: &Wave) {
    info!("wave:");
    for y in 0..wave.height {
        let mut output = String::new();
        for x in 0..wave.width {
            output = format!(
                "{}{:4x}",
                output,
                wave.array[(x + y * wave.width) as usize].pop_cnt()
            );
        }
        info!("        {}", output);
    }
}

fn print_rules(rules: &Rules) {
    for (ordinal, direction) in ["North", "East", "South", "West"].iter().enumerate() {
        let rule = &rules.rules[ordinal];
        info!("{direction:8} : {rule:?}");
    }
}

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
        .with_target(false)
        .with_line_number(true)
        .with_level(false)
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

    let width = *dim.width as isize;
    let height = *dim.height as isize;

    // remap the tiles to be dense
    let mut mapping = HashMap::new();
    let mut inverse_mapping = HashMap::new();
    let mut new_map = Vec::new();

    let mut banned_tiles = HashSet::new();

    mapping.insert(&1, 0);

    for tile in &mtxm.data {
        if !mapping.contains_key(&tile) {
            if *tile < 16 {
                banned_tiles.insert(mapping.len());
            }
            mapping.insert(tile, mapping.len());
        }

        new_map.push(mapping.get(&tile).unwrap().clone());
    }

    for m in mapping {
        inverse_mapping.insert(m.1, m.0);
    }

    let rules = Rules::new(width, height, &new_map, &banned_tiles);

    //print_rules(&rules);
    //print_map(&new_map, width, height);

    let map = gen_map(&rules, output_width as isize, output_height as isize)?;

    let map: Vec<u16> = map.into_iter().map(|x| *inverse_mapping[&x]).collect();
    //print_map_real(&map, output_width as isize, output_height as isize);

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

#[inline(never)]
fn gen_map(rules: &Rules, width: isize, height: isize) -> Result<Vec<usize>> {
    let mut rng = rand::thread_rng();

    let mut all_possible_tiles = BitSet::new();

    for rule in &rules.rules {
        for (tile, _allowed) in rule {
            all_possible_tiles.set(*tile);
        }
    }

    let mut wave = Wave::new(width as usize, height as usize, &all_possible_tiles);

    wave.constrain(rules);
    print_wave(&wave);

    let wave = loop {
        let mut fail_count = 0;

        let mut wave = wave.clone();
        // let index = wave.collapse_random(&mut rng);
        // wave.propagate(rules, index);
        // wave
        wave.constrain(&rules);
        print_wave(&wave);

        info!("RESTART");

        if let Ok(wave) = process_wave(
            wave,
            rules,
            &mut rng,
            0,
            &mut fail_count,
            &mut Instant::now(),
        ) {
            break wave;
        } else {
            continue;
        };
    };

    anyhow::Ok(wave.render())
}

fn process_wave(
    mut wave: Wave,
    rules: &Rules,
    mut rng: &mut ThreadRng,
    depth: usize,
    fail_count: &mut usize,
    last_time: &mut Instant,
) -> Result<Wave> {
    // info!("depth: {depth:5}, fail_count: {fail_count:5}, BEGIN");

    if Instant::now().duration_since(*last_time).as_millis() > 100 {
        *last_time = Instant::now();
        info!("depth: {depth:5}, fail_count: {fail_count:6}",);
        print_wave(&wave);
    }

    if wave.is_done() {
        return anyhow::Ok(wave);
    }

    let indices = wave.get_entropy_indices_in_order(&mut rng)?;
    // info!("depth: {depth:5}, fail_count: {fail_count:5}, indices: {indices:?}");

    for (index, pop_cnt) in &indices {
        // for i in 0..*pop_cnt {
        if *fail_count > 5000 {
            return anyhow::Ok(wave);
        }

        let mut wave = wave.clone();

        wave.collapse_index(&mut rng, *index);
        //wave.array[*index].clear_all_except_nth_set_bit(i);
        // info!(
        //     "depth: {depth:5}, fail_count: {fail_count:5} collapse_index: {}",
        //     *index
        // );
        // print_wave(&wave);

        if !wave.propagate(&rules, *index) {
            // *fail_count += 1;

            // if *fail_count > 20000 {
            //     //return anyhow::Result::Err(anyhow!("fail_count too high"));
            //     return anyhow::Ok(wave);
            // }

            continue;
        }

        if let Ok(ret) = process_wave(wave.clone(), rules, rng, depth + 1, fail_count, last_time) {
            return anyhow::Ok(ret);
        } else {
            *fail_count += 1;
        }
        // }
    }

    anyhow::Result::Err(anyhow!("Couldn't quite get there"))
}

// #[inline(never)]
// fn collapse_one(
//     mut wave: Vec<HashSet<u16>>,
//     width: usize,
//     height: usize,
//     mut rng: &mut ThreadRng,
//     rules: (
//         &HashMap<u16, HashSet<u16>>,
//         &HashMap<u16, HashSet<u16>>,
//         &HashMap<u16, HashSet<u16>>,
//         &HashMap<u16, HashSet<u16>>,
//     ),
// ) -> Result<CollapseResult> {
//     // print_wave(&wave, width, height);

//     // find lowest entropy
//     let entropies = wave
//         .iter()
//         .map(|x| x.len())
//         .enumerate()
//         .filter(|&x| x.1 > 1)
//         .collect::<Vec<(usize, usize)>>();

//     if entropies.len() == 0 {
//         return anyhow::Ok(CollapseResult::Complete(wave));
//     }

//     // info!("entropies.len(): {}", entropies.len());
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

//     // info!("minimum_entropies: {:?}", minimum_entropies);
//     anyhow::ensure!(minimum_entropies.len() > 0);

//     let chosen_index =
//         minimum_entropies[Uniform::from(0..minimum_entropies.len()).sample(&mut rng)];

//     // collapse random tile to something that is possible
//     wave[chosen_index] = remove_all_except_one_from_set(&mut rng, &wave[chosen_index]);

//     // propagate collapse
//     let mut queue = VecDeque::<usize>::new();

//     queue.push_back(chosen_index);

//     let span = tracing::trace_span!(
//         "while-loop",
//         iterations = field::Empty,
//         west_iterations = field::Empty
//     );
//     let _enter = span.enter();

//     let mut iterations = 0;
//     let mut west_iterations = 0;
//     while queue.len() > 0 {
//         iterations += 1;
//         // let span = tracing::trace_span!("queue-single");
//         // let _enter = span.enter();

//         // info!("{}", queue.len());
//         let chosen_index = queue.pop_front().unwrap();
//         // info!("chosen_index: {chosen_index}");
//         // info!("wave[chosen_index]: {:?}", wave[chosen_index]);

//         // west
//         if chosen_index % width > 0 {
//             west_iterations += 1;

//             // let span = tracing::trace_span!("west");
//             // let _enter = span.enter();

//             let target = chosen_index - 1;
//             let rules = rules.3; // NESW

//             let mut allowed = HashSet::new();
//             for x in wave[chosen_index].iter().filter_map(|x| rules.get(x)) {
//                 allowed.extend(x);
//             }

//             let new: HashSet<u16> = wave[target].intersection(&allowed).cloned().collect(); // intersection not difference
//             let new_len = new.len();
//             if new_len == 0 {
//                 return anyhow::Ok(CollapseResult::Contradiction);
//             }

//             let old_len = wave[target].len();

//             wave[target] = new;

//             if new_len < old_len {
//                 queue.push_back(target);
//             }
//         }

//         // east
//         if chosen_index % width < width - 1 {
//             let target = chosen_index + 1;
//             let rules = rules.1; // NESW

//             let mut allowed = HashSet::new();
//             for x in wave[chosen_index].iter().filter_map(|x| rules.get(x)) {
//                 allowed.extend(x);
//             }

//             let new: HashSet<_> = wave[target].intersection(&allowed).cloned().collect();
//             let new_len = new.len();
//             if new_len == 0 {
//                 return anyhow::Ok(CollapseResult::Contradiction);
//             }

//             let old_len = wave[target].len();

//             wave[target] = new;

//             if new_len < old_len {
//                 queue.push_back(target);
//             }
//         }

//         // south
//         if chosen_index / width > 0 {
//             let target = chosen_index - width;
//             let rules = rules.2; // NESW

//             let mut allowed = HashSet::new();
//             for x in wave[chosen_index].iter().filter_map(|x| rules.get(x)) {
//                 allowed.extend(x);
//             }

//             let new: HashSet<u16> = wave[target].intersection(&allowed).cloned().collect();
//             let new_len = new.len();
//             if new_len == 0 {
//                 return anyhow::Ok(CollapseResult::Contradiction);
//             }

//             let old_len = wave[target].len();

//             wave[target] = new;

//             if new_len < old_len {
//                 queue.push_back(target);
//             }
//         }

//         // north
//         if chosen_index / width < height - 1 {
//             let target = chosen_index + width;
//             let rules = rules.0; // NESW

//             let mut allowed = HashSet::new();
//             for x in wave[chosen_index].iter().filter_map(|x| rules.get(x)) {
//                 allowed.extend(x);
//             }

//             let new: HashSet<u16> = wave[target].intersection(&allowed).cloned().collect();
//             let new_len = new.len();
//             if new_len == 0 {
//                 return anyhow::Ok(CollapseResult::Contradiction);
//             }

//             let old_len = wave[target].len();

//             wave[target] = new;

//             if new_len < old_len {
//                 queue.push_back(target);
//             }
//         }
//     }

//     span.record("iterations", &iterations);
//     span.record("west_iterations", &west_iterations);

//     anyhow::Ok(CollapseResult::Next(wave))
// }

// enum CollapseResult {
//     Next(Vec<HashSet<u16>>),
//     Contradiction,
//     Complete(Vec<HashSet<u16>>),
// }

// #[inline(never)]
// fn remove_all_except_one_from_set(mut rng: &mut ThreadRng, set: &HashSet<u16>) -> HashSet<u16> {
//     let vec = Vec::from_iter(set.iter().copied());
//     let mut ret = HashSet::new();

//     ret.insert(*vec.choose(&mut rng).unwrap());

//     ret
// }
