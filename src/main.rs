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

fn remove_all_except_one_from_set(mut rng: &mut ThreadRng, set: &HashSet<u16>) -> HashSet<u16> {
    let vec = Vec::from_iter(set.iter().copied());
    let mut ret = HashSet::new();

    ret.insert(*vec.choose(&mut rng).unwrap());

    ret
}

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

    while queue.len() > 0 {
        info!("{}", queue.len());
        let chosen_index = queue.pop_front().unwrap();
        // info!("chosen_index: {chosen_index}");
        // info!("wave[chosen_index]: {:?}", wave[chosen_index]);

        // west
        if chosen_index % width > 0 {
            let target = chosen_index - 1;
            let west_rules = rules.3; // NESW

            let allowed = wave[chosen_index]
                .iter()
                .filter_map(|x| west_rules.get(x))
                .fold(HashSet::new(), |a, b| a.union(&b).cloned().collect());

            let old_len = wave[target].len();

            let new: HashSet<u16> = wave[target].intersection(&allowed).cloned().collect(); // intersection not difference
            let new_len = new.len();
            if new_len == 0 {
                return anyhow::Ok(CollapseResult::Contradiction);
            }

            wave[target] = new;

            if new_len < old_len {
                queue.push_back(target);
            }
        }

        // east
        if chosen_index % width < width - 1 {
            let target = chosen_index + 1;
            let east_rules = rules.1; // NESW

            let allowed = wave[chosen_index]
                .iter()
                .filter_map(|x| east_rules.get(x))
                .fold(HashSet::new(), |a, b| a.union(&b).cloned().collect());

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

        // south
        if chosen_index / width > 0 {
            let target = chosen_index - width;
            let west_rules = rules.2; // NESW

            let allowed = wave[chosen_index]
                .iter()
                .filter_map(|x| west_rules.get(x))
                .fold(HashSet::new(), |a, b| a.union(&b).cloned().collect());

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
            let west_rules = rules.0; // NESW

            let allowed = wave[chosen_index]
                .iter()
                .filter_map(|x| west_rules.get(x))
                .fold(HashSet::new(), |a, b| a.union(&b).cloned().collect());

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

    anyhow::Ok(CollapseResult::Next(wave))
}
