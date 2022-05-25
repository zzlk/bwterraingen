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

fn get_rules_from_chk(chk: &Vec<u8>) -> Result<Rules> {
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

    let mut banned_tiles = HashSet::new();
    for i in 0..16 {
        banned_tiles.insert(i);
    }

    anyhow::Ok(Rules::new(
        *dim.width as isize,
        *dim.height as isize,
        &mtxm.data,
        &banned_tiles,
    ))
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

    let rules = get_rules_from_chk(&chk)?;

    let map = gen_map(&rules, output_width as isize, output_height as isize)?;

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

fn gen_map(rules: &Rules, width: isize, height: isize) -> Result<Vec<u16>> {
    let mut rng = rand::thread_rng();

    let wave = Wave::new(width as usize, height as usize, rules);

    let wave = loop {
        let mut fail_count = 0;

        info!("RESTART");

        if let Ok(wave) = process_wave(
            wave.clone(),
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
    if Instant::now().duration_since(*last_time).as_millis() > 100 {
        *last_time = Instant::now();
        info!("depth: {depth:5}, fail_count: {fail_count:6}",);
        print_wave(&wave);
    }

    if wave.is_done() {
        return anyhow::Ok(wave);
    }

    let indices = wave.get_entropy_indices_in_order(&mut rng)?;

    for (index, _pop_cnt) in &indices {
        if *fail_count > 5000 {
            return anyhow::Ok(wave);
        }

        let mut wave = wave.clone();

        wave.collapse_index(&mut rng, *index);

        if !wave.propagate(*index) {
            continue;
        }

        if let Ok(ret) = process_wave(wave.clone(), rules, rng, depth + 1, fail_count, last_time) {
            return anyhow::Ok(ret);
        } else {
            *fail_count += 1;
        }
    }

    anyhow::Result::Err(anyhow!("Couldn't quite get there"))
}
