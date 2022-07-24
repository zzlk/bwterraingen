use crate::rules::Rules;
use anyhow::anyhow;
use anyhow::Result;
use std::collections::HashSet;

pub fn get_rules_from_chk(chk: &[u8]) -> Result<Rules> {
    let raw_chunks = bwmap::parse_chk(chk);
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

    let mut banned_tiles = HashSet::new();
    for i in 0..16 {
        banned_tiles.insert(i);
    }

    anyhow::Ok(Rules::new(
        *dim.width as isize,
        *dim.height as isize,
        &mtxm.data,
        &banned_tiles,
        *era.tileset,
    ))
}

pub fn get_list_of_unique_tiles_from_chk(chk: &[u8]) -> Result<HashSet<u16>> {
    let raw_chunks = bwmap::parse_chk(chk);
    let merged_chunks = bwmap::merge_raw_chunks(raw_chunks.as_slice());
    let parsed_chunks = bwmap::parse_merged_chunks(&merged_chunks)?;

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

    let mut set = HashSet::new();

    for tile in &mtxm.data {
        set.insert(*tile);
    }

    anyhow::Ok(set.drain().collect())
}

pub fn get_dim_from_chk(chk: &[u8]) -> Result<(u16, u16, Vec<u16>)> {
    let raw_chunks = bwmap::parse_chk(chk);
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

    anyhow::Ok((*dim.width, *dim.height, mtxm.data.clone()))
}

pub fn create_chk_from_wave(map: &Vec<u16>, era: u16, width: usize, height: usize) -> Vec<u8> {
    let mut bytes = include_bytes!("data/template.chk").to_vec();

    bytes.extend_from_slice(b"MASK");
    bytes.extend_from_slice(((width * height) as u32).to_le_bytes().as_slice());
    for _ in 0..(width * height) {
        bytes.extend_from_slice(0xFFu8.to_le_bytes().as_slice());
    }

    bytes.extend_from_slice(b"ERA ");
    bytes.extend_from_slice(2u32.to_le_bytes().as_slice());
    bytes.extend_from_slice((era as u16).to_le_bytes().as_slice());

    bytes.extend_from_slice(b"DIM ");
    bytes.extend_from_slice(4u32.to_le_bytes().as_slice());
    bytes.extend_from_slice((width as u16).to_le_bytes().as_slice());
    bytes.extend_from_slice((height as u16).to_le_bytes().as_slice());

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

    bytes
}
