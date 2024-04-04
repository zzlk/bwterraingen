use crate::rules::Rules;
use anyhow::Result;
use bwmap::ParsedChk;
use hashbrown::HashSet;

pub fn get_rules_from_chk(chk: &[u8]) -> Result<Rules> {
    let parsed_chk = ParsedChk::from_bytes(chk);

    let dim = parsed_chk.dim?;

    let mtxm = parsed_chk.mtxm?;

    let era = parsed_chk.era?;

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
    let parsed_chk = ParsedChk::from_bytes(chk);

    let mtxm = parsed_chk.mtxm?;

    let mut set = HashSet::new();

    for tile in &mtxm.data {
        set.insert(*tile);
    }

    anyhow::Ok(set.drain().collect())
}

pub fn get_dim_from_chk(chk: &[u8]) -> Result<(u16, u16, Vec<u16>)> {
    let parsed_chk = ParsedChk::from_bytes(chk);

    let dim = parsed_chk.dim?;
    let mtxm = parsed_chk.mtxm?;

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
    bytes.extend_from_slice(era.to_le_bytes().as_slice());

    bytes.extend_from_slice(b"DIM ");
    bytes.extend_from_slice(4u32.to_le_bytes().as_slice());
    bytes.extend_from_slice((width as u16).to_le_bytes().as_slice());
    bytes.extend_from_slice((height as u16).to_le_bytes().as_slice());

    bytes.extend_from_slice(b"MTXM");
    bytes.extend_from_slice(((map.len() * 2) as u32).to_le_bytes().as_slice());
    for word in map.iter() {
        bytes.extend_from_slice((*word).to_le_bytes().as_slice());
    }

    bytes.extend_from_slice(b"TILE");
    bytes.extend_from_slice(((map.len() * 2) as u32).to_le_bytes().as_slice());
    for word in map.iter() {
        bytes.extend_from_slice((*word).to_le_bytes().as_slice());
    }

    bytes
}
