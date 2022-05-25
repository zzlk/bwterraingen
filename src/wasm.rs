use crate::{create_chk_from_wave, Rules, Wave};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn get_rules(chk: Vec<u8>) -> String {
    serde_json::to_string(&crate::get_rules_from_chk(&chk).unwrap()).unwrap()
}

#[wasm_bindgen]
pub fn do_map(width: usize, height: usize, rules_json: String) -> Vec<u8> {
    let rules: Rules = serde_json::from_str(rules_json.as_str()).unwrap();
    let wave = Wave::new(width, height, &rules)
        .logical_conclusion()
        .unwrap();

    create_chk_from_wave(&wave.render(), rules.era, width, height)
}
