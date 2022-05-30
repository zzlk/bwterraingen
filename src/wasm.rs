use crate::{create_chk_from_wave, Rules, Wave};
use js_sys::{Uint16Array, Uint8Array};
use tracing::{error, Level};
use tracing_wasm::ConsoleConfig;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn get_rules(chk: &[u8]) -> String {
    serde_json::to_string(&crate::get_rules_from_chk(&chk).unwrap()).unwrap()
}

#[wasm_bindgen]
pub fn do_map(
    width: usize,
    height: usize,
    rules_json: String,
    update: &js_sys::Function,
) -> Vec<u8> {
    let rules: Rules = serde_json::from_str(rules_json.as_str()).unwrap();
    let this = JsValue::null();
    let wave = Wave::new(width, height, &rules)
        .logical_conclusion(&|wave| {
            let raw = wave.render();
            let png = crate::render::render(raw.as_slice(), width, height, rules.era as usize);
            let chk = create_chk_from_wave(&raw, rules.era, width, height);
            update
                .call3(
                    &this,
                    &JsValue::from(Uint8Array::from(chk.as_slice())),
                    &JsValue::from(Uint16Array::from(raw.as_slice())),
                    &JsValue::from(Uint8Array::from(png.as_slice())),
                )
                .unwrap();
        })
        .unwrap();

    let raw = wave.render();
    let png = crate::render::render(raw.as_slice(), width, height, rules.era as usize);
    let chk = create_chk_from_wave(&raw, rules.era, width, height);
    update
        .call3(
            &this,
            &JsValue::from(Uint8Array::from(chk.as_slice())),
            &JsValue::from(Uint16Array::from(raw.as_slice())),
            &JsValue::from(Uint8Array::from(png.as_slice())),
        )
        .unwrap();

    create_chk_from_wave(&wave.render(), rules.era, width, height)
}

// Called when the wasm module is instantiated
#[wasm_bindgen(start)]
pub fn main() {
    console_error_panic_hook::set_once();

    // LogTracer::init().expect("Failed to set logger");

    // let filter = EnvFilter::from_default_env();
    // let subscriber = tracing_subscriber::fmt()
    //     // filter spans/events with level TRACE or higher.
    //     .with_env_filter(filter)
    //     .with_span_events(FmtSpan::CLOSE)
    //     .with_file(true)
    //     .with_target(false)
    //     .with_line_number(true)
    //     .with_level(false)
    //     // build but do not install the subscriber.
    //     .finish();

    // tracing::subscriber::set_global_default(subscriber).unwrap();

    let subscriber = tracing_wasm::WASMLayerConfigBuilder::new()
        .set_max_level(Level::TRACE)
        .set_console_config(ConsoleConfig::ReportWithConsoleColor)
        .set_report_logs_in_timings(true)
        .build();

    tracing_wasm::set_as_global_default_with_config(subscriber);

    //wasm_logger::init(wasm_logger::Config::default());
    // // Use `web_sys`'s global `window` function to get a handle on the global
    // // window object.
    // let window = web_sys::window().expect("no global `window` exists");
    // let document = window.document().expect("should have a document on window");
    // let body = document.body().expect("document should have a body");

    // // Manufacture the element we're gonna append
    // let val = document.create_element("p")?;
    // val.set_inner_html("Open the browser's console to see log outputs");

    // body.append_child(&val)?;

    error!("hello from rust");

    // Ok(())
}
