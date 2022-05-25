use anyhow::Result;
use std::env;
use tracing::info;
use tracing_log::LogTracer;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::EnvFilter;

// use bwmap::get_chk_from_mpq_filename;
use bwterraingen::{get_rules_from_chk, Wave};

// fn print_map(map: &Vec<usize>, width: isize, height: isize) {
//     // 16 is creep, 64 is high dirt. 96 is water. 128 is yellow-grass
//     info!("map:");
//     for y in 0..height {
//         let mut output = String::new();
//         for x in 0..width {
//             output = format!("{}{:5}", output, map[(x + y * width) as usize]);
//         }
//         info!("        {}", output);
//     }
// }

fn main() -> Result<()> {
    setup_logging()?;

    let args: Vec<String> = env::args().collect();
    anyhow::ensure!(args.len() >= 1);

    let width = (&args[1]).parse::<usize>()?;
    let height = (&args[2]).parse::<usize>()?;

    anyhow::ensure!(args.len() > 3);

    let rules = args[3..]
        .into_iter()
        .filter_map(|arg| {
            info!("filename: {arg}");
            //let chk = get_chk_from_mpq_filename(arg.clone()).unwrap();
            let chk = std::fs::read(arg).unwrap();
            Some(get_rules_from_chk(&chk).unwrap())
        })
        .reduce(|x, y| x.combine(&y).unwrap())
        .unwrap();

    let wave = Wave::new(width, height, &rules).logical_conclusion()?;

    let output_chk = bwterraingen::create_chk_from_wave(&wave.render(), rules.era, width, height);

    std::fs::write("/home/stan/Downloads/out.chk", output_chk)?;

    anyhow::Ok(())
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
