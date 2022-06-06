use cached::proc_macro::cached;
use image::ImageDecoder;
use image::ImageEncoder;
use tracing::info;

const MAPS: [&[u8]; 8] = [
    include_bytes!("data/terrain/badlands.cv5.bin"),
    include_bytes!("data/terrain/platform.cv5.bin"),
    include_bytes!("data/terrain/install.cv5.bin"),
    include_bytes!("data/terrain/ashworld.cv5.bin"),
    include_bytes!("data/terrain/jungle.cv5.bin"),
    include_bytes!("data/terrain/desert.cv5.bin"),
    include_bytes!("data/terrain/ice.cv5.bin"),
    include_bytes!("data/terrain/twilight.cv5.bin"),
];

const TILESHEETS: [&[u8]; 8] = [
    include_bytes!("data/terrain/remaster/badlands.webp"),
    include_bytes!("data/terrain/remaster/platform.webp"),
    include_bytes!("data/terrain/remaster/install.webp"),
    include_bytes!("data/terrain/remaster/ashworld.webp"),
    include_bytes!("data/terrain/remaster/jungle.webp"),
    include_bytes!("data/terrain/remaster/desert.webp"),
    include_bytes!("data/terrain/remaster/ice.webp"),
    include_bytes!("data/terrain/remaster/twilight.webp"),
];

#[cached]
fn get_tilesheet(era: usize) -> Vec<u8> {
    let mut tile_map = Vec::new();
    let decoder = image::codecs::webp::WebPDecoder::new(TILESHEETS[era]).unwrap();

    tile_map.resize(decoder.total_bytes() as usize, 0);

    decoder.read_image(tile_map.as_mut_slice()).unwrap();

    tile_map
}

pub fn render(mtxm: &[u16], width: usize, height: usize, era: usize) -> Vec<u8> {
    // let start = Instant::now();
    let mut png = Vec::<u8>::new();
    let tile_map = get_tilesheet(era);

    // {
    //     let decoder = image::codecs::webp::WebPDecoder::new(TILESHEETS[era]).unwrap();

    //     tile_map.resize(decoder.total_bytes() as usize, 0);

    //     decoder.read_image(tile_map.as_mut_slice()).unwrap();
    // }

    // {
    //     let decoder = image::codecs::png::PngDecoder::new(TILESHEETS[era]).unwrap();

    //     tile_map.resize(decoder.total_bytes() as usize, 0);

    //     decoder.read_image(tile_map.as_mut_slice()).unwrap();
    // }

    let mtxm_tile_map: &[u16] = {
        fn reinterpret_slice2<T: Sized>(s: &[u8]) -> &[T] {
            assert!(
                s.len() % std::mem::size_of::<T>() == 0,
                "s.len(): {}, std::mem::size_of::<T>(): {}",
                s.len(),
                std::mem::size_of::<T>()
            );

            unsafe {
                std::slice::from_raw_parts(
                    s.as_ptr() as *const T,
                    s.len() / std::mem::size_of::<T>(),
                )
            }
        }

        reinterpret_slice2(MAPS[era])
    };

    {
        let mut img: image::RgbImage =
            image::ImageBuffer::new((width * 32) as u32, (height * 32) as u32);

        for y in 0..height {
            for x in 0..width {
                let mtxm_id = mtxm[x + y * width];

                let output_x = x * 32;
                let output_y = y * 32;

                let input_x = (mtxm_tile_map[mtxm_id as usize] % 64) as usize * 32;
                let input_y = (mtxm_tile_map[mtxm_id as usize] / 64) as usize * 32;

                for j in 0..32 {
                    for i in 0..32 {
                        img.put_pixel(
                            (output_x + i) as u32,
                            (output_y + j) as u32,
                            image::Rgb([
                                tile_map[(input_x + i) as usize * 4
                                    + (input_y + j) as usize * 64 * 32 * 4
                                    + 0],
                                tile_map[(input_x + i) as usize * 4
                                    + (input_y + j) as usize * 64 * 32 * 4
                                    + 1],
                                tile_map[(input_x + i) as usize * 4
                                    + (input_y + j) as usize * 64 * 32 * 4
                                    + 2],
                            ]),
                        );
                    }
                }
            }
        }

        image::codecs::png::PngEncoder::new(&mut png)
            .write_image(
                img.as_raw(),
                img.width(),
                img.height(),
                image::ColorType::Rgb8,
            )
            .unwrap();
    }

    // info!(
    //     "rendering time: {}",
    //     Instant::now().duration_since(start).as_micros()
    // );
    png
}
