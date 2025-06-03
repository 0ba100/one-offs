use anyhow::anyhow;
use clap::Parser;
use image::{ImageFormat, imageops::FilterType};

#[derive(Parser)]
struct Args {
    #[arg(short, long)]
    input: String,
}

fn main() -> anyhow::Result<()> {
    loop {
        let file_names = scan_download_folder_for_pngs()?;
        println!("Press enter to continue");

        std::io::stdin().read_line(&mut String::new())?;
        let second_scan = scan_download_folder_for_pngs()?;
        let new_files = second_scan
            .iter()
            .filter(|new| !file_names.iter().find(|old| old == new).is_some());

        let mut count = 0;
        for file in new_files {
            count += 1;
            println!("Compressing {}", file);
            compress_image(file)?;
        }
        println!("Compressed {} files", count);
    }
}

fn compress_image(filename: &String) -> anyhow::Result<()> {
    image::open(std::env::var("HOME")? + "/Downloads/" + &filename)?
        .resize(150, 150, FilterType::Lanczos3)
        .save_with_format(
            filename
                .split('.')
                .next()
                .ok_or(anyhow!("Bad Form"))?
                .trim()
                .to_owned()
                + ".avif",
            ImageFormat::Avif,
        )?;

    Ok(())
}

fn scan_download_folder_for_pngs() -> anyhow::Result<Vec<String>> {
    let download_folder = std::env::var("HOME").unwrap() + "/Downloads";
    let files = std::fs::read_dir(download_folder).unwrap();
    let mut file_names = Vec::new();
    for file in files {
        let file = file.unwrap();
        let file_name = file.file_name();
        file_names.push(file_name.to_string_lossy().to_string());
    }
    let file_names = file_names
        .iter()
        .filter(|name| name.ends_with(".png"))
        .map(|name| name.to_string())
        .collect::<Vec<String>>();

    Ok(file_names)
}
