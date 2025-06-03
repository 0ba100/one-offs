use clap::Parser;
use image::{imageops::FilterType, ImageFormat};

#[derive(Parser)]
struct Args {
    #[arg(short, long)]
    input: String
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    
    let resized_image = image::open(args.input)?.resize(150, 150, FilterType::Lanczos3);

    resized_image.save_with_format("compressed.avif", ImageFormat::Avif)?;
    
    Ok(())
}
