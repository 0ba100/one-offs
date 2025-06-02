use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn main() -> anyhow::Result<()> {
    let mut reader = BufReader::new(File::open("nmap-services.txt")?);
    let mut line = String::new();

    let mut records: Vec<(String, String, f64)> = Vec::new();

    while reader.read_line(&mut line)? > 0 {
        if &line.chars().nth(0) == &Some('#') {
            line.clear();
            continue;
        }

        let split: Vec<&str> = line.split('\t').collect();

        records.push((split[0].to_owned(), split[1].to_owned(), split[2].to_owned().trim().parse::<f64>()?));

        line.clear();
    }

    records.sort_by(|a, b| b.2.total_cmp(&a.2));

    for (i, record) in records.iter().take(30).enumerate() {
        println!("#{}: {} {} {}", i+1, record.0, record.1, record.2)
    }

    Ok(())
}
