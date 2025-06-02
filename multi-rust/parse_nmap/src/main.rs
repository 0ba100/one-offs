use std::{
    fs::File,
    io::{BufRead, BufReader},
};

struct Record {
    name: String,
    port: String,
    prevalence: f64
}

fn main() -> anyhow::Result<()> {
    let mut reader = BufReader::new(File::open("nmap-services.txt")?);
    let mut line = String::new();

    let mut records: Vec<Record> = Vec::new();

    while reader.read_line(&mut line)? > 0 {
        if &line.chars().nth(0) == &Some('#') {
            line.clear();
            continue;
        }

        let split: Vec<&str> = line.split('\t').collect();

        records.push(Record { name: split[0].to_owned(), port: split[1].to_owned(), prevalence: split[2].to_owned().trim().parse::<f64>()?});

        line.clear();
    }

    records.sort_by(|a, b| b.prevalence.total_cmp(&a.prevalence));

    for (i, record) in records.iter().take(30).enumerate() {
        println!("#{}: {} {} {}", i+1, record.name, record.port, record.prevalence)
    }

    Ok(())
}
