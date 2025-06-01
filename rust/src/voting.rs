use rand::{Rng, distr::Alphanumeric, rng};

fn random_string(length: usize) -> String {
    rng()
        .sample_iter(&Alphanumeric)
        .take(length)
        .map(char::from)
        .collect()
}

fn generate_random_strings(count: usize, length: usize) -> Vec<String> {
    (0..count).map(|_| random_string(length)).collect()
}

pub struct Person {
    pub name: String,
    pub uuid: String,
}

pub struct Poll {
    pub name: String,
    pub options: Vec<Option>,
}

pub struct Option {
    pub name: Person,
    pub votes: u32,
}

pub struct Vote {
    pub person: String,
    pub option: String,
}

fn vote_simulation() {
    let candidates: Vec<Option> = vec!["John", "Jane", "Jim"]
        .iter()
        .map(|name| Option {
            name: Person {
                name: name.to_string(),
                uuid: random_string(10),
            },
            votes: 0,
        })
        .collect();

    let voters: Vec<Person> = generate_random_strings(100, 10)
        .iter()
        .map(|name| Person {
            name: name.to_string(),
            uuid: random_string(10),
        })
        .collect();

    // each voter picks a random vote
    let votes: Vec<Vote> = voters.iter().map(|voter| {
        let option = candidates[rng().random_range(0..candidates.len())];
        Vote {
            person: voter.uuid.clone(),
            option: option.name.uuid.clone(),
        }
    }).collect();
}
