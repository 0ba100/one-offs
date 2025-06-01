use rand::{Rng, distr::Alphanumeric, rng};
use std::fmt::{self, Display};

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

pub fn vote_simulation() {
    let runners = vec!["John", "Jane", "Jim"].iter().map(|name| Person {
        name: name.to_string(),
        uuid: random_string(10),
    }).collect::<Vec<Person>>();

    let voters = generate_random_strings(100000, 5).iter().map(|name| Person {
        name: name.to_string(),
        uuid: random_string(10),
    }).collect::<Vec<Person>>();

    let votes = voters.iter().map(|voter| {
        let option = &runners[rng().random_range(0..runners.len())];
        Vote {
            person: voter.uuid.clone(),
            option: option.uuid.clone(),
        }
    }).collect::<Vec<Vote>>();

    let candidates = runners.iter().map(|runner| Option {
        person: runner.name.clone(),
        votes: votes.iter().filter(|vote| vote.option == runner.uuid).count() as u32,
    }).collect::<Vec<Option>>();

    println!("{}", candidates.iter().map(|c| c.to_string()).collect::<Vec<String>>().join("\n"));
}

pub struct Person { pub name: String, pub uuid: String }

impl Display for Person {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub struct Option { pub person: String, pub votes: u32 }

impl Display for Option {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.person, self.votes)
    }
}

#[derive(Debug)]
pub struct Vote { pub person: String, pub option: String }
