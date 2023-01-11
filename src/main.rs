#[macro_use]
extern crate serde_derive;
// mod annealing;
mod layout;

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fs::File;

use clap::{arg, command, Command};

use layout::{PhysicalLayout, Layout};

use self::layout::{optimize, CharMap};

fn main() {
    let matches = command!()
        .subcommand_required(true)
        .arg(arg!(--data[data]))
        .arg(arg!(--layout <layout>))
        .subcommand(Command::new("opt").arg(arg!(--output <output>)))
        .subcommand(Command::new("test").arg(arg!(--map <map>)))
        .get_matches();
    let data = matches
        .value_of("data")
        .map(|s| Cow::Owned(std::fs::read_to_string(s).unwrap()))
        .unwrap_or(Cow::Borrowed(include_str!("../ngram2.json")));
    let freqs_data_raw: BTreeMap<String, usize> =
        serde_json::from_str::<BTreeMap<String, usize>>(data.as_ref())
            .unwrap()
            .into_iter()
            .collect();
    let chars: Vec<char> = ('a'..='z').collect();
    let phys: PhysicalLayout =
        serde_yaml::from_reader(File::open(matches.value_of("layout").unwrap()).unwrap()).unwrap();
    match matches.subcommand().unwrap() {
        ("opt", m) => {
            let res = optimize(&phys, chars, &freqs_data_raw);
            if let Some(output) = m.value_of("output") {
                let out = File::create(output).unwrap();
                serde_json::to_writer_pretty(out, &res).unwrap();
            }
        }
        ("test", m) => {
            let map: CharMap =
                serde_json::from_reader(File::open(m.value_of("map").unwrap()).unwrap()).unwrap();
            let costs = phys.costs_cache();
            let layout = Layout::new(&phys);
            println!("valid: {}", map.is_valid());
            layout.print(&map);
            println!("cost: {}", map.cost(&freqs_data_raw, &phys, &costs));
        }
        _ => unreachable!(),
    }
}
