#[macro_use]
extern crate serde_derive;
// mod annealing;
mod layout;
mod charmap;

use std::borrow::Cow;
use std::collections::BTreeSet;
use std::fs::File;

use clap::{arg, command, Command};

use layout::{CostMap, RawFreqData};

use self::layout::optimize;

fn main() {
    let matches = command!()
        .subcommand_required(true)
        .arg(arg!(--data[data]))
        .arg(arg!(--layout <layout>))
        .subcommand(Command::new("opt").arg(arg!(--output <output>)).arg(arg!(--rounds [rounds])))
        .subcommand(Command::new("test"))
        .get_matches();
    let data = matches
        .value_of("data")
        .map(|s| Cow::Owned(std::fs::read_to_string(s).unwrap()))
        .unwrap_or(Cow::Borrowed(include_str!("../grams.json")));
    let freqs_data_raw: RawFreqData = serde_json::from_str(data.as_ref()).unwrap();
    let chars: Vec<char> = freqs_data_raw.iter().flat_map(|(v, _)| v.chars().collect::<Vec<_>>()).collect::<BTreeSet<char>>().into_iter().collect();
    println!("chars: len: {}, {:?}", chars.len(), chars);
    let mut phys: CostMap =
        serde_yaml::from_reader(File::open(matches.value_of("layout").unwrap()).unwrap()).unwrap();
    phys.cache();
    match matches.subcommand().unwrap() {
        ("opt", m) => {
            let res = optimize(&phys, &chars, &freqs_data_raw, m.value_of_t("rounds").unwrap_or(500));
            // if let Some(output) = m.value_of("output") {
            //     let out = File::create(output).unwrap();
            //     serde_json::to_writer_pretty(out, &res).unwrap();
            // }
        }
        // ("test", m) => {
        //     let map: CharMap =
        //         serde_json::from_reader(File::open(m.value_of("map").unwrap()).unwrap()).unwrap();
        //     let costs = phys.costs_cache();
        //     let layout = Layout::new(&phys);
        //     println!("valid: {}", map.is_valid());
        //     layout.print(&map);
        //     println!("cost: {}", map.cost(&freqs_data_raw, &phys, &costs));
        // }
        _ => unreachable!(),
    }
}
