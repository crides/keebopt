#[macro_use]
extern crate serde_derive;
// mod annealing;
mod charmap;
mod data;
mod layout;

use std::collections::BTreeSet;
use std::fs::File;

use clap::{arg, command, Args, Parser, Subcommand};

use layout::{optimize, CostMap};

use crate::layout::ChordMap;

use self::layout::RawFreqData;

#[derive(Parser, Debug)]
#[command(author, version, about, propagate_version = true)]
struct Options {
    #[arg(long)]
    layout: String,
    #[command(subcommand)]
    cmd: Subcmds,
}

#[derive(Subcommand, Debug)]
enum Subcmds {
    Opt {
        #[arg(short = 'o', long)]
        output: Option<String>,
        #[arg(short = 'r', long)]
        rounds: Option<usize>,
        #[command(flatten)]
        freq: FreqInput,
    },
    Gram {
        #[arg(short = 'i', long)]
        input: String,
        #[arg(short = 'o', long)]
        output: String,
    },
    Score {
        #[arg(short = 'm', long)]
        map: String,
        #[command(flatten)]
        freq: FreqInput,
    },
}

#[derive(Args, Debug)]
struct FreqInput {
    #[arg(short = 'g', long, group = "data")]
    gram: Option<String>,
    #[arg(short = 'l', long, group = "data")]
    log: Option<String>,
}

fn get_freqs(freq: &FreqInput) -> (RawFreqData, Vec<char>) {
    let grams: RawFreqData = match freq {
        FreqInput {
            gram: Some(gram), ..
        } => serde_json::from_str(std::fs::read_to_string(&gram).unwrap().as_ref()).unwrap(),
        FreqInput { log: Some(log), .. } => {
            data::gram_chars(data::parse_logkeys(&log).into_iter(), 2)
                .into_iter()
                .collect()
        }
        _ => serde_json::from_str(include_str!("../grams.json")).unwrap(),
    };
    let chars: Vec<char> = grams
        .iter()
        .flat_map(|(v, _)| v.chars().collect::<Vec<_>>())
        .collect::<BTreeSet<char>>()
        .into_iter()
        .collect();
    (grams, chars)
}

fn main() {
    let opts = Options::parse();
    let mut phys: CostMap = serde_yaml::from_reader(File::open(opts.layout).unwrap()).unwrap();
    phys.cache();
    match opts.cmd {
        Subcmds::Opt {
            output,
            rounds,
            freq,
        } => {
            let (freq_data_raw, chars) = get_freqs(&freq);
            println!("chars: len: {}, {:?}", chars.len(), chars);
            let res = optimize(&phys, &chars, &freq_data_raw, rounds.unwrap_or(500));
            if let Some(output) = output {
                std::fs::write(
                    &output,
                    serde_json::to_string_pretty(&res.to_map()).unwrap(),
                )
                .unwrap();
            }
        }
        Subcmds::Gram { input, output } => {
            let grams = data::gram_chars(data::parse_logkeys(&input).into_iter(), 2);
            let flat: Vec<_> = grams.into_iter().collect();
            std::fs::write(&output, serde_json::to_string(&flat).unwrap()).unwrap();
        }
        Subcmds::Score { map, freq } => {
            let freq_data_raw = get_freqs(&freq).0;
            let map =
                ChordMap::from_map(&serde_json::from_reader(File::open(&map).unwrap()).unwrap());
            println!("valid: {}", map.is_valid());
            map.print(&phys);
            println!("cost: {}", phys.total_cost(&map, &freq_data_raw));
        }
    }
}
