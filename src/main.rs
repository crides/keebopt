#[macro_use]
extern crate serde_derive;

// mod annealing;
mod layout;

use std::collections::BTreeMap;

use indicatif::{ProgressBar, ProgressStyle};

use layout::{Chord, FreqsData, Layout};

pub fn progress_bar(len: usize, msg: &str) -> ProgressBar {
    let pbar = ProgressBar::new(len as u64).with_style(
        ProgressStyle::default_bar()
            .progress_chars("=> ")
            .template("{msg} [{wide_bar}] ETA: {eta} ({pos}/{len})"),
    );
    pbar.set_message(msg);
    pbar
}

fn main() {
    let freqs_data: FreqsData =
        serde_json::from_str::<BTreeMap<String, usize>>(include_str!("../ngrams-all.json"))
            .unwrap()
            .into_iter()
            .collect();

    let mut target = 30000000000000.0f64;
    let mut lowest_count = 0;
    loop {
        let mut layout = Layout::init();
        let init_cost = layout.total_cost(&freqs_data);
        let mut cost = init_cost;

        for cycle in 0.. {
            let maps = {
                let mut maps: Vec<(Chord, char)> =
                    layout.0.iter().map(|(ch, &c)| (ch.clone(), c)).collect();
                maps.sort_by_key(|p| if p.1 == ' ' { 1 } else { 0 });
                maps
            };
            let mut new_layouts = (0usize..26)
                .flat_map(|i| {
                    ((i + 1)..Layout::CHORD_NUM_POS)
                        .map(|j| {
                            let mut new_maps = maps.clone();
                            let old_char = new_maps[i].1;
                            new_maps[i].1 = new_maps[j].1;
                            new_maps[j].1 = old_char;
                            let new_layout = Layout(new_maps.into_iter().collect());
                            let new_cost = new_layout.total_cost(&freqs_data);
                            (new_layout, new_cost)
                        })
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();
            new_layouts.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
            let old_cost = cost;
            let best = new_layouts.into_iter().next().unwrap();
            cost = best.1;
            layout = best.0;
            if cost >= old_cost {
                println!("cycle = {}, cost: {}", cycle, cost);
                break;
            }
        }
        if cost < target {
            println!("cost: {}, ratio: {}", cost, cost / init_cost);
            layout.print();
            target = cost;
            println!("==============================");
            println!("||  New Target: {}", target);
            println!("==============================");
            let mesg = format!("cost: {}\n{}", cost, layout.repr_layout());
            std::process::Command::new("notify-send")
                .args(&["keebopt", &mesg, "-u", "critical"])
                .spawn()
                .unwrap();
        // break;
        } else if cost == target {
            lowest_count += 1;
            std::process::Command::new("notify-send")
                .args(&["keebopt", "!!!!!!!!", "-u", "critical"])
                .spawn()
                .unwrap();
        }
        if lowest_count > 0 {
            // We've hit it more than 1 times
            break;
        }
    }
}