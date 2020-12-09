// mod annealing;
mod layout;

use rayon::prelude::*;

use std::collections::BTreeMap;

use indicatif::{ProgressBar, ProgressStyle};

use layout::{CharMap, Layout};

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
    let freqs_data_raw: BTreeMap<String, usize> =
        serde_json::from_str::<BTreeMap<String, usize>>(include_str!("../ngrams-all.json"))
            .unwrap()
            .into_iter()
            .collect();

    let mut target = 30000000000000.0f64;
    let mut lowest_count = 0;
    let char_map = CharMap::new(&('a'..='z').collect::<Vec<_>>());
    let freqs_data = char_map.transform_freqs_data(&freqs_data_raw);
    let layout_cost_cache = Layout::costs();
    loop {
        let mut layout = Layout::init();
        let init_cost = layout.total_cost(&freqs_data, &layout_cost_cache);
        let mut cost = init_cost;

        for cycle in 0.. {
            let mut new_layouts = (0..char_map.len())
                .flat_map(|i| {
                    ((i + 1)..layout.0.len())
                        .into_par_iter()
                        .map(|j| {
                            let mut new_layout = layout.0.clone();
                            new_layout.swap(i, j);
                            let new_layout = Layout(new_layout);
                            let new_cost = new_layout.total_cost(&freqs_data, &layout_cost_cache);
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
            layout.print(&char_map);
            target = cost;
            lowest_count = 0;
            println!("==============================");
            println!("||  New Target: {}", target);
            println!("==============================");
            let mesg = format!("cost: {}\n{}", cost, layout.repr_layout(&char_map));
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
        if lowest_count > 1 {
            // We've hit it more than 2 times
            println!("cost: {}, ratio: {}", cost, cost / init_cost);
            layout.print(&char_map);
            break;
        }
    }
}
