#[macro_use]
extern crate serde_derive;

mod annealing;
mod layout;

use std::collections::BTreeMap;

use indicatif::{ProgressBar, ProgressStyle};

use layout::{Layout, FreqsData};

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
    let mut layout: Layout = Layout({
        let mut target_chars: Vec<char> = (0..36)
            .map(|i| {
                if i < 26 {
                    (i as u8 + 'a' as u8) as char
                } else {
                    ' '
                }
            })
            .collect();
        (1..=255u8)
            .filter(|i| i.count_ones() <= 2)
            .enumerate()
            .map(|(_i, k)| {
                let ind = rand::random::<usize>() % target_chars.len();
                (k, target_chars.remove(ind))
            })
            .collect()
    });
    let freqs_data: FreqsData =
        serde_json::from_str::<BTreeMap<String, usize>>(include_str!("../ngrams-all.json"))
            .unwrap()
            .into_iter()
            .collect();
    let init_cost = layout.total_cost(&freqs_data);
    let mut cost = init_cost;

    let pbar = progress_bar(annealing::N, "annealing");
    pbar.set_draw_delta(annealing::N as u64 / 100);
    let mut layouts = Vec::new();
    for i in 1..(annealing::N + 1) {
        let mut new_layout = layout.clone();
        let times = rand::random::<usize>() % 3 + 1;
        new_layout.shuffle(times);
        let new_cost = new_layout.total_cost(&freqs_data);
        if annealing::accept_transition(new_cost - cost, i) {
            cost = new_cost;
            layout = new_layout;
            layouts.push((layout.clone(), cost));
        }
        if i % 100 == 0 && layouts.len() > 20 {
            layouts.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
            layouts.drain(20..);
        }
        pbar.inc(1);
    }
    pbar.finish_with_message("annealed");
    println!("init cost: {}", init_cost);
    for layout in layouts.iter().take(3) {
        println!("cost: {}, ratio: {}", layout.1, layout.1 / init_cost);
        layout.0.print();
    }
}
