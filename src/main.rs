// mod annealing;
mod layout;

use std::collections::BTreeMap;
use std::io::Write;

use layout::{CharMap, Layout};

fn main() {
    let freqs_data_raw: BTreeMap<String, usize> =
        serde_json::from_str::<BTreeMap<String, usize>>(include_str!("../ngram2.json"))
            .unwrap()
            .into_iter()
            .collect();

    let mut target = 30000000000000.0f64;
    let char_map = CharMap::new(&('a'..='z').collect::<Vec<_>>());
    let freqs_data = char_map.transform_freqs_data(&freqs_data_raw);
    let layout_cost_cache = Layout::costs();
    loop {
        let mut layout = Layout::init();
        let mut cost = layout.total_cost(&freqs_data, &layout_cost_cache);

        for cycle in 0.. {
            let mut new_layouts = (0..char_map.len())
                .flat_map(|i| {
                    ((i + 1)..layout.0.len())
                        .map(|j| (i, j))
                        .collect::<Vec<_>>()
                })
                .map(|(i, j)| {
                    let mut new_layout = layout.0.clone();
                    new_layout.swap(i, j);
                    let new_layout = Layout(new_layout);
                    let new_cost = new_layout.total_cost(&freqs_data, &layout_cost_cache);
                    (new_layout, new_cost)
                })
                .collect::<Vec<_>>();
            new_layouts.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
            let old_cost = cost;
            let best = new_layouts.into_iter().next().unwrap();
            cost = best.1;
            layout = best.0;
            if cost >= old_cost {
                print!(".");
                std::io::stdout().lock().flush().unwrap();
                break;
            }
        }
        if cost < target {
            println!("\ncost: {}", cost);
            target = cost;
        // break;
        } else if cost == target {
            println!("\ncost: {}", cost);
            layout.print(&char_map);
            break;
        }
    }
}
