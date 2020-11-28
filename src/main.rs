#![allow(dead_code)]
#[macro_use]
extern crate serde_derive;

mod annealing;

use std::collections::BTreeMap;

use indicatif::{ProgressBar, ProgressStyle};

pub fn progress_bar(len: usize, msg: &str) -> ProgressBar {
    let pbar = ProgressBar::new(len as u64).with_style(
        ProgressStyle::default_bar()
            .progress_chars("=> ")
            .template("{msg} [{wide_bar}] ETA: {eta} ({pos}/{len})"),
    );
    pbar.set_message(msg);
    pbar
}

#[derive(Deserialize)]
struct PhysicalKey {
    cost: f32,
    finger: u8,
    desc: String,
    output: String,
}

#[derive(Deserialize)]
#[serde(untagged)]
enum Step {
    Layer(u8),
    Chord(Vec<u8>),
}

#[derive(Deserialize)]
struct Key {
    cost: f32,
    path: Vec<Step>,
    desc: String,
    output: String,
}

type FreqsData = BTreeMap<String, usize>;

#[derive(Clone, Debug)]
pub struct Layout(BTreeMap<u8, char>);

impl Layout {
    fn shuffle(&mut self) {
        let mut maps: Vec<(u8, char)> = self.0.clone().into_iter().collect();
        let (i, j) = (
            rand::random::<usize>() % maps.len(),
            rand::random::<usize>() % (maps.len() - 1),
        );
        let j = if j >= i { j + 1 } else { j };
        let (mut a, mut b) = (maps[i], maps[j]);
        std::mem::swap(&mut a.1, &mut b.1);
        maps[i] = a;
        maps[j] = b;
        self.0 = maps.into_iter().collect();
    }

    fn _cost(finger: u8) -> f32 {
        match finger {
            8 => 1.0,
            4 => 1.1,
            2 => 1.4,
            1 => 2.3,
            0 => 0.0,
            _ => panic!("{:02x}", finger),
        }
    }

    fn cost(finger: u8) -> f32 {
        Layout::_cost(finger & 0xF) + Layout::_cost(finger >> 4)
    }

    fn chord_cost(chord: u8) -> f32 {
        if chord.count_ones() == 1 {
            Layout::cost(chord)
        } else {
            let top = ((chord as usize).next_power_of_two() / 2) as u8;
            let bottom = chord & !top;
            let (a, b) = (Layout::cost(top), Layout::cost(bottom));
            if top >> 4 == bottom {
                2.0 * a
            } else if top >= 0x10 && bottom >= 0x10 || top < 0x10 && bottom < 0x10 {
                1.2 * if a > b { a } else { b }
            } else if top == 8 && bottom == 4 || bottom == 4 && top == 2 || bottom == 2 && top == 1
            {
                1.7 * if a > b { a } else { b }
            } else {
                1.3 * if a > b { a } else { b }
            }
        }
    }

    fn total_cost(&self, data: &FreqsData) -> f32 {
        let char_map: BTreeMap<char, u8> = self.0.iter().map(|(k, v)| (*v, *k)).collect();
        let mut cost = 0.0f32;
        for (s, count) in data.iter() {
            let mut last_chord = None;
            cost += (*count as f32)
                * s.chars()
                    .map(|c| {
                        let chord = char_map[&c.to_ascii_lowercase()];
                        let mut cost = Layout::chord_cost(chord);
                        if let Some(last) = last_chord {
                            let common = last & chord;
                            if common != 0 {
                                cost += Layout::chord_cost(common)
                            }
                        }
                        last_chord = Some(chord);
                        cost
                    })
                    .sum::<f32>();
        }
        cost
    }

    fn repr_chord(chord: u8) -> String {
        (0..4)
            .rev()
            .map(|i| {
                let dyad = (chord >> (i + 3)) & 0x02 | (chord >> i) & 0x01;
                match dyad {
                    0 => " ",
                    1 => "▄",
                    2 => "▀",
                    3 => "█",
                    _ => panic!("!!"),
                }
            })
            .collect()
    }

    fn print(&self) {
        let mut maps: Vec<(u8, char)> = self.0.clone().into_iter().collect();
        maps.sort_by_key(|e| {
            (
                e.0.count_ones(),
                -((e.0 as usize).next_power_of_two() as isize),
            )
        });
        for chunk in maps.chunks_exact(4) {
            println!(
                "|{}|: {}, |{}|: {}, |{}|: {}, |{}|: {}",
                Layout::repr_chord(chunk[0].0),
                chunk[0].1,
                Layout::repr_chord(chunk[1].0),
                chunk[1].1,
                Layout::repr_chord(chunk[2].0),
                chunk[2].1,
                Layout::repr_chord(chunk[3].0),
                chunk[3].1
            );
            println!("------------------------------------------");
        }
    }
}

pub struct LayoutPermutations {
    orig_layout: Layout,
    swap_idx: Vec<usize>,
    started: bool,
}

impl LayoutPermutations {
    pub fn new(layout: &Layout, depth: usize) -> LayoutPermutations {
        LayoutPermutations {
            orig_layout: layout.clone(),
            swap_idx: vec![0; depth * 2],
            started: false,
        }
    }
}

impl Iterator for LayoutPermutations {
    type Item = Layout;

    fn next(&mut self) -> Option<Layout> {
        let mut some = false;
        let mut idx = 0;
        let mut val = 0;

        if self.started {
            for (i, e) in self.swap_idx.iter_mut().enumerate() {
                if *e + 1 < self.orig_layout.0.len() - i {
                    *e += 1;
                    some = true;
                    idx = i;
                    val = *e;
                    break;
                }
            }
        } else {
            self.started = true;
            some = true;
            idx = 1;
            val = 0;
        }

        if some {
            for i in 0..idx {
                self.swap_idx[i] = val + idx - i;
            }

            let mut i = 0;
            let mut maps: Vec<(u8, char)> = self.orig_layout.0.clone().into_iter().collect();
            while i < self.swap_idx.len() {
                let (x, y) = (self.swap_idx[i], self.swap_idx[i + 1]);
                let (mut a, mut b) = (maps[x], maps[y]);
                std::mem::swap(&mut a.1, &mut b.1);
                maps[x] = a;
                maps[y] = b;
                i += 2;
            }
            Some(Layout(maps.into_iter().collect()))
        } else {
            None
        }
    }
}

fn main() {
    let init_layout: Layout = Layout({
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
    let freqs_data: FreqsData = serde_json::from_str(include_str!("../ngram4.json")).unwrap();
    let mut accepted_layout = init_layout.clone();
    let mut layouts = vec![(init_layout.total_cost(&freqs_data), init_layout)];
    let init_cost = layouts[0].0;
    let mut accepted_cost = init_cost;
    dbg!(accepted_cost);

    let refine = true;
    if refine {
        let mut best_cost = accepted_cost;
        loop {
            let new_layouts: Vec<_> = layouts
                .drain(..)
                .flat_map(|l| {
                    let perms = LayoutPermutations::new(&l.1, 2).collect::<Vec<_>>();
                    dbg!(perms.len());
                    let pbar = progress_bar(perms.len(), "refining");
                    pbar.set_draw_delta(100);
                    let perms = perms.into_iter().map(|l| {
                        pbar.inc(1);
                        (l.total_cost(&freqs_data), l)
                    }).collect::<Vec<_>>();
                    pbar.finish_with_message("refined");
                    perms
                })
                .collect();
            dbg!(new_layouts.len());
            layouts = new_layouts;
            layouts.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
            layouts = layouts
                .into_iter()
                .enumerate()
                .filter(|(i, _)| *i < 10)
                .map(|(_, l)| l)
                .collect();
            for i in 0..(std::cmp::min(layouts.len(), 3)) {
                dbg!(layouts[i].0);
                layouts[i].1.print();
            }
            dbg!(
                layouts[0].0,
                layouts[0].0 / init_cost,
                layouts[0].0 / best_cost
            );
            if layouts[0].0 < best_cost {
                best_cost = layouts[0].0;
            } else {
                break;
            }
        }
        dbg!(layouts[0].0, layouts[0].0 / init_cost);
        layouts[0].1.print();
    } else {
        let pbar = progress_bar(annealing::N, "annealing");
        pbar.set_draw_delta(annealing::N as u64 / 100);
        for i in 1..(annealing::N + 1) {
            let mut cur_layout = accepted_layout.clone();
            for _ in 0..(rand::random::<usize>() % 3 + 1) {
                cur_layout.shuffle();
            }
            let new_cost = cur_layout.total_cost(&freqs_data);
            if annealing::accept_transition(new_cost - accepted_cost, i) {
                accepted_cost = new_cost;
                accepted_layout = cur_layout;
                layouts.push((accepted_cost, accepted_layout.clone()));
                layouts.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
                layouts.dedup_by_key(|e| e.0);
                if layouts.len() > 20 {
                    layouts.pop();
                }
            }
            pbar.inc(1);
        }
        pbar.finish_with_message("annealed");
        dbg!(layouts[0].0, layouts[0].0 / init_cost);
        layouts[0].1.print();
    }
}
