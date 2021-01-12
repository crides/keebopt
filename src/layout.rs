use std::collections::{BTreeMap, BTreeSet};

use rand::prelude::*;
use smallvec::{smallvec, SmallVec};

pub type FreqsData = Vec<(Vec<usize>, usize)>;

#[derive(Clone, Debug)]
pub struct Layout(pub Vec<Chord>);

pub struct CharMap(pub BTreeMap<char, usize>);

type ChordStore = SmallVec<[u8; 2]>;

#[derive(Clone, Debug, Ord, PartialOrd, PartialEq, Eq)]
pub struct Chord(pub ChordStore);

pub type CostCache = [[f64; Layout::KEYS_NUM]; Layout::KEYS_NUM];

impl CharMap {
    pub fn new(chars: &[char]) -> CharMap {
        CharMap(chars.iter().enumerate().map(|(i, c)| (*c, i)).collect())
    }

    pub fn transform_freqs_data(&self, data: &BTreeMap<String, usize>) -> FreqsData {
        &self.0;
        data.iter()
            .map(|(k, v)| {
                (
                    k.chars().map(|c| self.0[&c.to_ascii_lowercase()]).collect(),
                    *v,
                )
            })
            .collect()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl Layout {
    pub const KEYS_COST: &'static [f64] = &[
        1.0,
        1.1,
        1.4,
        2.3,
        1.0 * 1.2,
        1.1 * 1.2,
        1.4 * 1.2,
        2.3 * 1.2,
    ];
    pub const KEYS_NUM: usize = Layout::KEYS_COST.len();

    fn _chord_cost(chord: &Chord) -> f64 {
        if chord.0.len() == 1 {
            Layout::cost(chord.0[0])
        } else {
            let (big, small) = if chord.0[0] > chord.0[1] {
                (chord.0[0], chord.0[1])
            } else {
                (chord.0[1], chord.0[0])
            };
            let mut cost = Layout::cost(big) + Layout::cost(small);
            if big == small + 4 {
                cost *= 1.6;
            }
            if big >= 4 && small >= 4 || big < 4 && small < 4 {
            } else {
                cost *= 1.1;
            }
            if (big == 5 || big == 7) && (small == 1 || small == 3) {
                cost *= 1.2;
            }
            if small == 0 && big == 5 || big == 5 && small == 2 || big == 6 && small == 3 {
                cost *= 1.9;
            }
            cost
        }
    }

    fn consec_cost(old: &Chord, new: &Chord) -> f64 {
        let mut cost = 0.0;
        for l in &old.0 {
            for c in &new.0 {
                let (small, big) = if c < l { (*c, *l) } else { (*l, *c) };
                if small == big {
                    cost += 0.6 * Layout::cost(small);
                } else if small + 4 == big {
                    cost += 2.0 * Layout::cost(small);
                }
            }
        }
        match (old.0.len(), new.0.len()) {
            (1, 1) => (),
            (1, 2) | (2, 1) => cost *= 1.4,
            (2, 2) => cost *= 1.8,
            _ => unreachable!(),
        }
        cost
    }

    pub fn costs() -> CostCache {
        let mut costs = [[0.0; Layout::KEYS_NUM]; Layout::KEYS_NUM];
        for i in 0..Layout::KEYS_NUM {
            for j in 0..Layout::KEYS_NUM {
                if i == j {
                    costs[i][i] = Layout::_chord_cost(&Chord(smallvec![i as u8]));
                } else {
                    costs[i][j] = Layout::_chord_cost(&Chord(smallvec![i as u8, j as u8]));
                }
            }
        }
        costs
    }

    pub fn init() -> Layout {
        let mut layout: Vec<_> = (0..Layout::KEYS_NUM)
            .map(|i| smallvec![i as u8])
            .chain((0..Layout::KEYS_NUM).flat_map(|i| {
                ((i + 1)..Layout::KEYS_NUM)
                    .map(|j| smallvec![i as u8, j as u8])
                    .collect::<Vec<_>>()
            }))
            .map(Chord)
            .collect();
        layout.shuffle(&mut thread_rng());
        Layout(layout)
    }

    const fn cost(finger: u8) -> f64 {
        Layout::KEYS_COST[finger as usize]
    }

    fn chord_cost(chord: &Chord, costs: &CostCache) -> f64 {
        if chord.0.len() == 1 {
            Layout::cost(chord.0[0])
        } else {
            costs[chord.0[0] as usize][chord.0[1] as usize]
        }
    }

    pub fn total_cost(&self, data: &FreqsData, costs: &CostCache) -> f64 {
        let mut cost = 0.0f64;
        for (s, count) in data.iter() {
            let mut last_chord: Option<&Chord> = None;
            cost += (*count as f64) * {
                let mut cost = 0.0;
                for &c in s {
                    let chord = &self.0[c];
                    cost += Layout::chord_cost(chord, costs);
                    if let Some(last) = &last_chord {
                        cost += Layout::consec_cost(last, chord);
                    }
                    last_chord = Some(chord);
                }
                cost
            };
        }
        cost
    }

    fn repr_chord(chord: &Chord) -> String {
        let keys: BTreeSet<u8> = chord.0.iter().copied().collect();
        [(0, 4), (1, 5), (2, 6), (3, 7)]
            .iter()
            .map(|(top, bottom)| {
                let dyad = (keys.contains(top), keys.contains(bottom));
                match dyad {
                    (false, false) => " ",
                    (false, true) => "▄",
                    (true, false) => "▀",
                    (true, true) => "█",
                }
            })
            .collect()
    }

    fn repr_mapping(chord: &Chord, c: char) -> String {
        format!("|{}|: {}", Layout::repr_chord(chord), c)
    }

    pub fn repr_layout(&self, char_map: &CharMap) -> String {
        let rev_char_map: BTreeMap<usize, char> =
            char_map.0.iter().map(|(&c, &i)| (i, c)).collect();
        let char_num = rev_char_map.len();
        let mut maps: Vec<(Chord, char)> = self
            .0
            .iter()
            .enumerate()
            .filter(|(i, _)| *i < char_num)
            .map(|(i, c)| (c.clone(), rev_char_map[&i]))
            .collect();
        maps.sort_by_key(|e| {
            (
                if e.0 .0.len() > 1 { 1 } else { 0 },
                e.0 .0.iter().copied().max().unwrap(),
            )
        });
        maps.chunks(Layout::KEYS_NUM)
            .map(|chunk| {
                let reprs = chunk
                    .iter()
                    .map(|p| Layout::repr_mapping(&p.0, p.1))
                    .collect::<Vec<_>>();
                reprs.join(", ")
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn print(&self, char_map: &CharMap) {
        println!("{}", self.repr_layout(char_map));
    }
}
