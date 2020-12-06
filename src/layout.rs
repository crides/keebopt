use std::collections::{BTreeMap, BTreeSet};

use smallvec::{smallvec, SmallVec};

pub type FreqsData = Vec<(String, usize)>;

#[derive(Clone, Debug, Copy, Ord, PartialOrd, Eq, PartialEq)]
pub enum Char {
    Char(char),
    Nothing(u8),
}

#[derive(Clone, Debug)]
pub struct Layout(pub BTreeMap<Char, Chord>);

type ChordStore = SmallVec<[u8; 4]>;

#[derive(Clone, Debug, Ord, PartialOrd, PartialEq, Eq)]
pub struct Chord(pub ChordStore);

pub type CostCache = [[f64; Layout::KEYS_NUM]; Layout::KEYS_NUM];

impl Layout {
    pub const KEYS: &'static [u8] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    pub const KEYS_COST: &'static [f64] = &[3.2, 1.0, 1.1, 1.4, 2.3, 3.2, 1.0, 1.1, 1.4, 2.3];
    pub const KEYS_NUM: usize = Layout::KEYS.len();
    pub const CHORD_NUM_POS: usize = Layout::KEYS_NUM + Layout::KEYS_NUM * (Layout::KEYS_NUM - 1) / 2 - 2;
    pub fn costs() -> CostCache {
        let mut costs = [[0.0; Layout::KEYS_NUM]; Layout::KEYS_NUM];
        for i in Layout::KEYS {
            for j in Layout::KEYS {
                if i == j {
                    costs[*i as usize][*i as usize] = Layout::_chord_cost(&Chord(smallvec![*i]));
                } else {
                    costs[*i as usize][*j as usize] = Layout::_chord_cost(&Chord(smallvec![*i, *j]));
                }
            }
        }
        costs
    }
    pub fn init() -> Layout {
        let mut target_chars: Vec<Char> = (0..Layout::CHORD_NUM_POS)
            .map(|i| {
                if i < 26 {
                    Char::Char((i as u8 + 'a' as u8) as char)
                } else {
                    Char::Nothing(i as u8 - 26)
                }
            })
            .collect();
        Layout(
            (0..Layout::KEYS_NUM)
                .map(|i| smallvec![Layout::KEYS[i]])
                .chain((0..Layout::KEYS_NUM).flat_map(|i| {
                    ((i + 1)..Layout::KEYS_NUM)
                        .filter_map(|j| {
                            if i == 0 && j == 6 || i == 1 && j == 5 {
                                None
                            } else {
                                Some(smallvec![Layout::KEYS[i], Layout::KEYS[j]])
                            }
                        })
                        .collect::<Vec<_>>()
                }))
                .map(|v| {
                    let ind = rand::random::<usize>() % target_chars.len();
                    (target_chars.remove(ind), Chord(v))
                })
                .collect(),
        )
    }

    const fn cost(finger: u8) -> f64 {
        Layout::KEYS_COST[finger as usize]
    }

    fn _chord_cost(chord: &Chord) -> f64 {
        if chord.0.len() == 1 {
            Layout::cost(chord.0[0])
        } else {
            let (big, small) = if chord.0[0] > chord.0[1] {
                (chord.0[0], chord.0[1])
            } else {
                (chord.0[1], chord.0[0])
            };
            let (a, b) = (Layout::cost(big), Layout::cost(small));
            (a + b)
                * if big - 5 == small {
                    1.5
                } else if big >= 5 && small >= 5 || big < 5 && small < 5 {
                    1.0
                } else if small == 1 && big == 7 || big == 7 && small == 3 || big == 8 && small == 4 {
                    1.9
                } else {
                    1.3
                }
        }
    }

    fn chord_cost(chord: &Chord, costs: &CostCache) -> f64 {
        if chord.0.len() == 1 {
            Layout::cost(chord.0[0])
        } else {
            costs[chord.0[0] as usize][chord.0[1] as usize]
        }
    }

    fn consec_cost(old: &Chord, new: &Chord) -> f64 {
        let mut cost = 0.0;
        for l in &old.0 {
            for c in &new.0 {
                let (small, big) = if c < l {
                    (*c, *l)
                } else {
                    (*l, *c)
                };
                cost +=
                if small == big {
                    0.5 * Layout::cost(small)
                } else if small == big - 5 {
                    Layout::cost(small)
                } else if small == 0 && big == 6 || small == 1 && big == 5 {
                    (Layout::cost(1) + Layout::cost(5)) / 2.0 * 1.414
                } else if small == 0 && big == 1 || small == 5 && big == 6 {
                    (Layout::cost(1) + Layout::cost(5)) / 2.0
                } else {
                    0.0
                }
            }
        }
        cost
    }

    pub fn total_cost(&self, data: &FreqsData, costs: &CostCache) -> f64 {
        let mut cost = 0.0f64;
        for (s, count) in data.iter() {
            let mut last_chord: Option<&Chord> = None;
            cost += (*count as f64)
                * {
                    let mut cost = 0.0;
                    for c in s.chars() {
                        let chord = &self.0[&Char::Char(c.to_ascii_lowercase())];
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

    fn repr_chord(chord: Chord) -> String {
        let keys: BTreeSet<u8> = chord.0.into_iter().collect();
        [(0, 5), (1, 6), (2, 7), (3, 8), (4, 9)]
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

    fn repr_mapping(chord: Chord, c: char) -> String {
        format!("|{}|: {}", Layout::repr_chord(chord), c)
    }

    pub fn repr_layout(&self) -> String {
        let mut maps: Vec<(Chord, Char)> =
            self.0.clone().into_iter().map(|(a, b)| (b, a)).filter(|p| matches!(p.1, Char::Char(..))).collect();
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
                    .map(|p| Layout::repr_mapping(p.0.clone(), if let Char::Char(c) = p.1 { c } else { panic!() }))
                    .collect::<Vec<_>>();
                reprs.join(", ")
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn print(&self) {
        println!("{}", self.repr_layout());
    }
}
