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
    pub const KEYS_COST: &'static [f64] = &[1.0, 1.1, 1.4, 2.3, 1.0, 1.1, 1.4, 2.3];
    pub const KEYS_NUM: usize = Layout::KEYS_COST.len();
    pub const CHORD_NUM_POS: usize = Layout::KEYS_NUM + Layout::KEYS_NUM * (Layout::KEYS_NUM - 1) / 2;
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
                .map(|i| smallvec![i as u8])
                .chain((0..Layout::KEYS_NUM).flat_map(|i| {
                    ((i + 1)..Layout::KEYS_NUM)
                        .map(|j| {
                            smallvec![i as u8, j as u8]
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
                * if big - 4 == small {
                    1.5
                } else if big >= 4 && small >= 4 || big < 4 && small < 4 {
                    1.0
                } else if small == 0 && big == 5 || big == 5 && small == 2 || big == 6 && small == 3 {
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
                } else if small == big - 4 {
                    Layout::cost(small)
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
