use std::collections::BTreeMap;

use rand::prelude::*;
use smallvec::{smallvec, SmallVec};
use lazy_static::lazy_static;

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

struct Key {
    cost: f32,
    id: u8,
}

struct Finger {
    base_cost: f32,
    keys: Vec<Key>,
}

struct PhysicalLayout {
    fingers: Vec<Finger>,
}

lazy_static! {
    static ref PHYS_LAYOUT: PhysicalLayout = PhysicalLayout {
        fingers: vec![
            Finger {
                base_cost: 1.0,
                keys: vec![
                    Key {
                        cost: 1.1,
                        id: 0,
                    },
                    Key {
                        cost: 1.3,
                        id: 4,
                    },
                ],
            },
            Finger {
                base_cost: 1.1,
                keys: vec![
                    Key {
                        cost: 1.2,
                        id: 1,
                    },
                    Key {
                        cost: 1.0,
                        id: 5,
                    },
                    Key {
                        cost: 1.5,
                        id: 8,
                    },
                ],
            },
            Finger {
                base_cost: 1.4,
                keys: vec![
                    Key {
                        cost: 1.2,
                        id: 2,
                    },
                    Key {
                        cost: 1.0,
                        id: 6,
                    },
                    Key {
                        cost: 1.5,
                        id: 9,
                    },
                ],
            },
            Finger {
                base_cost: 2.3,
                keys: vec![
                    Key {
                        cost: 1.2,
                        id: 3,
                    },
                    Key {
                        cost: 1.4,
                        id: 7,
                    },
                ],
            },
        ],
    };
}

impl Layout {
    pub const KEYS_COST: &'static [f64] = &[
        1.0,
        1.1 * 1.2,
        1.4 * 1.2,
        2.3,
        1.0 * 1.2,
        1.1,
        1.4,
        2.3 * 1.2,
        1.1 * 1.5,
        1.4 * 1.5,
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
            if matches!((small, big), (0, 4) | (1, 5) | (2, 6) | (3, 7) | (5, 8) | (6, 9)) {
                cost *= 1.6;
            }
            // if big >= 4 && small >= 4 || big < 4 && small < 4 {
            // } else {
            //     cost *= 1.1;
            // }
            if matches!((small, big), (5, 7) | (1, 3)) {
                cost *= 1.2;
            }
            if matches!((small, big), (0, 5) | (2, 5) | (3, 6) | (0, 8) | (2, 8) | (3, 9)) {
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
        let fingers = vec![vec![0, 4], vec![1, 5, 8], vec![2, 6, 9], vec![3, 7]];
        let f = &fingers;
        let mut layout: Vec<_> = (0..Layout::KEYS_NUM)
            .map(|i| smallvec![i as u8])
            .chain((0..(fingers.len())).flat_map(move |i| {
                ((i+1)..f.len()).flat_map(move |j| {
                    let (a, b) = (&f[i], &f[j]);
                    (0..a.len()).flat_map(move |m| {
                        (0..b.len()).map(move |n| {
                            smallvec![a[m] as u8, b[n] as u8]
                        })
                    })
                })
            }))
            .chain([(0, 4), (1, 5), (5, 8), (2, 6), (6, 9), (3, 7)].iter().map(|&(a, b)| smallvec![a, b]))
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

    pub fn print(&self, char_map: &CharMap) {
        let rev_char_map: BTreeMap<usize, char> =
            char_map.0.iter().map(|(&c, &i)| (i, c)).collect();
        let char_num = rev_char_map.len();
        self
            .0
            .iter()
            .enumerate()
            .filter(|(i, _)| *i < char_num)
            .for_each(|(i, c)| {
                let target = rev_char_map[&i];
                if c.0.len() == 1 {
                    println!("{}: {}", i, target);
                } else {
                    println!("combo_{a}{b} {{ timeout-ms = <75>; key-positions = <{a} {b}>; bindings = <&kp {target}>; }};", a = c.0[0] + 1, b = c.0[1] + 1, target = target.to_uppercase());
                }
            });

    }
}
