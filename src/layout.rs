use std::collections::{BTreeMap, BTreeSet};

use smallvec::{smallvec, SmallVec};

pub type FreqsData = Vec<(String, usize)>;

#[derive(Clone, Debug)]
pub struct Layout(pub BTreeMap<Chord, char>);

type ChordStore = SmallVec<[u8; 4]>;

#[derive(Clone, Debug, Ord, PartialOrd, PartialEq, Eq)]
pub struct Chord(pub ChordStore);

impl Layout {
    pub const KEYS: &'static [u8] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    pub const KEYS_COST: &'static [f64] = &[3.2, 1.0, 1.1, 1.4, 2.3, 3.2, 1.0, 1.1, 1.4, 2.3];
    pub const KEYS_NUM: usize = Layout::KEYS.len();
    pub const CHORD_NUM_POS: usize = Layout::KEYS_NUM + Layout::KEYS_NUM * (Layout::KEYS_NUM - 1) / 2 - 2;
    pub fn init() -> Layout {
        let mut target_chars: Vec<char> = (0..Layout::CHORD_NUM_POS)
            .map(|i| {
                if i < 26 {
                    (i as u8 + 'a' as u8) as char
                } else {
                    ' '
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
                    (Chord(v), target_chars.remove(ind))
                })
                .collect(),
        )
    }

    fn cost(finger: u8) -> f64 {
        Layout::KEYS_COST[finger as usize]
    }

    fn chord_cost(chord: &Chord) -> f64 {
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
                } else if small == 1 && big == 7 || big == 7 && small == 3 || big == 8 && small == 4
                {
                    1.9
                } else {
                    1.3
                }
        }
    }

    fn consec_cost(old: &Chord, new: &Chord) -> f64 {
        old.0
            .iter()
            .map(|l| {
                new.0
                    .iter()
                    .map(|c| {
                        let (small, big) = if c < l {
                            (*c, *l)
                        } else {
                            (*l, *c)
                        };
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
                    })
                    .sum::<f64>()
            })
            .sum()
    }

    pub fn total_cost(&self, data: &FreqsData) -> f64 {
        let char_map: BTreeMap<char, Chord> = self.0.iter().map(|(k, v)| (*v, k.clone())).collect();
        let mut cost = 0.0f64;
        for (s, count) in data.iter() {
            let mut last_chord: Option<Chord> = None;
            cost += (*count as f64)
                * s.chars()
                    .map(|c| {
                        let chord = &char_map[&c.to_ascii_lowercase()];
                        let mut cost = Layout::chord_cost(chord);
                        if let Some(last) = &last_chord {
                            cost += Layout::consec_cost(last, chord);
                        }
                        last_chord = Some(chord.clone());
                        cost
                    })
                    .sum::<f64>();
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
        let mut maps: Vec<(Chord, char)> =
            self.0.clone().into_iter().filter(|p| p.1 != ' ').collect();
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
                    .map(|p| Layout::repr_mapping(p.0.clone(), p.1))
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
