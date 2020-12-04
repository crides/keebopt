#![allow(dead_code)]

use std::collections::BTreeMap;

pub type FreqsData = Vec<(String, usize)>;

#[derive(Clone, Debug)]
pub struct Layout(pub BTreeMap<u8, char>);

#[derive(Clone, Debug)]
pub struct ReverseLayout(pub BTreeMap<char, u8>);

pub struct Chord(pub Vec<u8>);

impl Layout {
    pub fn shuffle(&mut self, n: usize) {
        let mut maps: Vec<(u8, char)> = self.0.clone().into_iter().collect();
        for _ in 0..n {
            loop {  // TODO
                let (i, j) = (
                    rand::random::<usize>() % maps.len(),
                    rand::random::<usize>() % (maps.len() - 1),
                );
                let j = if j >= i { j + 1 } else { j };
                let (mut a, mut b) = (maps[i], maps[j]);
                if a.1 == ' ' && b.1 == ' ' {
                    continue;
                }
                std::mem::swap(&mut a.1, &mut b.1);
                maps[i] = a;
                maps[j] = b;
                break;
            }
        }
        self.0 = maps.into_iter().collect();
    }

    fn _cost(finger: u8) -> f64 {
        match finger {
            8 => 1.0,
            4 => 1.1,
            2 => 1.4,
            1 => 2.3,
            0 => 0.0,
            _ => panic!("{:02x}", finger),
        }
    }

    fn cost(finger: u8) -> f64 {
        Layout::_cost(finger & 0xF) + Layout::_cost(finger >> 4)
    }

    fn chord_cost(chord: u8) -> f64 {
        if chord.count_ones() == 1 {
            Layout::cost(chord)
        } else {
            let top = ((chord as usize).next_power_of_two() / 2) as u8;
            let bottom = chord & !top;
            let (a, b) = (Layout::cost(top), Layout::cost(bottom));
            (a + b)
                * if top >> 4 == bottom {
                    1.5
                } else if top >= 0x10 && bottom >= 0x10 || top < 0x10 && bottom < 0x10 {
                    1.2
                } else if top == 8 && bottom == 4
                    || bottom == 4 && top == 2
                    || bottom == 2 && top == 1
                {
                    1.7
                } else {
                    1.3
                }
        }
    }

    pub fn total_cost(&self, data: &FreqsData) -> f64 {
        let char_map: BTreeMap<char, u8> = self.0.iter().map(|(k, v)| (*v, *k)).collect();
        let mut cost = 0.0f64;
        for (s, count) in data.iter() {
            let mut last_chord = None;
            cost += (*count as f64)
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
                    .sum::<f64>();
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

    fn repr_mapping(chord: u8, c: char) -> String {
        format!("|{}|: {}", Layout::repr_chord(chord), c)
    }

    pub fn print(&self) {
        let mut maps: Vec<(u8, char)> = self.0.clone().into_iter().filter(|p| p.1 != ' ').collect();
        maps.sort_by_key(|e| {
            (
                e.0.count_ones(),
                -((e.0 as usize).next_power_of_two() as isize),
            )
        });
        for chunk in maps.chunks(8) {
            let reprs = chunk.iter().map(|p| Layout::repr_mapping(p.0, p.1)).collect::<Vec<_>>();
            println!("{}", reprs.join(", "));
        }
    }
}
