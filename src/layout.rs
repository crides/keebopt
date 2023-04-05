use core::fmt;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::io::Write;

use rand::prelude::SliceRandom;
use rand::thread_rng;
use rayon::prelude::*;

pub const CHORDS_IN_LINE: usize = 8;

pub type RawFreqsData = Vec<(Vec<String>, usize)>;
pub type FreqsData = Vec<(Vec<Chord>, usize)>;
pub type Key = (u8, u8);

#[derive(Copy, Clone, Debug, Ord, PartialOrd, PartialEq, Eq, Serialize, Deserialize)]
pub enum Chord {
    Key(Key),
    Chord(Key, Key),
}

impl Chord {
    fn to_keys(&self) -> Vec<Key> {
        match self {
            Chord::Key(k) => vec![*k],
            Chord::Chord(k1, k2) => vec![*k1, *k2],
        }
    }
}

impl std::fmt::Display for Chord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Chord::Key((f1, n1)) => f.write_fmt(format_args!("{}.{}", f1, n1)),
            Chord::Chord((f1, n1), (f2, n2)) => {
                f.write_fmt(format_args!("{}.{}+{}.{}", f1, n1, f2, n2))
            }
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CharMap(pub BTreeMap<char, Chord>);

pub type CostCache = Vec<Vec<f64>>;

impl CharMap {
    pub fn new(layout: &Layout, chars: &[char]) -> CharMap {
        CharMap(
            chars
                .iter()
                .enumerate()
                .map(|(i, c)| (*c, layout.chords[i]))
                .collect(),
        )
    }

    pub fn transform_freqs_data(&self, data: &RawFreqsData) -> FreqsData {
        data.iter()
            .map(|(k, v)| {
                (
                    k.iter().map(|c| self.0[&c.chars().next().unwrap().to_ascii_lowercase()]).collect(),
                    *v,
                )
            })
            .collect()
    }

    pub fn cost(
        &self,
        data: &RawFreqsData,
        phys: &PhysicalLayout,
        costs: &CostCache,
    ) -> f64 {
        let freqs_data = self.transform_freqs_data(&data);
        phys.total_cost(&freqs_data, &costs)
    }

    pub fn is_valid(&self) -> bool {
        self.0.len() == self.0.values().collect::<BTreeSet<_>>().len()
    }
}

#[derive(Debug, Clone, Deserialize)]
struct Finger {
    base: f32,
    keys: Vec<f32>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct PhysicalLayout {
    fingers: Vec<Finger>,
}

impl PhysicalLayout {
    pub fn key_to_ind(&self, k: Key) -> usize {
        k.0 as usize * self.fingers[0].keys.len() + k.1 as usize
    }

    pub fn num_keys(&self) -> usize {
        self.fingers.iter().map(|f| f.keys.len()).sum()
    }

    pub fn inds(&self) -> Vec<Key> {
        self.fingers
            .iter()
            .enumerate()
            .flat_map(|(i, finger)| (0..finger.keys.len()).map(move |j| (i as u8, j as u8)))
            .collect()
    }

    pub fn cost(&self, key: Key) -> f64 {
        let finger = &self.fingers[key.0 as usize];
        finger.keys[key.1 as usize] as f64 * finger.base as f64
    }

    pub fn costs_cache(&self) -> CostCache {
        let num_keys = self.num_keys();
        let mut costs = vec![vec![0.0; num_keys]; num_keys];
        let inds = self.inds();
        for i in &inds {
            for j in &inds {
                let (ii, ji) = (self.key_to_ind(*i), self.key_to_ind(*j));
                if i == j {
                    costs[ii][ii] = self.chord_cost(&Chord::Key(*i));
                } else {
                    costs[ii][ji] = self.chord_cost(&Chord::Chord(*i, *j));
                }
            }
        }
        costs
    }

    fn chord_cost(&self, chord: &Chord) -> f64 {
        match chord {
            Chord::Key(k) => self.cost(*k),
            Chord::Chord(k1, k2) => {
                let base = self.cost(*k1) + self.cost(*k2);
                let vdiff = k1.1.abs_diff(k2.1);
                let hdiff = k1.0.abs_diff(k2.0);
                base * (if k1.0 == 1 && k2.0 == 3 { 1.5 } else { 1.0 })
                    * (1.0
                        + (if vdiff == 0 {
                            0.0
                        } else if vdiff == 1 {
                            0.2
                        } else {
                            1.0
                        }) * (1.0 - hdiff as f64 * 0.1))
            }
        }
    }

    fn consec_cost(&self, last: &Chord, this: &Chord) -> f64 {
        let overlap_key = |k1: Key, k2: Key| {
            if k1.0 == k2.0 {
                0.5 * (self.cost(k1) + self.cost(k2) + k1.1.abs_diff(k2.1) as f64)
            } else {
                0.0
            }
        };
        match (last, this) {
            (&Chord::Key(a), &Chord::Key(b)) => overlap_key(a, b),
            (&Chord::Key(a), &Chord::Chord(b, c)) => overlap_key(a, b) + overlap_key(a, c),
            (&Chord::Chord(a, b), &Chord::Key(c)) => overlap_key(a, c) + overlap_key(b, c),
            (&Chord::Chord(a, b), &Chord::Chord(c, d)) => {
                overlap_key(a, c) + overlap_key(a, d) + overlap_key(b, c) + overlap_key(b, d)
            }
        }
    }

    fn chord_cost_cached(&self, chord: &Chord, costs: &CostCache) -> f64 {
        match chord {
            Chord::Key(k) => {
                let ind = self.key_to_ind(*k);
                costs[ind][ind]
            }
            Chord::Chord(k1, k2) => costs[self.key_to_ind(*k1)][self.key_to_ind(*k2)],
        }
    }

    pub fn total_cost(&self, data: &FreqsData, costs: &CostCache) -> f64 {
        let mut cost = 0.0f64;
        for (s, count) in data.iter() {
            cost += (*count as f64) * {
                let mut cost = 0.0;
                let mut last_chord: Option<Chord> = None;
                for &c in s {
                    cost += self.chord_cost_cached(&c, costs);
                    if let Some(last) = &last_chord {
                        cost += self.consec_cost(last, &c);
                    }
                    last_chord = Some(c);
                }
                cost
            };
        }
        cost
    }
}

#[derive(Debug, Clone)]
pub struct Layout<'p> {
    chords: Vec<Chord>,
    phys: &'p PhysicalLayout,
}

impl<'p> Layout<'p> {
    pub fn new(phys: &'p PhysicalLayout) -> Layout {
        let inds = phys.inds();
        let mut chords = Vec::new();
        for ind in &inds {
            chords.push(Chord::Key(*ind));
        }
        for (i, fst) in inds.iter().enumerate() {
            for snd in &inds[i + 1..] {
                if fst.0 != snd.0 || fst.1.abs_diff(snd.1) == 1 {
                    chords.push(Chord::Chord(*fst, *snd));
                }
            }
        }
        dbg!(chords.len());
        Layout { chords, phys }
    }

    pub fn print(&self, char_map: &CharMap) {
        let mut key_map = vec![vec![' '; self.phys.fingers[0].keys.len()]; self.phys.fingers.len()];
        for (&c, &chord) in char_map.0.iter() {
            if let Chord::Key((i, j)) = chord {
                key_map[i as usize][j as usize] = c;
            }
        }
        let mut chord_map: Vec<_> = char_map
            .0
            .iter()
            .filter(|(_c, chord)| matches!(chord, Chord::Chord(..)))
            .collect();
        chord_map.sort_by_key(|(_, c)| match c {
            Chord::Chord(k1, k2) => (k1.0.abs_diff(k2.0), k1.1.abs_diff(k2.1)),
            _ => unreachable!(),
        });
        {
            for i in 0..3 {
                for finger in &key_map {
                    print!(" {} ", finger[i]);
                }
                println!("");
            }
            println!("");
            for i in (0..chord_map.len()).step_by(CHORDS_IN_LINE) {
                for row in 0..3 {
                    for j in 0..(std::cmp::min(CHORDS_IN_LINE, chord_map.len() - i)) {
                        let ind = i + j;
                        let keys = chord_map[ind].1.to_keys();
                        for finger in 0..self.phys.fingers.len() {
                            print!(
                                "{}",
                                if keys
                                    .iter()
                                    .find(|k| k.0 == finger as u8 && k.1 == row)
                                    .is_some()
                                {
                                    "#"
                                } else {
                                    "."
                                }
                            );
                        }
                        if row == 1 {
                            print!(" = {} ", chord_map[ind].0);
                        } else {
                            print!("     ");
                        }
                    }
                    println!("");
                }
                println!("");
            }
        }
    }
}

pub fn optimize(
    phys: &PhysicalLayout,
    chars: &[char],
    freqs_data: &RawFreqsData,
    rounds: usize,
) -> CharMap {
    let costs_cache = phys.costs_cache();
    let layout = Layout::new(&phys);
    let (best, best_cost) = (0..rounds).into_par_iter().map(|_| {
        print!("+");
        std::io::stdout().flush().unwrap();
        let mut chars: Vec<char> = chars.to_vec();
        chars.shuffle(&mut thread_rng());
        let mut char_map = CharMap::new(&layout, &chars);
        let mut cost = char_map.cost(freqs_data, &phys, &costs_cache);
        let mut i = 0;
        loop {
            let best = (0..layout.chords.len())
                .flat_map(|li| (0..char_map.0.len()).map(|j| (li, j)).collect::<Vec<_>>())
                .map(|(li, mi)| {
                    let mut new_char_map = char_map.clone();
                    let (lc, mc) = (layout.chords[li], new_char_map.0[&chars[mi]]);
                    if let Some(key) =
                        new_char_map
                            .0
                            .iter()
                            .find_map(|(key, c)| if *c == lc { Some(*key) } else { None })
                    {
                        new_char_map.0.insert(key, mc);
                    }
                    new_char_map.0.insert(chars[mi], lc);
                    let new_cost = new_char_map.cost(freqs_data, &phys, &costs_cache);
                    (new_char_map, new_cost)
                })
                .min_by(|a, b| a.1.total_cmp(&b.1))
                .unwrap();
            match best.1.partial_cmp(&cost).unwrap() {
                Ordering::Less => {
                    (char_map, cost) = best;
                }
                Ordering::Greater | Ordering::Equal => {
                    print!("{}-", i);
                    std::io::stdout().flush().unwrap();
                    return (char_map, cost)
                }
            }
            i += 1;
            // print!(".");
            // std::io::stdout().flush().unwrap();
        }
    }).min_by(|(_m1, c1), (_m2, c2)| c1.total_cmp(c2)).unwrap();
    println!("\ncost: {}", best_cost);
    layout.print(&best);
    return best;
}
