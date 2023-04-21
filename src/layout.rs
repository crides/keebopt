use core::fmt;
use std::collections::{BTreeMap, BTreeSet};
use std::io::Write;

use rand::prelude::SliceRandom;
use rand::thread_rng;
use rayon::prelude::*;
use itertools::iproduct;

use crate::charmap::{CharMap, RawCharMap};

pub type FreqDataItem = (String, usize);
pub type RawFreqData = Vec<FreqDataItem>;
pub type FreqData = BTreeMap<char, Vec<FreqDataItem>>;
pub type Key = (u8, u8);

fn to_freq_data(raw: &RawFreqData) -> FreqData {
    let mut map = BTreeMap::new();
    for (s, cnt) in raw.into_iter() {
        let mut deduped = s.chars().collect::<Vec<_>>(); // FIXME opt
        deduped.dedup();
        for c in deduped.into_iter() {
            map.entry(c)
                .and_modify(|a: &mut Vec<_>| a.push((s.clone(), *cnt)))
                .or_insert_with(|| vec![(s.clone(), *cnt)]);
        }
    }
    map
}

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

#[derive(Clone, Debug)]
pub struct ChordMap(pub CharMap<Chord>);

pub type CostCache = Vec<Vec<f64>>;

impl ChordMap {
    pub fn new(chars: &[char], chords: &[Chord]) -> ChordMap {
        assert!(chars.len() <= chords.len(), "Too few chords! chars: {} vs. chords: {}", chars.len(), chords.len());
        let mut map = CharMap::new();
        for (c, chord) in chars.iter().copied().zip(chords.iter().copied()) {
            map.set(c, chord);
        }
        ChordMap(map)
    }

    /// No dups
    pub fn is_valid(&self) -> bool {
        self.0.iter().collect::<BTreeSet<_>>().len() == self.0.iter().count()
    }

    pub fn from_map(inp: &BTreeMap<char, Chord>) -> Self {
        let mut map = CharMap::new();
        for (&k, &v) in inp.iter() {
            map[k] = v;
        }
        Self(map)
    }

    pub fn to_map(&self) -> BTreeMap<char, Chord> {
        self.0.iter().map(|(c, &ch)| (c, ch)).collect()
    }

    pub fn print(&self, cost_map: &CostMap) {
        let mut key_map =
            vec![vec![' '; cost_map.fingers[0].keys.len()]; cost_map.fingers.len()];
        for (c, chord) in self.0.iter() {
            if let Chord::Key((i, j)) = chord {
                key_map[*i as usize][*j as usize] = c;
            }
        }
        let mut chord_map: Vec<_> = self
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
            let chords_in_line = 14;
            for i in (0..chord_map.len()).step_by(chords_in_line) {
                for row in 0..3 {
                    for j in 0..(std::cmp::min(chords_in_line, chord_map.len() - i)) {
                        let ind = i + j;
                        let keys = chord_map[ind].1.to_keys();
                        for finger in 0..cost_map.fingers.len() {
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

struct LayoutScore {
    chars: RawCharMap<f64>,
    total: f64,
}

#[derive(Debug, Clone, Deserialize)]
struct Finger {
    base: f64,
    keys: Vec<f64>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct CostMap {
    fingers: Vec<Finger>,
    #[serde(skip)]
    col_len: usize,
    #[serde(skip)]
    finger_len: usize,
    #[serde(skip)]
    costs_cache: CostCache,
}

impl CostMap {
    pub fn chords(&self) -> Vec<Chord> {
        let mut chords = Vec::new();
        for ind in self.inds() {
            chords.push(Chord::Key(ind));
        }
        for (i, fst) in self.inds().enumerate() {
            for snd in self.inds().skip(i + 1) {
                if fst.0 != snd.0 || fst.1.abs_diff(snd.1) == 1 {
                    chords.push(Chord::Chord(fst, snd));
                }
            }
        }
        dbg!(chords.len());
        chords
    }

    pub fn cache(&mut self) {
        self.col_len = self.fingers.iter().map(|f| f.keys.len()).max().unwrap();
        self.finger_len = self.fingers.len();
        let num_keys = self.num_keys();
        let mut costs = vec![vec![0.0; num_keys]; num_keys];
        for i in self.inds() {
            for j in self.inds() {
                let (ii, ji) = (self.key_to_ind(i), self.key_to_ind(j));
                if i == j {
                    costs[ii][ii] = self.chord_cost(&Chord::Key(i));
                } else {
                    costs[ii][ji] = self.chord_cost(&Chord::Chord(i, j));
                }
            }
        }
        self.costs_cache = costs;
    }

    pub const fn key_to_ind(&self, k: Key) -> usize {
        k.0 as usize * self.col_len + k.1 as usize
    }

    pub fn num_keys(&self) -> usize {
        self.fingers.iter().map(|f| f.keys.len()).sum()
    }

    pub fn inds(&self) -> impl Iterator<Item = Key> {
        iproduct!(0..self.finger_len as u8, 0..self.col_len as u8)
    }

    pub fn cost(&self, key: Key) -> f64 {
        let finger = &self.fingers[key.0 as usize];
        finger.keys[key.1 as usize] * finger.base
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

    fn overlap_key(&self, k1: Key, k2: Key) -> f64 {
        if k1.0 == k2.0 {
            0.5 * (self.chord_cost_cached(&Chord::Key(k1)) + self.chord_cost_cached(&Chord::Key(k2)) + k1.1.abs_diff(k2.1) as f64)
        } else {
            0.0
        }
    }

    fn consec_cost(&self, last: &Chord, this: &Chord) -> f64 {
        match (last, this) {
            (&Chord::Key(a), &Chord::Key(b)) => self.overlap_key(a, b),
            (&Chord::Key(a), &Chord::Chord(b, c)) => {
                self.overlap_key(a, b) + self.overlap_key(a, c)
            }
            (&Chord::Chord(a, b), &Chord::Key(c)) => {
                self.overlap_key(a, c) + self.overlap_key(b, c)
            }
            (&Chord::Chord(a, b), &Chord::Chord(c, d)) => {
                self.overlap_key(a, c)
                    + self.overlap_key(a, d)
                    + self.overlap_key(b, c)
                    + self.overlap_key(b, d)
            }
        }
    }

    fn chord_cost_cached(&self, chord: &Chord) -> f64 {
        match chord {
            Chord::Key(k) => {
                let ind = self.key_to_ind(*k);
                self.costs_cache[ind][ind]
            }
            Chord::Chord(k1, k2) => self.costs_cache[self.key_to_ind(*k1)][self.key_to_ind(*k2)],
        }
    }

    fn freq_item_cost(&self, map: &ChordMap, s: &str, c: usize) -> f64 {
        (c as f64) * {
            let mut cost = 0.0;
            let mut last_chord: Option<&Chord> = None;
            for c in s.chars() {
                let chord = &map.0[c];
                cost += self.chord_cost_cached(chord);
                if let Some(last) = &last_chord {
                    cost += self.consec_cost(last, chord);
                }
                last_chord = Some(chord);
                coz::progress!();
            }
            cost
        }
    }

    pub fn total_cost(&self, map: &ChordMap, data: &RawFreqData) -> f64 {
        data.iter()
            .map(|(s, c)| self.freq_item_cost(map, s, *c))
            .sum()
    }

    fn layout_cost(&self, map: &ChordMap, data: &RawFreqData) -> LayoutScore {
        let mut scores = RawCharMap::new();
        let mut total = 0.0;
        for (s, count) in data.iter() {
            let score = self.freq_item_cost(map, s, *count);
            let mut deduped = s.chars().collect::<Vec<_>>(); // FIXME opt
            deduped.dedup();
            for c in deduped.into_iter() {
                scores[c] += score;
            }
            total += score;
            coz::progress!();
        }
        LayoutScore {
            total,
            chars: scores,
        }
    }
}

pub fn optimize(
    cost_map: &CostMap,
    chars: &[char],
    raw_freq_data: &RawFreqData,
    rounds: usize,
) -> ChordMap {
    let chords = cost_map.chords();
    let freq_data = to_freq_data(&raw_freq_data);
    let results = (0..rounds)
        .into_par_iter()
        .map(|_| {
            print!("+");
            std::io::stdout().flush().unwrap();
            let mut chords = chords.clone();
            chords.shuffle(&mut thread_rng());
            let mut char_map = ChordMap::new(chars, &chords);
            let mut cost = cost_map.layout_cost(&char_map, raw_freq_data);
            let mut i: usize = 0;
            loop {
                let rev_char_map: BTreeMap<Chord, char> =
                    char_map.0.iter().map(|(c, &chord)| (chord, c)).collect();
                let best = iproduct!(0..chords.len(), 0..chars.len())
                    .filter_map(|(li, ci)| {
                        let mut new_char_map = char_map.clone();
                        let cchar = chars[ci];
                        let (lc, cc) = (chords[li], new_char_map.0[cchar]);
                        new_char_map.0[cchar] = lc;
                        let new_cost_rell = if let Some(&lchar) = rev_char_map.get(&lc) {
                            if lchar == cchar {
                                return None;
                            }
                            new_char_map.0[lchar] = cc;
                            cost_map.total_cost(&new_char_map, &freq_data[&lchar])
                                - cost.chars[lchar]
                        } else {
                            0.0
                        };
                        let new_cost_relc = cost_map.total_cost(&new_char_map, &freq_data[&cchar])
                            - cost.chars[cchar];
                        coz::progress!();
                        Some((new_char_map, new_cost_rell + new_cost_relc))
                    })
                    .min_by(|a, b| a.1.total_cmp(&b.1))
                    .unwrap();
                coz::progress!();
                if best.1 < 0.0 {
                    char_map = best.0;
                    cost = cost_map.layout_cost(&char_map, &raw_freq_data);
                } else {
                    print!("{}-", i);
                    std::io::stdout().flush().unwrap();
                    return (char_map, cost, i);
                }
                i += 1;
            }
        })
        .collect::<Vec<_>>();
    let total_steps: usize = results.iter().map(|(_, _, step)| step).sum();
    let (best, best_cost, _) = results
        .into_iter()
        .min_by(|(_m1, c1, _), (_m2, c2, _)| c1.total.total_cmp(&c2.total))
        .unwrap();
    let total_char_count: usize = raw_freq_data.iter().map(|(s, c)| s.len() * c).sum();
    println!("\nsteps: {}", total_steps);
    println!("cost: {}", best_cost.total / total_char_count as f64);
    best.print(cost_map);
    return best;
}
