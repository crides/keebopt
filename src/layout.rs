use core::fmt;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::io::Write;

use rand::prelude::SliceRandom;
use rand::thread_rng;
use rayon::prelude::*;

use crate::charmap::{FlatCharMap, RawFlatCharMap};

pub const CHORDS_IN_LINE: usize = 8;

pub type FreqDataItem = (String, usize);
pub type RawFreqData = Vec<FreqDataItem>;
pub type FreqData = BTreeMap<char, Vec<FreqDataItem>>;
pub type Key = (u8, u8);

fn to_freq_data(raw: &RawFreqData) -> FreqData {
    let mut map = BTreeMap::new();
    for (s, cnt) in raw.into_iter() {
        let mut deduped = s.chars().collect::<Vec<_>>();    // FIXME opt
        deduped.dedup();
        for c in deduped.into_iter() {
            map.entry(c).and_modify(|a: &mut Vec<_>| a.push((s.clone(), *cnt))).or_insert_with(|| vec![(s.clone(), *cnt)]);
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
pub struct CharMap(pub FlatCharMap<Chord>);

pub type CostCache = Vec<Vec<f64>>;

impl CharMap {
    pub fn new(layout: &Layout, chars: &[char]) -> CharMap {
        let mut map = FlatCharMap::new();
        for (i, c) in chars.iter().enumerate() {
            map.set(*c, layout.chords[i]);
        }
        CharMap(map)
    }

    /// No dups
    pub fn is_valid(&self) -> bool {
        self.0.iter().collect::<BTreeSet<_>>().len() == self.0.iter().count()
    }
}

struct LayoutScore {
    chars: RawFlatCharMap<f64>,
    total: f64,
}

#[derive(Debug, Clone, Deserialize)]
struct Finger {
    base: f64,
    keys: Vec<f64>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct PhysicalLayout {
    fingers: Vec<Finger>,
    #[serde(skip)]
    col_len: usize,
    #[serde(skip)]
    finger_len: usize,
    #[serde(skip)]
    costs_cache: CostCache,
}

impl PhysicalLayout {
    pub fn cache(&mut self) {
        self.col_len = self.fingers.iter().map(|f| f.keys.len()).max().unwrap();
        self.finger_len = self.fingers.len();
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
        self.costs_cache = costs;
    }

    pub fn key_to_ind(&self, k: Key) -> usize {
        k.0 as usize * self.col_len + k.1 as usize
    }

    pub fn num_keys(&self) -> usize {
        self.fingers.iter().map(|f| f.keys.len()).sum()
    }

    pub fn inds(&self) -> Vec<Key> {
        let mut ret = vec![(0, 0); self.finger_len * self.col_len];
        let mut ind = 0;
        for i in 0..self.finger_len {
            for j in 0..self.col_len {
                ret[ind] = (i as u8, j as u8);
                ind += 1;
            }
        }
        ret
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
            0.5 * (self.cost(k1) + self.cost(k2) + k1.1.abs_diff(k2.1) as f64)
        } else {
            0.0
        }
    }

    fn consec_cost(&self, last: &Chord, this: &Chord) -> f64 {
        match (last, this) {
            (&Chord::Key(a), &Chord::Key(b)) => self.overlap_key(a, b),
            (&Chord::Key(a), &Chord::Chord(b, c)) => self.overlap_key(a, b) + self.overlap_key(a, c),
            (&Chord::Chord(a, b), &Chord::Key(c)) => self.overlap_key(a, c) + self.overlap_key(b, c),
            (&Chord::Chord(a, b), &Chord::Chord(c, d)) => {
                self.overlap_key(a, c) + self.overlap_key(a, d) + self.overlap_key(b, c) + self.overlap_key(b, d)
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

    fn freq_item_cost(&self, map: &CharMap, s: &str, c: usize) -> f64 {
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

    pub fn total_cost(&self, map: &CharMap, data: &RawFreqData) -> f64 {
        data.iter().map(|(s, c)| self.freq_item_cost(map, s, *c)).sum()
    }

    pub fn layout_cost(&self, map: &CharMap, data: &RawFreqData) -> LayoutScore {
        let mut scores = RawFlatCharMap::new();
        let mut total = 0.0;
        // let mut bang_count = 0;
        for (s, count) in data.iter() {
            let score = self.freq_item_cost(map, s, *count);
            // println!("{}, {}", s, score);
            let mut deduped = s.chars().collect::<Vec<_>>();    // FIXME opt
            deduped.dedup();
            for c in deduped.into_iter() {
                scores[c] += score;
                // if c == 'a' {
                //     bang_count += 1;
                //     println!("lcost {} -> {}", s, score);
                // }
            }
            total += score;
            coz::progress!();
        }
        // println!("bang count {}", bang_count);
        LayoutScore { total, chars: scores }
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
        for (c, chord) in char_map.0.iter() {
            if let Chord::Key((i, j)) = chord {
                key_map[*i as usize][*j as usize] = c;
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
            // for i in (0..chord_map.len()).step_by(CHORDS_IN_LINE) {
            //     for row in 0..3 {
            //         for j in 0..(std::cmp::min(CHORDS_IN_LINE, chord_map.len() - i)) {
            //             let ind = i + j;
            //             let keys = chord_map[ind].1.to_keys();
            //             for finger in 0..self.phys.fingers.len() {
            //                 print!(
            //                     "{}",
            //                     if keys
            //                         .iter()
            //                         .find(|k| k.0 == finger as u8 && k.1 == row)
            //                         .is_some()
            //                     {
            //                         "#"
            //                     } else {
            //                         "."
            //                     }
            //                 );
            //             }
            //             if row == 1 {
            //                 print!(" = {} ", chord_map[ind].0);
            //             } else {
            //                 print!("     ");
            //             }
            //         }
            //         println!("");
            //     }
            //     println!("");
            // }
        }
    }
}

pub fn test(
    phys: &PhysicalLayout,
    chars: &[char],
    raw_freq_data: &RawFreqData,
) {
    let layout = &Layout::new(&phys);
    let freq_data = to_freq_data(&raw_freq_data);
    // let char_map = CharMap::new(layout, &chars);
    let mut char_map = CharMap(FlatCharMap::new());
    char_map.0.set('!', Chord::Key((0, 0)));
    char_map.0.set('"', Chord::Key((0, 1)));
    char_map.0.set('#', Chord::Key((0, 2)));
    char_map.0.set('$', Chord::Key((1, 0)));
    char_map.0.set('%', Chord::Key((1, 1)));
    char_map.0.set('&', Chord::Key((1, 2)));
    char_map.0.set('\'', Chord::Key((2, 0)));
    char_map.0.set('(', Chord::Key((2, 1)));
    char_map.0.set(')', Chord::Key((2, 2)));
    char_map.0.set('*', Chord::Key((3, 0)));
    char_map.0.set('+', Chord::Key((3, 1)));
    char_map.0.set(',', Chord::Key((3, 2)));
    char_map.0.set('-', Chord::Chord((0, 0), (0, 1)));
    char_map.0.set('.', Chord::Chord((0, 0), (1, 0)));
    char_map.0.set('/', Chord::Chord((0, 0), (1, 1)));
    char_map.0.set('0', Chord::Chord((0, 0), (1, 2)));
    char_map.0.set('1', Chord::Chord((0, 0), (2, 0)));
    char_map.0.set('2', Chord::Chord((0, 0), (2, 1)));
    char_map.0.set('3', Chord::Chord((0, 0), (2, 2)));
    char_map.0.set('4', Chord::Chord((0, 0), (3, 0)));
    char_map.0.set('5', Chord::Chord((0, 0), (3, 1)));
    char_map.0.set('6', Chord::Chord((0, 0), (3, 2)));
    char_map.0.set('7', Chord::Chord((0, 1), (0, 2)));
    char_map.0.set('8', Chord::Chord((0, 1), (1, 0)));
    char_map.0.set('9', Chord::Chord((0, 1), (1, 1)));
    char_map.0.set(':', Chord::Chord((0, 1), (1, 2)));
    char_map.0.set(';', Chord::Chord((0, 1), (2, 0)));
    char_map.0.set('<', Chord::Chord((0, 1), (2, 1)));
    char_map.0.set('=', Chord::Chord((0, 1), (2, 2)));
    char_map.0.set('>', Chord::Chord((0, 1), (3, 0)));
    char_map.0.set('?', Chord::Chord((0, 1), (3, 1)));
    char_map.0.set('@', Chord::Chord((0, 1), (3, 2)));
    char_map.0.set('[', Chord::Chord((0, 2), (1, 0)));
    char_map.0.set('\\', Chord::Chord((0, 2), (1, 1)));
    char_map.0.set(']', Chord::Chord((0, 2), (1, 2)));
    char_map.0.set('^', Chord::Chord((0, 2), (2, 0)));
    char_map.0.set('_', Chord::Chord((0, 2), (2, 1)));
    char_map.0.set('`', Chord::Chord((0, 2), (2, 2)));
    char_map.0.set('a', Chord::Chord((0, 2), (3, 0)));
    char_map.0.set('b', Chord::Chord((0, 2), (3, 1)));
    char_map.0.set('c', Chord::Chord((0, 2), (3, 2)));
    char_map.0.set('d', Chord::Chord((1, 0), (1, 1)));
    char_map.0.set('e', Chord::Chord((1, 0), (2, 0)));
    char_map.0.set('f', Chord::Chord((1, 0), (2, 1)));
    char_map.0.set('g', Chord::Chord((1, 0), (2, 2)));
    char_map.0.set('h', Chord::Chord((1, 0), (3, 0)));
    char_map.0.set('i', Chord::Chord((1, 0), (3, 1)));
    char_map.0.set('j', Chord::Chord((1, 0), (3, 2)));
    char_map.0.set('k', Chord::Chord((1, 1), (1, 2)));
    char_map.0.set('l', Chord::Chord((1, 1), (2, 0)));
    char_map.0.set('m', Chord::Chord((1, 1), (2, 1)));
    char_map.0.set('n', Chord::Chord((1, 1), (2, 2)));
    char_map.0.set('o', Chord::Chord((1, 1), (3, 0)));
    char_map.0.set('p', Chord::Chord((1, 1), (3, 1)));
    char_map.0.set('q', Chord::Chord((1, 1), (3, 2)));
    char_map.0.set('r', Chord::Chord((1, 2), (2, 0)));
    char_map.0.set('s', Chord::Chord((1, 2), (2, 1)));
    char_map.0.set('t', Chord::Chord((1, 2), (2, 2)));
    char_map.0.set('u', Chord::Chord((1, 2), (3, 0)));
    char_map.0.set('v', Chord::Chord((1, 2), (3, 1)));
    char_map.0.set('w', Chord::Chord((1, 2), (3, 2)));
    char_map.0.set('x', Chord::Chord((2, 0), (2, 1)));
    char_map.0.set('y', Chord::Chord((2, 0), (3, 0)));
    char_map.0.set('z', Chord::Chord((2, 0), (3, 1)));
    char_map.0.set('{', Chord::Chord((2, 0), (3, 2)));
    char_map.0.set('|', Chord::Chord((2, 1), (2, 2)));
    char_map.0.set('}', Chord::Chord((2, 1), (3, 0)));
    char_map.0.set('~', Chord::Chord((2, 1), (3, 1)));

    let cost = phys.layout_cost(&char_map, raw_freq_data);
    layout.print(&char_map);
    let rev_char_map: BTreeMap<Chord, char> = char_map
        .0
        .iter()
        .map(|(c, &chord)| (chord, c))
        .collect();
    let mut new_char_map = char_map.clone();
    let cchar = 'x';
    let (lc, cc) = (
        Chord::Key((2, 1)),
        new_char_map.0[cchar],
    );
    new_char_map.0[cchar] = lc;
    let new_cost_rell = if let Some(&lchar) = rev_char_map.get(&lc) {
        if lchar == cchar {
            return;
        }
        new_char_map.0[lchar] = cc;
        phys.total_cost(&new_char_map, &freq_data[&lchar]) - phys.total_cost(&char_map, &freq_data[&lchar])
    } else {
        0.0
    };

    let lchar = rev_char_map.get(&lc).copied().unwrap_or(' ');
    std::fs::write("dbg.json", serde_json::to_string(&vec![
        freq_data[&cchar].iter().map(|(s, c)| (s, phys.freq_item_cost(&char_map, s, *c))).collect::<Vec<_>>(),
        freq_data[&lchar].iter().map(|(s, c)| (s, phys.freq_item_cost(&char_map, s, *c))).collect::<Vec<_>>(),
        freq_data[&cchar].iter().map(|(s, c)| (s, phys.freq_item_cost(&new_char_map, s, *c))).collect::<Vec<_>>(),
        freq_data[&lchar].iter().map(|(s, c)| (s, phys.freq_item_cost(&new_char_map, s, *c))).collect::<Vec<_>>(),
    ]).unwrap()).unwrap();
    dbg!(cost.total);
    let new_cost_relc = phys.total_cost(&new_char_map, &freq_data[&cchar]) - phys.total_cost(&char_map, &freq_data[&cchar]);
    coz::progress!();
    let new_cost = phys.layout_cost(&new_char_map, raw_freq_data);
    for c in char_map.0.keys() {
        println!("{} -> {} - {} = {}", c, new_cost.chars[c], cost.chars[c], new_cost.chars[c] - cost.chars[c]);
    }
    println!("{} ({}: {}) <=> {} ({}: {}) -> {:.2} <=> {:.2} | {:.2}", cchar, cc, phys.chord_cost_cached(&cc), lchar, lc, phys.chord_cost_cached(&lc), new_cost_relc, new_cost_rell, new_cost.total);
    println!("{} | {} - {} = {}", new_cost_relc + new_cost_rell, new_cost.total, cost.total, new_cost.total - cost.total);
    // let both = raw_freq_data.iter().filter(|(s, _c)| s.contains('x') && s.contains('(')).cloned().collect::<Vec<_>>();
    // println!("both {} | {}", phys.total_cost(&char_map, &both), phys.total_cost(&new_char_map, &both));
}

pub fn optimize(
    phys: &PhysicalLayout,
    chars: &[char],
    raw_freq_data: &RawFreqData,
    rounds: usize,
) -> CharMap {
    let layout = &Layout::new(&phys);
    let freq_data = to_freq_data(&raw_freq_data);
    let mut char_map = CharMap::new(layout, &chars);
    layout.print(&char_map);
    // let mut cost = phys.layout_cost(&char_map, raw_freq_data);
    // println!("orig score: {}", cost.total);
    // println!("indiv diffs: {:?}", cost.chars.iter().filter_map(|(c, v)| freq_data.get(&c).map(|f| (c, v, phys.total_cost(&char_map, f)))).collect::<Vec<_>>());
    // println!("! rel {} {}", freq_data[&'!'].len(), freq_data[&'!'].iter().map(|(s, c)| s.as_str()).collect::<Vec<_>>().join(" "));
    // return char_map;
    let results = (0..rounds)
        .into_par_iter()
        .map(|_| {
            print!("+");
            std::io::stdout().flush().unwrap();
            let mut chars: Vec<char> = chars.to_vec();
            // chars.shuffle(&mut thread_rng());
            let mut char_map = CharMap::new(layout, &chars);
            layout.print(&char_map);
            let mut cost = phys.layout_cost(&char_map, raw_freq_data);
            println!("orig score: {}", cost.total);
            let mut i: usize = 0;
            loop {
                let rev_char_map: BTreeMap<Chord, char> = char_map
                    .0
                    .iter()
                    .map(|(c, &chord)| (chord, c))
                    .collect();
                let best = (0..layout.chords.len())
                    .flat_map(|li| (0..chars.len()).map(|i| (li, i)).collect::<Vec<_>>())
                    .filter_map(|(li, ci)| {
                        let mut new_char_map = char_map.clone();
                        let cchar = chars[ci];
                        let (lc, cc) = (
                            layout.chords[li],
                            new_char_map.0[cchar],
                        );
                        new_char_map.0[cchar] = lc;
                        let new_cost_rell = if let Some(&lchar) = rev_char_map.get(&lc) {
                            if lchar == cchar {
                                return None
                            }
                            new_char_map.0[lchar] = cc;
                            phys.total_cost(&new_char_map, &freq_data[&lchar]) - cost.chars[lchar]
                        } else {
                            0.0
                        };
                        let new_cost_relc = phys.total_cost(&new_char_map, &freq_data[&cchar]) - cost.chars[cchar];
                        coz::progress!();
                        let new_cost = phys.layout_cost(&new_char_map, &raw_freq_data);
                        let lchar = rev_char_map.get(&lc).copied().unwrap_or(' ');
                        assert!(new_cost_relc + new_cost_rell - (new_cost.total - cost.total) < 0.001, "\n{} ({}: {}) <=> {} ({}: {}) -> {:.2} <=> {:.2} | {:.2}\n{} | {} - {} = {}\n{:?}", cchar, cc, phys.chord_cost_cached(&cc), lchar, lc, phys.chord_cost_cached(&lc), new_cost_relc, new_cost_rell, new_cost.total, new_cost_relc + new_cost_rell, new_cost.total, cost.total, new_cost.total - cost.total, char_map.0.iter().collect::<BTreeMap<char, &Chord>>());
                        Some((new_char_map, new_cost_rell + new_cost_relc, new_cost, cchar, lchar))
                    })
                    .min_by(|a, b| a.1.total_cmp(&b.1))
                    .unwrap();
                let best_total = best.2;//phys.layout_cost(&best.0, raw_freq_data);
                println!("swap {} <=> {} |-> {}, {}, | {}", best.3, best.4, best.1, best_total.total, cost.total);
                coz::progress!();
                // match best_total.total.partial_cmp(&cost.total).unwrap() {
                match best_total.total.partial_cmp(&cost.total).unwrap() {
                    Ordering::Less => {
                        char_map = best.0;
                        cost = best_total;
                    }
                    Ordering::Greater | Ordering::Equal => {
                        print!("{}-", i);
                        std::io::stdout().flush().unwrap();
                        return (char_map, cost, i);
                    }
                }
                i += 1;
                print!(".");
                std::io::stdout().flush().unwrap();
            }
        })
        .collect::<Vec<_>>();
    let total_steps: usize = results.iter().map(|(_, _, step)| step).sum();
    let (best, best_cost, _) = results.into_iter().min_by(|(_m1, c1, _), (_m2, c2, _)| c1.total.total_cmp(&c2.total)).unwrap();
    let total_char_count: usize = raw_freq_data.iter().map(|(s, c)| s.len() * c).sum();
    println!("\nsteps: {}", total_steps);
    println!("cost: {}", best_cost.total / total_char_count as f64);
    layout.print(&best);
    return best;
}
