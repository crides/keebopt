use std::collections::BTreeMap;
use std::io::Write;

use itertools::iproduct;
use rand::prelude::SliceRandom;
use rand::{thread_rng, Rng};
use rayon::prelude::*;

use crate::layout::{Chord, FreqData};
use crate::{ChordMap, CostMap, RawFreqData};

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

pub fn optimize(
    cost_map: &CostMap,
    chars: &[char],
    raw_freq_data: &RawFreqData,
    rounds: usize,
    annealed: bool,
) -> ChordMap {
    let chords = cost_map.chords();
    let freq_data = to_freq_data(&raw_freq_data);
    let total_char_count: usize = raw_freq_data.iter().map(|(s, c)| s.len() * c).sum();
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
            let mut time: usize = 0;
            loop {
                let rev_char_map: BTreeMap<Chord, char> =
                    char_map.0.iter().map(|(c, &chord)| (chord, c)).collect();
                let mut swaps = iproduct!(0..chords.len(), 0..chars.len()).filter_map(|(li, ci)| {
                    let mut new_char_map = char_map.clone();
                    let cchar = chars[ci];
                    let (lc, cc) = (chords[li], new_char_map.0[cchar]);
                    new_char_map.0[cchar] = lc;
                    let new_cost_rell = if let Some(&lchar) = rev_char_map.get(&lc) {
                        if lchar == cchar {
                            return None;
                        }
                        new_char_map.0[lchar] = cc;
                        cost_map.total_cost(&new_char_map, &freq_data[&lchar]) - cost.chars[lchar]
                    } else {
                        0.0
                    };
                    let new_cost_relc =
                        cost_map.total_cost(&new_char_map, &freq_data[&cchar]) - cost.chars[cchar];
                    let total_rel_cost = new_cost_rell + new_cost_relc;
                    Some((new_char_map, total_rel_cost))
                });
                let best = if annealed {
                    swaps.find(|(_, rel)| {
                        let norm_diff = rel / total_char_count as f64;
                        time += 1;
                        *rel < 0.0
                            || f64::exp(-norm_diff * 10.0 / f64::exp(-(time as f64) / 50000.0))
                                >= thread_rng().gen::<f64>()
                    })
                } else {
                    swaps
                        .min_by(|a, b| a.1.total_cmp(&b.1))
                        .filter(|(_, rel)| *rel < 0.0)
                };
                coz::progress!();
                if let Some(best) = best {
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
    println!("\nsteps: {}", total_steps);
    println!("cost: {}", best_cost.total / total_char_count as f64);
    best.print(cost_map);
    return best;
}
