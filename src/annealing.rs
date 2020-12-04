use rand::thread_rng;
use rand::Rng;
use std::f64;

// These values are taken from Carpalx, with T0 adjusted for the scale that our
// penalty model outputs.
const T0: f64 = 3.0;
const K: f64 = 3.5;
const P0: f64 = 1.0;
pub const N: usize = 10_000;
const KN: f64 = K / (N as f64);

// T(i) = T0 exp(-ik/N)
fn temperature(i: usize) -> f64 {
    T0 * f64::exp(-(i as f64) * KN)
}

// p(dE, i) = p0 exp(-dE/T(i))
fn cutoff_p(de: f64, i: usize) -> f64 {
    let t = temperature(i);
    P0 * f64::exp(-de / t)
}

// For positive dE, accept if r < p_dE where r ~ Uniform(0, 1)
pub fn accept_transition(de: f64, i: usize) -> bool {
    if de < 0.0 {
        true
    } else {
        let de = de / 500000000000.0;
        let p_de = cutoff_p(de, i);
        // println!("{}, {}", de, p_de);
        let r = thread_rng().gen::<f64>();
        r < p_de
    }
}
