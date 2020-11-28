use rand::thread_rng;
use rand::Rng;
use std::f32;

// These values are taken from Carpalx, with T0 adjusted for the scale that our
// penalty model outputs.
const T0: f32 = 1.5;
const K: f32 = 10.0;
const P0: f32 = 1.0;
pub const N: usize = 20000;
const KN: f32 = K / (N as f32);

// T(i) = T0 exp(-ik/N)
fn temperature(i: usize) -> f32 {
    T0 * f32::exp(-(i as f32) * KN)
}

// p(dE, i) = p0 exp(-dE/T(i))
fn cutoff_p(de: f32, i: usize) -> f32 {
    let t = temperature(i);
    P0 * f32::exp(-de / t)
}

// For positive dE, accept if r < p_dE where r ~ Uniform(0, 1)
pub fn accept_transition(de: f32, i: usize) -> bool {
    if de < 0.0 {
        true
    } else {
        let p_de = cutoff_p(de, i);
        let r = thread_rng().gen::<f32>();
        r < p_de
    }
}
