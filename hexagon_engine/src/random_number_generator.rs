// a (de)-serializable rng

use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize, Default)]
pub struct RandomNumberGenerator {
    seed: u32,
    count: u32,
}

impl RandomNumberGenerator {
    pub fn new(seed: u32) -> RandomNumberGenerator {
        RandomNumberGenerator { seed, count: 0 }
    }

    pub fn next_f64(&mut self) -> f64 {
        let total = self.seed.wrapping_add(self.count);
        self.count = self.count.wrapping_add(1);
        let s = total.wrapping_add(0x6D2B79F5);
        let t = (s ^ (s >> 15)).wrapping_mul(1 | s);
        let t = t.wrapping_add((t ^ (t >> 7)).wrapping_mul(61 | t));
        (t ^ (t >> 14)) as f64 / 4294967296.0
    }

    /// This removes a random element from the list and returns it
    pub(crate) fn select_random_element<T>(&mut self, list: &mut Vec<T>) -> Option<T> {
        if list.is_empty() {
            return None;
        }
        let length_before = list.len();
        loop {
            let random = self.next_f64(); // this is a number between 0 and 1
            let index = (random * length_before as f64) as usize;
            if index < length_before {
                return Some(list.remove(index));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_rng() {
        let mut rng = RandomNumberGenerator::new(123);
        let mut bins = [0; 10];
        let count = 1_000_000;
        let expectation = (count as f32) / bins.len() as f32;
        for _ in 0..count {
            bins[(rng.next_f64() * 10.) as usize] += 1;
        }
        // dbg!(bins);
        bins.iter()
            .for_each(|&bin| assert!(((bin as f32) / expectation - 1.).abs() < 0.01));
    }
}
