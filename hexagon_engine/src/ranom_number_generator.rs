// a (de)-serializeable rng
pub struct RandomNumberGenerator {
    seed: usize,
    count: usize,
}

impl RandomNumberGenerator {
    pub fn new(seed: usize) -> RandomNumberGenerator {
        RandomNumberGenerator { seed, count: 0 }
    }

    pub fn next(&mut self) -> f64 {
        let total = self.seed.wrapping_add(self.count) as u32;
        self.count = self.count.wrapping_add(1);
        let s = total.wrapping_add(0x6D2B79F5);
        let t = (s ^ (s >> 15)).wrapping_mul(1 | s);
        let t = t.wrapping_add((t ^ (t >> 7)).wrapping_mul(61 | t));
        (t ^ (t >> 14)) as f64 / 4294967296.0
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
            bins[(rng.next() * 10.) as usize] += 1;
        }
        // dbg!(bins);
        bins.iter()
            .for_each(|&bin| assert!(((bin as f32) / expectation - 1.).abs() < 0.01));
    }
}
