// Taken from tiny rand crate, added serde
// [Wyrand](https://github.com/wangyi-fudan/wyhash/blob/master/Modern%20Non-Cryptographic%20Hash%20Function%20and%20Pseudorandom%20Number%20Generator.pdf) RNG.

use std::u16;

#[derive(Default, serde::Deserialize, serde::Serialize, Clone)]
pub struct Wyrand(u64);

impl Wyrand {
    #[inline(always)]
    fn next_u64(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0xA076_1D64_78BD_642F);
        let r = u128::from(self.0) * u128::from(self.0 ^ 0xE703_7ED1_A0B4_28DB);
        (r as u64) ^ (r >> 64) as u64
    }
}

impl Wyrand {
    #[inline(always)]
    fn seed(seed: u64) -> Self {
        Self(seed)
    }
}

#[derive(serde::Deserialize, serde::Serialize, Clone)]
pub(crate) struct Rand(Wyrand);

impl Rand {
    pub(crate) fn with_seed(seed: u64) -> Self {
        Self(Wyrand::seed(seed))
    }

    #[inline(always)]
    pub(crate) fn select_rand_element<T>(&mut self, possible_outer_connectors: &mut Vec<T>) -> T {
        let i = self.next_with_limit(possible_outer_connectors.len());
        possible_outer_connectors.remove(i)
    }

    pub(crate) fn get_tile(&mut self, used_tiles: &mut Vec<u16>) -> u16 {
        let max = crate::permutations::DIFFERENT_TILES_COUNT;
        let mut next =
            self.next_with_limit((max as usize).checked_sub(used_tiles.len()).unwrap()) as u16;
        for i in used_tiles.iter() {
            if *i < next {
                next += 1;
            }
        }
        used_tiles.push(next);
        next
    }

    #[inline(always)]
    fn next_u64(&mut self) -> u64 {
        self.0.next_u64()
    }

    #[inline(always)]
    pub fn next_with_limit(&mut self, limit: usize) -> usize {
        let limit = limit as u64;
        loop {
            let i = self.next_u64();
            let max = u64::MAX / limit * limit;
            if i < max {
                return (i % limit) as usize;
            }
        }
    }
}

impl std::fmt::Debug for Rand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Rand").finish()
    }
}
