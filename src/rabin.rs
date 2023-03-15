//! Source: https://github.com/rustic-rs/rustic/tree/main/src/cdc
//! Original source: https://github.com/green-coder/cdc

use cdchunking::ChunkerImpl;

// The irreducible polynomial to be used in the fingerprint function.
pub trait Polynomial {
    fn degree(&self) -> i32;
    fn modulo(self, m: Self) -> Self;
}

pub type Polynomial64 = u64;

impl Polynomial for Polynomial64 {
    // The degree of the polynom.
    fn degree(&self) -> i32 {
        63 - self.leading_zeros() as i32
    }

    fn modulo(self, m: Self) -> Self {
        let mut p = self;
        while p.degree() >= m.degree() {
            p ^= m << (p.degree() - m.degree());
        }

        p
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn polynom_degree() {
        assert_eq!(0u64.degree(), -1);
        assert_eq!(1u64.degree(), 0);

        assert_eq!(((1u64 << 7) - 1).degree(), 6);
        assert_eq!((1u64 << 7).degree(), 7);
        assert_eq!(((1u64 << 7) + 1).degree(), 7);
    }

    #[test]
    fn polynom_modulo() {
        assert_eq!(7u64.modulo(3), 1);
        assert_eq!(7u64.modulo(4), 3);
        assert_eq!(7u64.modulo(2), 1);

        assert_eq!(16u64.modulo(8), 0);
        assert_eq!(19u64.modulo(8), 3);

        assert_eq!(16u64.modulo(4), 0);
        assert_eq!(19u64.modulo(4), 3);
    }
}

#[derive(Clone, Debug)]
pub struct Rabin64 {
    // Configuration
    window_size: usize, // The size of the data window used in the hash calculation.

    // Precalculations
    polynom_shift: i32,
    out_table: [Polynomial64; 256],
    mod_table: [Polynomial64; 256],

    // Current state
    window_data: Vec<u8>,
    window_index: usize,
    pub hash: Polynomial64,
}

impl Rabin64 {
    pub fn calculate_out_table(
        window_size: usize,
        mod_polynom: Polynomial64,
    ) -> [Polynomial64; 256] {
        (0_u64..256)
            .map(|b| {
                let mut hash = (b as Polynomial64).modulo(mod_polynom);
                for _ in 0..window_size - 1 {
                    hash <<= 8;
                    hash = hash.modulo(mod_polynom);
                }
                hash
            })
            .collect::<Vec<_>>()
            .try_into()
            .unwrap()
    }

    pub fn calculate_mod_table(mod_polynom: Polynomial64) -> [Polynomial64; 256] {
        let k = mod_polynom.degree();

        (0_u64..256)
            .map(|b| {
                let p = (b as Polynomial64) << k;
                p.modulo(mod_polynom) | p
            })
            .collect::<Vec<_>>()
            .try_into()
            .unwrap()
    }

    pub fn new_with_polynom(window_size: usize, mod_polynom: Polynomial64) -> Rabin64 {
        let window_data = vec![0; window_size];

        Rabin64 {
            window_size,
            polynom_shift: mod_polynom.degree() - 8,
            out_table: Self::calculate_out_table(window_size, mod_polynom),
            mod_table: Self::calculate_mod_table(mod_polynom),
            window_data,
            window_index: 0,
            hash: 0,
        }
    }

    fn reset(&mut self) {
        self.window_index = 0;
        self.hash = 0;
        // We do not reset all window elements to zero.
        // This only works if we ingest the first window_size bytes using `eat`.
    }

    /// Only ingests the byte, does not remove the oldest byte from the window.
    /// This is useful after a reset.
    #[inline]
    fn eat(&mut self, byte: u8) {
        self.window_data[self.window_index] = byte;
        let mod_index = (self.hash >> self.polynom_shift) & 255;
        self.hash <<= 8;
        self.hash |= u64::from(byte);
        self.hash ^= self.mod_table[mod_index as usize];

        // Move the windowIndex to the next position.
        self.window_index = (self.window_index + 1) % self.window_size;
    }

    #[inline]
    fn slide(&mut self, byte: u8) {
        // Take the old value out of the window and the hash.
        let out_value = self.window_data[self.window_index];
        self.hash ^= self.out_table[out_value as usize];

        // Put the new value in the window and in the hash.
        self.eat(byte)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Rabin {
    inner: Rabin64,
    pos: usize,
    mask: Polynomial64,
}

impl Rabin {
    pub(crate) fn new(window_size: usize, mask: u64) -> Rabin {
        Rabin {
            // This is the default polynom from https://github.com/fd0/rabin-cdc/blob/master/rabin.h
            // There's also some discussion here: https://github.com/lemire/rollinghashcpp/issues/6
            inner: Rabin64::new_with_polynom(window_size, 0x3DA3358B4DC173),
            pos: 0,
            mask,
        }
    }
}

impl ChunkerImpl for Rabin {
    fn find_boundary(&mut self, data: &[u8]) -> Option<usize> {
        for (i, &b) in data.iter().enumerate() {
            if self.pos < self.inner.window_size {
                // Prefill window
                self.inner.eat(b)
            } else {
                // Calculate rolling hash
                self.inner.slide(b);
                if self.inner.hash & self.mask == 0 {
                    return Some(i);
                }
            }
            self.pos += 1;
        }

        None
    }

    fn reset(&mut self) {
        self.inner.reset();
        self.pos = 0;
    }
}
