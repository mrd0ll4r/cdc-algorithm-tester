use cdchunking::ChunkerImpl;
use std::fs::File;
use std::io::{BufWriter, Write};

/// A hasher that uses the Gear algorithm.
///
/// This algorithm uses a `u8->u64` lookup table, which it mixes into the hash for each input byte.
///
/// In contrast to the algorithm described in the paper, this implementation uses 64-bit hash values
/// and lookup table entries.
///
/// Source: Xia, Wen, et al. "Ddelta: A deduplication-inspired fast delta compression approach."
/// Performance Evaluation 79 (2014): 258-272.
/// PDF: https://cswxia.github.io/pub/DElta-PEVA-2014.pdf
#[derive(Debug)]
pub(crate) struct ScalarGear64 {
    mask: u64,
    state: Gear64State,
    hash_writer: Option<BufWriter<File>>,
}

impl ScalarGear64 {
    pub(crate) fn new(mask: u64, hash_value_output_file: Option<File>) -> ScalarGear64 {
        ScalarGear64 {
            mask,
            state: Default::default(),
            hash_writer: hash_value_output_file.map(|f| BufWriter::new(f)),
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
struct Gear64State {
    hash: u64,
}

impl Gear64State {
    fn reset(&mut self) {
        self.hash = 0
    }

    fn ingest(&mut self, b: u8) {
        self.hash = (self.hash << 1).wrapping_add(gearhash::DEFAULT_TABLE[b as usize]);
    }

    fn check_hash(&self, mask: u64) -> bool {
        self.hash & mask == 0
    }
}

impl ChunkerImpl for ScalarGear64 {
    fn find_boundary(&mut self, data: &[u8]) -> Option<usize> {
        for (i, &b) in data.iter().enumerate() {
            self.state.ingest(b);

            // Record hash value
            if let Some(ref mut writer) = self.hash_writer {
                writeln!(writer, "{}", self.state.hash).expect("unable to write to hash file")
            }

            if self.state.check_hash(self.mask) {
                return Some(i);
            }
        }

        None
    }

    fn reset(&mut self) {
        self.state.reset()
    }
}

/// A hasher that implements the Gear algorithm, with optional SIMD-acceleration.
///
/// See the documentation on `ScalarGear` for more information.
#[derive(Debug, Clone)]
pub(crate) struct MaybeSimdGear64<'t> {
    mask: u64,
    hasher: gearhash::Hasher<'t>,
}

impl MaybeSimdGear64<'_> {
    pub(crate) fn new(mask: u64) -> MaybeSimdGear64<'static> {
        MaybeSimdGear64 {
            mask,
            hasher: Default::default(),
        }
    }
}

impl<'t> ChunkerImpl for MaybeSimdGear64<'t> {
    fn find_boundary(&mut self, data: &[u8]) -> Option<usize> {
        let a = self.hasher.next_match(data, self.mask);
        if let Some(pos) = a {
            // I think they return the next byte... hopefully.
            debug_assert!(pos > 0)
        }

        return a.map(|pos| pos - 1);
    }

    fn reset(&mut self) {
        self.hasher.set_hash(0)
    }
}
