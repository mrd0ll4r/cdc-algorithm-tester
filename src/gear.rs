use cdchunking::ChunkerImpl;

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
#[derive(Debug, Clone)]
pub(crate) struct ScalarGear {
    mask: u64,
    state: GearState,
}

impl ScalarGear {
    pub(crate) fn new(mask: u64) -> ScalarGear {
        ScalarGear {
            mask,
            state: Default::default(),
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
struct GearState {
    hash: u64,
}

impl GearState {
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

impl ChunkerImpl for ScalarGear {
    fn find_boundary(&mut self, data: &[u8]) -> Option<usize> {
        for (i, &b) in data.iter().enumerate() {
            self.state.ingest(b);

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
pub(crate) struct MaybeSimdGear<'t> {
    mask: u64,
    hasher: gearhash::Hasher<'t>,
}

impl MaybeSimdGear<'_> {
    pub(crate) fn new(mask: u64) -> MaybeSimdGear<'static> {
        MaybeSimdGear {
            mask,
            hasher: Default::default(),
        }
    }
}

impl<'t> ChunkerImpl for MaybeSimdGear<'t> {
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