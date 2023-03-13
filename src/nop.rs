use cdchunking::ChunkerImpl;

#[derive(Debug, Clone, Copy)]
pub(crate) struct NopChunker {}

impl NopChunker {
    pub(crate) fn new() -> NopChunker {
        NopChunker {}
    }
}

impl ChunkerImpl for NopChunker {
    fn find_boundary(&mut self, _data: &[u8]) -> Option<usize> {
        None
    }

    fn reset(&mut self) {
        // Nothing to do.
    }
}
