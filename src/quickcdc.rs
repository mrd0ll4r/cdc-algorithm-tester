use cdchunking::ChunkerImpl;
use log::debug;
use std::collections::{HashMap, VecDeque};
use std::io::Read;
use std::mem::size_of;

pub(crate) struct QuickCDCWrapperWithHashMap<const N: usize, const M: usize, R, T> {
    algo: T,
    reader: R,
    map_table_front: HashMap<[u8; N], u32>,
    map_table_end: HashMap<[u8; M], u32>,
    min_chunk_size: usize,
    current_chunk: VecDeque<u8>,
    buf: [u8; 4096],
    state: State,
}

#[derive(Clone, Copy, Debug)]
enum State {
    Init,
    FirstSliceEmitted { chunk_size: usize, remaining: usize },
    SecondSliceEmitted { chunk_size: usize },
    EOFChunk { remaining: usize },
    EOF,
}

impl Default for State {
    fn default() -> Self {
        Self::Init
    }
}

#[derive(Debug)]
pub(crate) enum ChunkInput<'a> {
    Data(&'a [u8]),
    End,
}

#[derive(Clone, Copy, Debug)]
enum Pos {
    Boundary(usize),
    Pos(usize),
}

impl<const N: usize, const M: usize, R: Read, T: ChunkerImpl>
    QuickCDCWrapperWithHashMap<N, M, R, T>
{
    pub(crate) fn new(algo: T, reader: R, min_chunk_size: usize) -> Self {
        QuickCDCWrapperWithHashMap {
            algo,
            reader,
            map_table_front: Default::default(),
            map_table_end: Default::default(),
            min_chunk_size,
            current_chunk: VecDeque::with_capacity(2 * min_chunk_size),
            buf: [0; 4096],
            state: Default::default(),
        }
    }

    fn fill_current_chunk(&mut self) -> std::io::Result<usize> {
        // Read new data
        let n = self.reader.read(&mut self.buf)?;
        self.current_chunk.extend(&self.buf[..n]);
        Ok(n)
    }

    fn calculate_feature_vectors(&self, chunk_size: usize) -> ([u8; N], [u8; M]) {
        assert!(chunk_size >= N && chunk_size >= M);
        assert!(chunk_size <= self.current_chunk.len());

        // Record front feature vector
        let mut front = [0; N];
        for i in 0..N {
            front[i] = self.current_chunk[i];
        }

        // Record end feature vector
        let mut end = [0; M];
        for i in 0..M {
            end[i] = self.current_chunk[chunk_size - M + i];
        }

        (front, end)
    }

    fn produce_chunk(&mut self) -> std::io::Result<(usize, bool)> {
        let mut first = true;
        let mut front_lookup_needed = true;
        let mut jump_target = None;
        let mut pos = None;
        loop {
            if !first {
                let n = match self.fill_current_chunk() {
                    Ok(n) => n,
                    Err(err) => return Err(err),
                };
                debug!(
                    "filled {} bytes, now have {} bytes in buffer",
                    n,
                    self.current_chunk.len()
                );

                if n == 0 {
                    // EOF
                    return Ok((self.current_chunk.len(), true));
                }
            }
            first = false;

            // Check that we have at least enough data for front/end indices
            if self.current_chunk.len() < N.max(M) {
                // Probably EOF, but need to re-read to confirm
                continue;
            }

            // Calculate jump target
            if front_lookup_needed {
                front_lookup_needed = false;
                // Check front table
                let mut front = [0; N];
                for i in 0..N {
                    // Cannot panic because we checked the size of the current block earlier.
                    front[i] = self.current_chunk[i];
                }
                jump_target = self
                    .map_table_front
                    .get(&front)
                    .cloned()
                    .map(|v| v as usize);
                debug!(
                    "calculated front feature vector {:?}, entry in jump table is {:?}",
                    front, jump_target
                );
            }

            // Check if we can jump
            if let Some(jump_target) = jump_target {
                if self.current_chunk.len() < jump_target {
                    continue;
                }

                // Check jump target
                let mut end = [0; M];
                for i in 0..M {
                    end[i] = self.current_chunk[jump_target - M + i]
                }
                let end_length = self.map_table_end.get(&end).cloned().map(|v| v as usize);
                debug!(
                    "front length is {}, end length is {:?}",
                    jump_target, end_length
                );
                if let Some(length) = end_length {
                    if length == jump_target {
                        // This is probably a duplicate chunk, let's jump
                        debug!("front and end feature vectors match, length matches, will jump {} bytes ahead, end feature vector was {:?}",length,end);
                        return Ok((length, false));
                    }
                }
            }

            debug!("cannot jump, will skip bytes and perform CDC");
            // We cannot jump. Skip min_chunk_size bytes, then do CDC.
            if self.current_chunk.len() < self.min_chunk_size {
                // Need more data.
                // This only happens if we didn't have a match in the front jump table.
                continue;
            }

            if pos.is_none() {
                // First time we've reached min_chunk_size.
                // Initialize the algorithm and get ready to run CDC.
                pos = Some(self.min_chunk_size);
                self.algo.reset()
            }

            loop {
                debug!("pos is {:?}", pos);
                match self.advance_chunker(pos.unwrap()) {
                    Pos::Boundary(boundary) => {
                        // We found a boundary! Return the chunk
                        let chunk_size = pos.unwrap() + boundary + 1;
                        debug!(
                            "boundary found at {}, chunk size will be {}",
                            boundary, chunk_size
                        );
                        assert!(self.current_chunk.len() >= chunk_size);
                        return Ok((chunk_size, false));
                    }
                    Pos::Pos(new_pos) => {
                        pos = Some(new_pos);
                        if new_pos == self.current_chunk.len() {
                            // Need more data
                            break;
                        }
                        // Continue chunking the remaining data in current_chunk
                    }
                }
            }
        }
    }

    fn advance_chunker(&mut self, pos: usize) -> Pos {
        let (s1, s2) = self.current_chunk.as_slices();
        debug!(
            "running CDC algorith, pos: {}, current chunk size: {}, s1 size: {}, s2 size: {}",
            pos,
            self.current_chunk.len(),
            s1.len(),
            s2.len()
        );
        let (s, new_pos) = if pos < s1.len() {
            // Feed remaining data from s1
            debug!("feeding from first slice...");
            (&s1[pos..], s1.len())
        } else {
            // Feed remaining data from s2
            let pos_in_s2 = pos - s1.len();
            debug!("feeding from second slice...");
            (&s2[pos_in_s2..], self.current_chunk.len())
        };
        debug!(
            "slice to feed has {} bytes, new pos is {}",
            s.len(),
            new_pos
        );

        match self.algo.find_boundary(s) {
            None => {
                // No boundary in current block, need more data.
                debug!("no boundary found, getting more data...");
                Pos::Pos(new_pos)
            }
            Some(boundary) => {
                // We found a boundary!
                debug!("boundary found at {}", boundary);
                Pos::Boundary(boundary)
            }
        }
    }

    pub(crate) fn get_next_chunk(&mut self) -> Option<std::io::Result<ChunkInput>> {
        debug!(
            "current state is {:?}, buffer has {} byte remaining",
            self.state,
            self.current_chunk.len()
        );

        let (next_state, retval) = match self.state {
            State::Init => {
                // A fresh chunk needs to be produced.
                // We may already have data in the current buffer.
                // Emit: first slice of chunk
                // Next state: FirstSliceEmitted or EOFChunk or SecondSliceEmitted

                match self.produce_chunk() {
                    Ok((chunk_size, eof)) => {
                        assert!(chunk_size <= self.current_chunk.len());

                        let (s1, _) = self.current_chunk.as_slices();

                        if eof {
                            (
                                State::EOFChunk {
                                    remaining: self.current_chunk.len() - s1.len(),
                                },
                                Some(Ok(ChunkInput::Data(s1))),
                            )
                        } else {
                            if chunk_size < s1.len() {
                                // We won't need to emit a second slice, so we can skip one state here.
                                (
                                    State::SecondSliceEmitted { chunk_size },
                                    Some(Ok(ChunkInput::Data(&s1[..chunk_size]))),
                                )
                            } else {
                                (
                                    State::FirstSliceEmitted {
                                        chunk_size,
                                        remaining: chunk_size - s1.len(),
                                    },
                                    Some(Ok(ChunkInput::Data(s1))),
                                )
                            }
                        }
                    }
                    Err(err) => (State::EOF, Some(Err(err))),
                }
            }
            State::FirstSliceEmitted {
                chunk_size,
                remaining,
            } => {
                // Emit: second slice
                // Next state: SecondSliceEmitted

                let (_, s2) = self.current_chunk.as_slices();
                assert!(remaining <= s2.len());

                (
                    State::SecondSliceEmitted { chunk_size },
                    Some(Ok(ChunkInput::Data(&s2[..remaining]))),
                )
            }
            State::SecondSliceEmitted { chunk_size } => {
                // Calculate feature vectors, record in table, advance ring buffer.
                // Emit: End
                // Next state: Init

                if chunk_size > N + M {
                    let (front_features, end_features) = self.calculate_feature_vectors(chunk_size);
                    debug!(
                        "calculated feature vecotrs ({:?}, {:?}), will record with size {}",
                        front_features, end_features, chunk_size
                    );

                    assert!(chunk_size <= u32::MAX as usize);

                    self.map_table_front
                        .insert(front_features, chunk_size as u32);
                    self.map_table_end.insert(end_features, chunk_size as u32);
                }

                self.current_chunk.drain(..chunk_size).for_each(|_| {});

                (State::Init, Some(Ok(ChunkInput::End)))
            }
            State::EOFChunk { remaining } => {
                // If remaining data: emit from second slice
                // If no remaining data: emit end
                // Next state: EOFChunk{0} or EOF
                if remaining > 0 {
                    let (_, s2) = self.current_chunk.as_slices();
                    assert_eq!(remaining, s2.len());

                    (
                        State::EOFChunk { remaining: 0 },
                        Some(Ok(ChunkInput::Data(s2))),
                    )
                } else {
                    (State::EOF, Some(Ok(ChunkInput::End)))
                }
            }
            State::EOF => (State::EOF, None),
        };
        debug!("next state is {:?}, retval is {:?}", next_state, retval);

        self.state = next_state;
        return retval;
    }
}

pub(crate) struct QuickCDCWrapperDeque<const N: usize, const M: usize, R, T>
where
    [(); 256_usize.pow(N as u32)]:,
    [(); 256_usize.pow(M as u32)]:,
{
    algo: T,
    reader: R,
    map_table_front: Box<[u32; 256_usize.pow(N as u32)]>,
    map_table_end: Box<[u32; 256_usize.pow(M as u32)]>,
    min_chunk_size: usize,
    current_chunk: VecDeque<u8>,
    buf: [u8; 4096],
    state: State,
}

impl<const N: usize, const M: usize, R: Read, T: ChunkerImpl> QuickCDCWrapperDeque<N, M, R, T>
where
    [(); 256_usize.pow(N as u32)]:,
    [(); 256_usize.pow(M as u32)]:,
{
    pub(crate) fn new(algo: T, reader: R, min_chunk_size: usize) -> Self {
        QuickCDCWrapperDeque {
            algo,
            reader,
            map_table_front: Box::new([0; 256_usize.pow(N as u32)]),
            map_table_end: Box::new([0; 256_usize.pow(M as u32)]),
            min_chunk_size,
            current_chunk: VecDeque::with_capacity(2 * min_chunk_size),
            buf: [0; 4096],
            state: Default::default(),
        }
    }

    fn fill_current_chunk(&mut self) -> std::io::Result<usize> {
        // Read new data
        let n = self.reader.read(&mut self.buf)?;
        self.current_chunk.extend(&self.buf[..n]);
        Ok(n)
    }

    fn calculate_feature_vectors(&self, chunk_size: usize) -> (usize, usize) {
        assert!(chunk_size >= N && chunk_size >= M);
        assert!(chunk_size <= self.current_chunk.len());

        // Record front feature vector
        let mut front = [0; N];
        for i in 0..N {
            front[i] = self.current_chunk[i];
        }

        // Record end feature vector
        let mut end = [0; M];
        for i in 0..M {
            end[i] = self.current_chunk[chunk_size - M + i];
        }

        (
            Self::feature_vector_to_index(&front[..]),
            Self::feature_vector_to_index(&end[..]),
        )
    }

    fn feature_vector_to_index(v: &[u8]) -> usize {
        assert!(v.len() <= size_of::<usize>());

        let mut tmp = 0_usize;
        for &b in v.iter() {
            tmp = tmp << 8;
            tmp |= b as usize
        }

        tmp
    }

    fn produce_chunk(&mut self) -> std::io::Result<(usize, bool)> {
        let mut first = true;
        let mut front_lookup_needed = true;
        let mut jump_target = None;
        let mut pos = None;
        loop {
            if !first {
                let n = match self.fill_current_chunk() {
                    Ok(n) => n,
                    Err(err) => return Err(err),
                };
                debug!(
                    "filled {} bytes, now have {} bytes in buffer",
                    n,
                    self.current_chunk.len()
                );

                if n == 0 {
                    // EOF
                    return Ok((self.current_chunk.len(), true));
                }
            }
            first = false;

            // Check that we have at least enough data for front/end indices
            if self.current_chunk.len() < N.max(M) {
                // Probably EOF, but need to re-read to confirm
                continue;
            }

            // Calculate jump target
            if front_lookup_needed {
                front_lookup_needed = false;
                // Check front table
                let mut front = [0; N];
                for i in 0..N {
                    // Cannot panic because we checked the size of the current block earlier.
                    front[i] = self.current_chunk[i];
                }
                jump_target = self
                    .map_table_front
                    .get(Self::feature_vector_to_index(&front[..]))
                    .cloned()
                    .map(|v| if v == 0 { None } else { Some(v) })
                    .flatten()
                    .map(|v| v as usize);
                debug!(
                    "calculated front feature vector {:?}, entry in jump table is {:?}",
                    front, jump_target
                );
            }

            // Check if we can jump
            if let Some(jump_target) = jump_target {
                if self.current_chunk.len() < jump_target {
                    continue;
                }

                // Check jump target
                let mut end = [0; M];
                for i in 0..M {
                    end[i] = self.current_chunk[jump_target - M + i]
                }
                let end_length = self
                    .map_table_end
                    .get(Self::feature_vector_to_index(&end[..]))
                    .cloned()
                    .map(|v| if v == 0 { None } else { Some(v) })
                    .flatten()
                    .map(|v| v as usize);
                debug!(
                    "front length is {}, end length is {:?}",
                    jump_target, end_length
                );
                if let Some(length) = end_length {
                    if length == jump_target {
                        // This is probably a duplicate chunk, let's jump
                        debug!("front and end feature vectors match, length matches, will jump {} bytes ahead, end feature vector was {:?}",length,end);
                        return Ok((length, false));
                    }
                }
            }

            debug!("cannot jump, will skip bytes and perform CDC");
            // We cannot jump. Skip min_chunk_size bytes, then do CDC.
            if self.current_chunk.len() < self.min_chunk_size {
                // Need more data.
                // This only happens if we didn't have a match in the front jump table.
                continue;
            }

            if pos.is_none() {
                // First time we've reached min_chunk_size.
                // Initialize the algorithm and get ready to run CDC.
                pos = Some(self.min_chunk_size);
                self.algo.reset()
            }

            loop {
                debug!("pos is {:?}", pos);
                match self.advance_chunker(pos.unwrap()) {
                    Pos::Boundary(boundary) => {
                        // We found a boundary! Return the chunk
                        let chunk_size = pos.unwrap() + boundary + 1;
                        debug!(
                            "boundary found at {}, chunk size will be {}",
                            boundary, chunk_size
                        );
                        assert!(self.current_chunk.len() >= chunk_size);
                        return Ok((chunk_size, false));
                    }
                    Pos::Pos(new_pos) => {
                        pos = Some(new_pos);
                        if new_pos == self.current_chunk.len() {
                            // Need more data
                            break;
                        }
                        // Continue chunking the remaining data in current_chunk
                    }
                }
            }
        }
    }

    fn advance_chunker(&mut self, pos: usize) -> Pos {
        let (s1, s2) = self.current_chunk.as_slices();
        debug!(
            "running CDC algorith, pos: {}, current chunk size: {}, s1 size: {}, s2 size: {}",
            pos,
            self.current_chunk.len(),
            s1.len(),
            s2.len()
        );
        let (s, new_pos) = if pos < s1.len() {
            // Feed remaining data from s1
            debug!("feeding from first slice...");
            (&s1[pos..], s1.len())
        } else {
            // Feed remaining data from s2
            let pos_in_s2 = pos - s1.len();
            debug!("feeding from second slice...");
            (&s2[pos_in_s2..], self.current_chunk.len())
        };
        debug!(
            "slice to feed has {} bytes, new pos is {}",
            s.len(),
            new_pos
        );

        match self.algo.find_boundary(s) {
            None => {
                // No boundary in current block, need more data.
                debug!("no boundary found, getting more data...");
                Pos::Pos(new_pos)
            }
            Some(boundary) => {
                // We found a boundary!
                debug!("boundary found at {}", boundary);
                Pos::Boundary(boundary)
            }
        }
    }

    pub(crate) fn get_next_chunk(&mut self) -> Option<std::io::Result<ChunkInput>> {
        debug!(
            "current state is {:?}, buffer has {} byte remaining",
            self.state,
            self.current_chunk.len()
        );

        let (next_state, retval) = match self.state {
            State::Init => {
                // A fresh chunk needs to be produced.
                // We may already have data in the current buffer.
                // Emit: first slice of chunk
                // Next state: FirstSliceEmitted or EOFChunk or SecondSliceEmitted

                match self.produce_chunk() {
                    Ok((chunk_size, eof)) => {
                        assert!(chunk_size <= self.current_chunk.len());

                        let (s1, _) = self.current_chunk.as_slices();

                        if eof {
                            (
                                State::EOFChunk {
                                    remaining: self.current_chunk.len() - s1.len(),
                                },
                                Some(Ok(ChunkInput::Data(s1))),
                            )
                        } else {
                            if chunk_size < s1.len() {
                                // We won't need to emit a second slice, so we can skip one state here.
                                (
                                    State::SecondSliceEmitted { chunk_size },
                                    Some(Ok(ChunkInput::Data(&s1[..chunk_size]))),
                                )
                            } else {
                                (
                                    State::FirstSliceEmitted {
                                        chunk_size,
                                        remaining: chunk_size - s1.len(),
                                    },
                                    Some(Ok(ChunkInput::Data(s1))),
                                )
                            }
                        }
                    }
                    Err(err) => (State::EOF, Some(Err(err))),
                }
            }
            State::FirstSliceEmitted {
                chunk_size,
                remaining,
            } => {
                // Emit: second slice
                // Next state: SecondSliceEmitted

                let (_, s2) = self.current_chunk.as_slices();
                assert!(remaining <= s2.len());

                (
                    State::SecondSliceEmitted { chunk_size },
                    Some(Ok(ChunkInput::Data(&s2[..remaining]))),
                )
            }
            State::SecondSliceEmitted { chunk_size } => {
                // Calculate feature vectors if chunk is large enough, record in table, advance ring buffer.
                // Emit: End
                // Next state: Init

                if chunk_size > N + M {
                    let (front_features, end_features) = self.calculate_feature_vectors(chunk_size);
                    debug!(
                        "calculated feature vecotrs ({:?}, {:?}), will record with size {}",
                        front_features, end_features, chunk_size
                    );

                    assert!(chunk_size <= u32::MAX as usize);

                    self.map_table_front[front_features] = chunk_size as u32;
                    self.map_table_end[end_features] = chunk_size as u32;
                }

                self.current_chunk.drain(..chunk_size).for_each(|_| {});

                (State::Init, Some(Ok(ChunkInput::End)))
            }
            State::EOFChunk { remaining } => {
                // If remaining data: emit from second slice
                // If no remaining data: emit end
                // Next state: EOFChunk{0} or EOF
                if remaining > 0 {
                    let (_, s2) = self.current_chunk.as_slices();
                    assert_eq!(remaining, s2.len());

                    (
                        State::EOFChunk { remaining: 0 },
                        Some(Ok(ChunkInput::Data(s2))),
                    )
                } else {
                    (State::EOF, Some(Ok(ChunkInput::End)))
                }
            }
            State::EOF => (State::EOF, None),
        };
        debug!("next state is {:?}, retval is {:?}", next_state, retval);

        self.state = next_state;
        return retval;
    }
}
