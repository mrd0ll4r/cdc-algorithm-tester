/*
Copyright notice for the Rust port:

 (C) 2016 Remi Rampin and adler32-rs contributors

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.


Copyright notice for the original C code from the zlib project:

 (C) 1995-2017 Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly        Mark Adler
  jloup@gzip.org          madler@alumni.caltech.edu
 */

use cdchunking::ChunkerImpl;
use log::debug;
use std::fmt::{Debug, Formatter};

#[derive(Clone)]
pub(crate) struct Adler32Chunker<const W: usize> {
    mask: u32,
    inner: adler32::RollingAdler32,
    window: [u8; W],
    pos: usize,
}

impl<const W: usize> Debug for Adler32Chunker<W> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Adler32Chunker")
            .field("W", &W)
            .field("mask", &self.mask)
            .field("window", &self.window)
            .field("pos", &self.pos)
            .finish()
    }
}

impl<const W: usize> Adler32Chunker<W> {
    pub(crate) fn new(mask: u32) -> Self {
        Adler32Chunker {
            mask,
            window: [0; W],
            pos: 0,
            inner: adler32::RollingAdler32::new(),
        }
    }
}

impl<const W: usize> ChunkerImpl for Adler32Chunker<W> {
    fn find_boundary(&mut self, data: &[u8]) -> Option<usize> {
        for (i, &b) in data.iter().enumerate() {
            let pos = self.pos % W;
            debug!(
                "ingesting {}, pos is {}, pos in window is {}, hash is {:032b}, mask {:032b}",
                b,
                self.pos,
                pos,
                self.inner.hash(),
                self.mask
            );
            if self.pos >= W {
                self.inner.remove(W, self.window[pos])
            }
            self.inner.update(b);
            self.window[pos] = b;
            self.pos += 1;

            if self.pos >= W {
                debug!(
                    "checking has. pos is {}, hash is {:032b}, mask {:032b}",
                    self.pos,
                    self.inner.hash(),
                    self.mask
                );
                if self.inner.hash() & self.mask == 0 {
                    return Some(i);
                }
            }
        }

        None
    }

    fn reset(&mut self) {
        self.pos = 0;
        self.window = [0; W];
        self.inner = adler32::RollingAdler32::new()
    }
}
