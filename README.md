# cdc-algorithm-tester

Rust Binary to Test Various CDC Algorithms

```
A program that invokes a selectable chunking algorithm with given configuration on a provided file.

Usage: cdc-algorithm-tester [OPTIONS] --input-file <FILE> <COMMAND>

Commands:
  ae    Chunks the input file using AE
  ram   Chunks the input file using RAM
  bfbc  Subcommands relating to BFBC chunking
  mii   Chunks the input file using MII
  pci   Chunks the input file using PCI
  help  Print this message or the help of the given subcommand(s)

Options:
      --skip-fingerprinting     Whether to only perform chunking, not fingerprinting
  -i, --input-file <FILE>       The file to operate on
      --max-chunk-size <BYTES>  A max chunk size to optionally enforce
  -h, --help                    Print help
  -V, --version                 Print version
```

## Usage

Specify an input file with `-i <FILE>`, then run one of the subcommands on it, to chunk it using the specified algorithm.
The output is a list of chunks as `<digest>,<size in bytes>`, like:
```
6264013dd6158754a260e8ef56f866502d0cd5c3,586
e1fb30e629e75152a72e6d1e55eae3a8ca5683c6,1248
75b4613cea811551651bb70cf6764c8e553b580d,1947
53c98786b2bc574c87bd4fc3131c4ae90448185d,924
385b9cf93889b6890711ee06513dbbd362695cfd,713
8da320a0bcd286028b773ec9a0265ac7f03eb310,590
...
```

It is trivial to validate that the blocks add up to the original file:
```bash
$ cdc-algorithm-tester <invocation> | awk -F',' '{print $2}' | paste -sd+ | bc
<sum of all chunk sizes>
```

The various algorithms have different parameters to configure.
Use `help <command>` to get a list for each of them, for example:

```bash
$ cdc-algorithm-tester help ae
Chunks the input file using AE

Usage: cdc-algorithm-tester --input-file <FILE> ae <WINDOW_SIZE>

Arguments:
  <WINDOW_SIZE>  The size of the fixed window

Options:
  -h, --help     Print help
  -V, --version  Print version
```

## Performance considerations

We use the [cdchunking](https://crates.io/crates/cdchunking) crate as a framework to implement and use chunking algorithms.
Specifically, we iterate over the file in buffered blocks, which avoids frequent `read()` operations.
The CDC algorithms are fed these blocks and update their internal state accordingly.
They produce cut-point indices.

On a higher level, we iterate over the file using _the same_ buffered blocks, re-sliced according to the chunk boundaries produced by the chosen algorithm.
This allows us to chunk a file without allocating.
On an even higher level, it is then possible to load the file into a RAMDisk before chunking, to isolate from disk performance.

The algorithms are, whenever applicable, implemented as described or shown in the respective publications.
It was sometimes necessary to modify the implementations for correctness, which we have always done.
In some cases we have taken the liberty of implementing obvious performance improvements, which can be enabled with respective flags.

In general, we chose to operate on a single file, which in practice means that we TAR our datasets and chunk the tarballs.
Keep in mind that there is considerable overhead implied with launching a process, which could affect results when chunking many (small) files.

By default, we produce SHA1 fingerprints for the chunks.
The `--skip-fingerprinting` flag can be provided to instead provide empty strings as fingerprints, to isolate the performance of the CDC algorithms.
We use [std::hints::black_box](https://doc.rust-lang.org/std/hint/fn.black_box.html) to ensure that the compiler still assumes chunk data to be used in either case.

## Building

The code is built with link-time optimization and `codegen-units=1` for the `release` profile, which is used to measure performance.
This will take a while to build, but should produce fast code.

Preferably and most easily: build id docker using the [build-in-docker.sh](./build-in-docker.sh) script.
The binary will be placed in `out/`.

Alternatively, on a host with an up-to-date version of Rust installed:
```
cargo build --release --locked
```
This will produce a binary in `target/release/`

If performance is not a consideration, build without `--release`.
This will also enable some assertions used to check correctness of the algorithms.

## Implemented Algorithms

See our fork of the `cdchunking-rs` crate [here](https://github.com/mrd0ll4r/cdchunking-rs/tree/new-algorithms).
Currently, we implement:
- Asymmetric Extremum (AE)
- Rapid Asymmetric Extremum (RAM), with optional optimizations made to the implementation
- Byte Frequency-Based Chunking (BFBC), including functionality to derive byte frequencies from an input file.
  We extend the invocation of this algorithm with functionality to use the top `n` most frequent pairs, as well as to skip the `k` most frequent and use the `n` next ones afterwards.
- Minimal Incremental Interval (MII)
- Parity Check of Interval (PCI), including an optimized version that calculates a running popcount, instead of re-calculating the popcount of the window for each byte.

## License

MIT, see [LICENSE](LICENSE).