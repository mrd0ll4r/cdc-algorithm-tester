mod logging;

use anyhow::{ensure, Context};
use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use cdchunking::{ChunkInput, ChunkStream, Chunker, ChunkerImpl};
use clap::{Parser, Subcommand};
use log::debug;
use sha1::{Digest, Sha1};
use std::collections::HashMap;
use std::fmt::Debug;
use std::fs::File;
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::PathBuf;

macro_rules! impl_pci_test_with_size {
    ($running_popcount:expr,$window_size:literal,$thresh:expr,$f:expr,$max_chunk_size:expr,$skip_fingerprinting:expr) => {
        if $running_popcount {
            let algo = cdchunking::PCIChunkerRunningPopcount::<$window_size>::new($thresh);
            chunk_with_algorithm_and_size_limit($f, algo, $max_chunk_size, $skip_fingerprinting)
        } else {
            let algo = cdchunking::PCIChunker::<$window_size>::new($thresh);
            chunk_with_algorithm_and_size_limit($f, algo, $max_chunk_size, $skip_fingerprinting)
        }
    };
}

macro_rules! impl_pci_test_for_sizes {
    ($running_popcount:expr,$window_size:expr,$thresh:expr,$f:expr,$max_chunk_size:expr,$skip_fingerprinting:expr,$( $x:expr ),*) => {
        match $window_size {
            $(
            $x => impl_pci_test_with_size!($running_popcount,$x,$thresh,$f,$max_chunk_size,$skip_fingerprinting),
            )*
            _ => unreachable!(),
        }
    };
}

#[derive(Parser)]
#[command(name = "CDC Algorithm Tester")]
#[command(author = "Leo Balduf <leobalduf@gmail.com>")]
#[command(version)]
#[command(propagate_version = true)]
#[command(about = "A program that invokes a selectable chunking algorithm with given configuration on a provided file.", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Whether to only perform chunking, not fingerprinting.
    #[arg(long)]
    skip_fingerprinting: bool,

    /// The file to operate on.
    #[arg(short, long, value_name = "FILE")]
    input_file: PathBuf,

    /// A max chunk size to optionally enforce.
    #[arg(long, value_name = "BYTES")]
    max_chunk_size: Option<usize>,
}

#[derive(Subcommand)]
enum Commands {
    /// Chunks the input file using AE.
    AE {
        /// The size of the fixed window.
        window_size: usize,
    },

    /// Chunks the input file using RAM.
    RAM {
        /// The size of the fixed window.
        window_size: usize,
        /// Whether to use a potentially optimized version of the algorithm.
        #[arg(long)]
        use_optimized_version: bool,
    },

    /// Subcommands relating to BFBC chunking.
    BFBC {
        #[command(subcommand)]
        command: BFBCCommands,
    },

    /// Chunks the input file using MII.
    MII {
        /// The threshold length of the increasing sequence.
        interval_length_threshold: usize,
    },

    /// Chunks the input file using PCI.
    PCI {
        /// The size of the window. Currently, only windows between 2 and 5 bytes are implemented.
        #[arg(value_parser = clap::value_parser!(u16).range(2..=5))]
        window_size: u16,
        /// The threshold for the number of one bits in the window.
        one_bits_threshold: u32,
        /// Whether to keep a running popcount while ingesting bytes.
        /// Originally, the popcount of the entire window is calculated for each byte.
        #[arg(long)]
        running_popcount: bool,
    },
}

#[derive(Subcommand)]
enum BFBCCommands {
    /// Analyzes the input file for byte pair frequencies for subsequent application of BFBC.
    Analyze {
        /// The file to store frequency results in.
        output: PathBuf,
    },
    /// Chunks the input file using BFBC and the given byte pair frequencies.
    /// As an addition to the original algorithm, this supports skipping the k most frequent byte
    /// pairs, and then using the n next most frequent ones. Set num_byte_pairs_to_skip to zero for
    /// the original algorithm.
    Chunk {
        /// The file to read frequency results from.
        frequency_file: PathBuf,
        /// The number of most frequent byte pairs to skip.
        num_byte_pairs_to_skip: usize,
        /// The number of most frequent byte pairs to use.
        num_byte_pairs: usize,
        /// The minimum chunk size.
        min_chunk_size: usize,
    },
}

fn main() -> anyhow::Result<()> {
    logging::set_up_logging().unwrap();

    // For AE: windowSize := int(math.Round(float64(avgSize) / (math.E - 1)))

    debug!("parsing commandline...");
    let cli: Cli = Cli::parse();

    debug!("opening input file...");
    let f = File::open(cli.input_file).context("unable to open input file")?;

    match cli.command {
        Commands::AE { window_size } => {
            let algo = cdchunking::AEChunker::new(window_size);
            chunk_with_algorithm_and_size_limit(
                f,
                algo,
                cli.max_chunk_size,
                cli.skip_fingerprinting,
            )
        }
        Commands::RAM {
            window_size,
            use_optimized_version,
        } => {
            ensure!(window_size > 0, "window size needs to be at least 1");
            if use_optimized_version {
                let algo = cdchunking::MaybeOptimizedRAMChunker::new(window_size);
                chunk_with_algorithm_and_size_limit(
                    f,
                    algo,
                    cli.max_chunk_size,
                    cli.skip_fingerprinting,
                )
            } else {
                let algo = cdchunking::RAMChunker::new(window_size);
                chunk_with_algorithm_and_size_limit(
                    f,
                    algo,
                    cli.max_chunk_size,
                    cli.skip_fingerprinting,
                )
            }
        }
        Commands::BFBC { command } => match command {
            BFBCCommands::Analyze { output } => derive_bfbc_frequencies_to_file(f, output),
            BFBCCommands::Chunk {
                frequency_file,
                num_byte_pairs_to_skip,
                num_byte_pairs,
                min_chunk_size,
            } => {
                ensure!(
                    num_byte_pairs > 0,
                    "need to operate on at least one byte pair"
                );
                let byte_pairs = read_bfbc_frequencies_from_file(
                    frequency_file,
                    num_byte_pairs_to_skip,
                    num_byte_pairs,
                )
                .context("unable to read BFBC byte pair frequencies from file")?;
                let algo = cdchunking::BFBCChunker::new(byte_pairs, min_chunk_size);
                chunk_with_algorithm_and_size_limit(
                    f,
                    algo,
                    cli.max_chunk_size,
                    cli.skip_fingerprinting,
                )
            }
        },
        Commands::MII {
            interval_length_threshold,
        } => {
            ensure!(
                interval_length_threshold > 0,
                "interval length threshold needs to be at least 1"
            );
            let algo = cdchunking::MIIChunker::new(interval_length_threshold);
            chunk_with_algorithm_and_size_limit(
                f,
                algo,
                cli.max_chunk_size,
                cli.skip_fingerprinting,
            )
        }
        Commands::PCI {
            window_size,
            one_bits_threshold,
            running_popcount,
        } => {
            ensure!(
                one_bits_threshold > 0,
                "one bits threshold must be at least 1"
            );

            ensure!(
                one_bits_threshold <= window_size as u32 * 8,
                "there are only {} bits in {} bytes, threshold mus be <= that",
                window_size * 8,
                window_size
            );

            impl_pci_test_for_sizes!(
                running_popcount,
                window_size,
                one_bits_threshold,
                f,
                cli.max_chunk_size,
                cli.skip_fingerprinting,
                2,
                3,
                4,
                5
            )
        }
    }
}

fn chunk_with_algorithm_and_size_limit<C: ChunkerImpl + Debug>(
    f: File,
    cdc_algo: C,
    size_limit: Option<usize>,
    skip_fingerprinting: bool,
) -> anyhow::Result<()> {
    debug!(
        "will chunk using {:?} and a size limit of {:?}",
        cdc_algo, size_limit
    );
    if let Some(size_limit) = size_limit {
        let chunker = Chunker::new(cdc_algo).max_size(size_limit);
        process_chunk_stream(chunker.stream(f), skip_fingerprinting)
    } else {
        let chunker = Chunker::new(cdc_algo);
        process_chunk_stream(chunker.stream(f), skip_fingerprinting)
    }
}

fn process_chunk_stream<C: ChunkerImpl, R: Read>(
    mut chunk_iterator: ChunkStream<R, C>,
    skip_fingerprinting: bool,
) -> anyhow::Result<()> {
    let mut chunk_size = 0;
    let mut chunk_hasher = Sha1::new();

    while let Some(chunk) = chunk_iterator.read() {
        let chunk = chunk.context("unable to read file")?;

        // Make sure that the compiler cannot optimize out anything about the chunks produced.
        std::hint::black_box(&chunk);

        match chunk {
            ChunkInput::Data(d) => {
                debug!("got {} bytes of data for the current chunk...", d.len());
                chunk_size += d.len();
                if !skip_fingerprinting {
                    chunk_hasher.update(d);
                }
            }
            ChunkInput::End => {
                let digest = if skip_fingerprinting {
                    // This does not allocate.
                    String::new()
                } else {
                    format!("{:x}", chunk_hasher.finalize_reset())
                };
                debug!("chunk complete, size: {}, digest: {}", chunk_size, digest);
                println!("{},{}", digest, chunk_size);
                chunk_size = 0;
            }
        }
    }

    Ok(())
}

fn read_bfbc_frequencies_from_file(
    input: PathBuf,
    skip: usize,
    count: usize,
) -> anyhow::Result<Vec<(u8, u8)>> {
    let f = File::open(input).context("unable to open file")?;
    let mut reader = BufReader::new(f);
    let mut byte_pairs = Vec::with_capacity(count + skip);

    for _i in 0..(count + skip) {
        let p0 = reader.read_u8().context("unable to read from file")?;
        let p1 = reader.read_u8().context("unable to read from file")?;
        let cnt = reader
            .read_u64::<BigEndian>()
            .context("unable to read from file")?;
        debug!("read pair ({},{}), count {}", p0, p1, cnt);

        byte_pairs.push((p0, p1));
    }

    let byte_pairs = byte_pairs.into_iter().skip(skip).collect::<Vec<_>>();
    assert_eq!(byte_pairs.len(), count);

    Ok(byte_pairs)
}

fn derive_bfbc_frequencies_to_file(f: File, output: PathBuf) -> anyhow::Result<()> {
    // Calculate frequencies from input file.
    let frequencies = calculate_bfbc_frequencies(f)
        .context("unable to calculate byte pair frequencies from input file")?;

    // Create output file.
    let out_file = File::create(output).context("unable to create output file")?;
    let mut out_writer = BufWriter::new(out_file);

    // Write results to file.
    for (pair, count) in frequencies {
        out_writer
            .write_u8(pair.0)
            .context("unable to write to file")?;
        out_writer
            .write_u8(pair.1)
            .context("unable to write to file")?;
        out_writer
            .write_u64::<BigEndian>(count)
            .context("unable to write to file")?;
    }

    // Flush output writer.
    out_writer.flush().context("unable to flush output file")?;

    Ok(())
}

fn calculate_bfbc_frequencies(f: File) -> anyhow::Result<Vec<((u8, u8), u64)>> {
    let buf_reader = BufReader::new(f);
    let mut pairs_with_frequency = buf_reader
        .bytes()
        .fold(Ok((HashMap::new(), None)), |acc, current_byte| match acc {
            Ok((mut acc, previous_byte)) => match current_byte {
                Ok(b) => {
                    if let Some(previous_byte) = previous_byte {
                        let pair = (previous_byte, b);
                        *acc.entry(pair).or_default() += 1;
                    }

                    Ok((acc, Some(b)))
                }
                Err(err) => Err(err),
            },
            Err(err) => Err(err),
        })
        .map(|(hm, _)| hm)
        .context("unable to read input file")?
        .into_iter()
        .collect::<Vec<_>>();
    debug!(
        "calculated occurrences for {} byte pairs",
        pairs_with_frequency.len()
    );

    pairs_with_frequency.sort_by_key(|p| p.1);
    pairs_with_frequency.reverse();
    debug!(
        "top 10 most frequent bytes: {:?}",
        pairs_with_frequency.iter().take(10).collect::<Vec<_>>()
    );

    Ok(pairs_with_frequency)
}
