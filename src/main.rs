#![feature(generic_const_exprs)]

mod gear;
mod logging;
mod quickcdc;

use crate::quickcdc::{QuickCDCWrapperDeque, QuickCDCWrapperWithHashMap};
use anyhow::{anyhow, ensure, Context};
use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use cdchunking::{ChunkInput, ChunkStream, Chunker, ChunkerImpl};
use clap::{Parser, Subcommand};
use log::debug;
use sha1::{Digest, Sha1};
use std::collections::HashMap;
use std::f64::consts;
use std::fmt::Debug;
use std::fs::File;
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::PathBuf;

macro_rules! impl_pci_test_with_size {
    ($running_popcount:expr,$window_size:literal,$thresh:expr,$f:expr,$max_chunk_size:expr,$quiet:expr,$quickcdc_min_chunk_size:expr,$quickcdc_use_hashmap:expr,$quickcdc_n:expr,$quickcdc_m:expr) => {
        if $running_popcount {
            let algo = cdchunking::PCIChunkerRunningPopcount::<$window_size>::new($thresh);
            chunk_with_algorithm_and_size_limit(
                $f,
                algo,
                $max_chunk_size,
                $quiet,
                $quickcdc_min_chunk_size,
                $quickcdc_use_hashmap,
                $quickcdc_n,
                $quickcdc_m,
            )
        } else {
            let algo = cdchunking::PCIChunker::<$window_size>::new($thresh);
            chunk_with_algorithm_and_size_limit(
                $f,
                algo,
                $max_chunk_size,
                $quiet,
                $quickcdc_min_chunk_size,
                $quickcdc_use_hashmap,
                $quickcdc_n,
                $quickcdc_m,
            )
        }
    };
}

macro_rules! impl_pci_test_for_sizes {
    ($running_popcount:expr,$window_size:expr,$thresh:expr,$f:expr,$max_chunk_size:expr,$quiet:expr,$quickcdc_min_chunk_size:expr,$quickcdc_use_hashmap:expr,$quickcdc_n:expr,$quickcdc_m:expr,$( $x:expr ),*) => {
        match $window_size {
            $(
            $x => impl_pci_test_with_size!($running_popcount,$x,$thresh,$f,$max_chunk_size,$quiet,$quickcdc_min_chunk_size,$quickcdc_use_hashmap,$quickcdc_n,$quickcdc_m),
            )*
            _ => unreachable!(),
        }
    };
}

macro_rules! impl_quickcdc_for_sizes {
    ($algo:expr,$f:expr,$min_chunk_size:expr,$quiet:expr,$p_n:expr,$p_m:expr,$( ($n:expr,$m:expr) ),*) => {
        match ($p_n,$p_m) {
            $(
            ($n,$m) => {
                process_quickcdc_wrapper::<$n,$m,_,_>(
                        $algo,
                        $f,
                        $min_chunk_size,
                        $quiet,
                    )
            },
            )*
            _ => unreachable!(),
        }
    };
}

macro_rules! impl_quickcdc_hashmap_for_sizes {
    ($algo:expr,$f:expr,$min_chunk_size:expr,$quiet:expr,$p_n:expr,$p_m:expr,$( ($n:expr,$m:expr) ),*) => {
        match ($p_n,$p_m) {
            $(
            ($n,$m) => {
                process_quickcdc_wrapper_hashmap::<$n,$m,_,_>(
                        $algo,
                        $f,
                        $min_chunk_size,
                        $quiet,
                    )
            },
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
    #[arg(short, long)]
    quiet: bool,

    /// The file to operate on.
    #[arg(short, long, value_name = "FILE")]
    input_file: PathBuf,

    /// A max chunk size to optionally enforce.
    ///
    /// This is currently not possible while also using the QuickCDC wrapper.
    #[arg(long, value_name = "BYTES")]
    max_chunk_size: Option<usize>,

    /// Enable the QuickCDC wrapper around the selected algorithm using the given minimum chunk size.
    ///
    /// Note that the minimum chunk size will be skipped and not processed by the inner CDC
    /// algorithm. From the perspective of the inner algorithm, chunks are `min_chunk_size` smaller
    /// than they actually are. You need to adjust the parameters of the inner algorithm
    /// accordingly.
    ///
    /// Setting this to zero will enable only the caching functionality of the QuickCDC wrapper, and
    /// not skip any bytes for unique chunks.
    ///
    /// Chunks that are smaller than the sum of both feature vector lengths (e.g., 3+3=6 by default)
    /// will not be cached.
    #[arg(long, value_name = "BYTES")]
    quickcdc_min_chunk_size: Option<usize>,

    /// The number of bytes to use for the front feature vector of the QuickCDC wrapper.
    ///
    /// Only implemented for 1,2,3, or 4 bytes.
    /// Note that a large value may cause large consumptions of memory.
    #[arg(long, value_name = "BYTES", default_value_t = 3, value_parser = clap::value_parser!(u16).range(1..=4))]
    quickcdc_front_feature_vector_length: u16,

    /// The number of bytes to use for the end feature vector of the QuickCDC wrapper.
    ///
    /// Only implemented for 1,2,3, or 4 bytes.
    /// Note that a large value may cause large consumptions of memory.
    #[arg(long, value_name = "BYTES", default_value_t = 3, value_parser = clap::value_parser!(u16).range(1..=4))]
    quickcdc_end_feature_vector_length: u16,

    /// Use the hashmap-based implementation of the QuickCDC wrapper.
    #[arg(long)]
    quickcdc_use_hashmap: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Chunks the input file using AE.
    AE {
        /// The target chunk size.
        target_chunk_size: usize,
    },

    /// Chunks the input file using RAM.
    RAM {
        /// The target chunk size.
        target_chunk_size: usize,
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
        /// The size of the window. Currently, not all possible window sizes are implemented.
        ///
        /// Windows between 2 and 16 bytes, as well as some larger ones are implemented.
        window_size: u16,
        /// The threshold for the number of one bits in the window.
        one_bits_threshold: u32,
        /// Whether to keep a running popcount while ingesting bytes.
        /// Originally, the popcount of the entire window is calculated for each byte.
        #[arg(long)]
        running_popcount: bool,
    },

    /// Chunks the input file using Gear.
    Gear {
        /// The target chunk size.
        target_chunk_size: usize,
    },

    /// Chunks the input file using Gear with normalized chunking modifications.
    NCGear {
        /// The target chunk size.
        target_chunk_size: usize,

        /// The level of normalized chunking to apply. Needs to be in [1;16].
        ///
        /// This controls how much the two bitmasks are shifted away from each other.
        #[arg(value_parser = clap::value_parser!(u8).range(1..=16))]
        level: u8,
    },

    /// Chunks the input file using 64-bit Gear.
    Gear64 {
        /// Whether to use the SIMD-ready implementation.
        /// If unset, the default (scalar) implementation is used.
        /// This does not control compiler autovectorization.
        #[arg(long)]
        allow_simd_impl: bool,

        /// The target chunk size.
        target_chunk_size: usize,
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
        Commands::AE { target_chunk_size } => {
            ensure!(
                target_chunk_size > 0,
                "target chunk size needs to be at least 1"
            );
            let window_size = (target_chunk_size as f64 / (consts::E - 1_f64)) as usize;
            let algo = cdchunking::AEChunker::new(window_size);
            chunk_with_algorithm_and_size_limit(
                f,
                algo,
                cli.max_chunk_size,
                cli.quiet,
                cli.quickcdc_min_chunk_size,
                cli.quickcdc_use_hashmap,
                cli.quickcdc_front_feature_vector_length,
                cli.quickcdc_end_feature_vector_length,
            )
        }
        Commands::RAM {
            target_chunk_size,
            use_optimized_version,
        } => {
            ensure!(
                target_chunk_size > 0,
                "target chunk size needs to be at least 1"
            );
            let window_size = (target_chunk_size as f64 / (consts::E - 1_f64)) as usize;
            if use_optimized_version {
                let algo = cdchunking::MaybeOptimizedRAMChunker::new(window_size);
                chunk_with_algorithm_and_size_limit(
                    f,
                    algo,
                    cli.max_chunk_size,
                    cli.quiet,
                    cli.quickcdc_min_chunk_size,
                    cli.quickcdc_use_hashmap,
                    cli.quickcdc_front_feature_vector_length,
                    cli.quickcdc_end_feature_vector_length,
                )
            } else {
                let algo = cdchunking::RAMChunker::new(window_size);
                chunk_with_algorithm_and_size_limit(
                    f,
                    algo,
                    cli.max_chunk_size,
                    cli.quiet,
                    cli.quickcdc_min_chunk_size,
                    cli.quickcdc_use_hashmap,
                    cli.quickcdc_front_feature_vector_length,
                    cli.quickcdc_end_feature_vector_length,
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
                    cli.quiet,
                    cli.quickcdc_min_chunk_size,
                    cli.quickcdc_use_hashmap,
                    cli.quickcdc_front_feature_vector_length,
                    cli.quickcdc_end_feature_vector_length,
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
                cli.quiet,
                cli.quickcdc_min_chunk_size,
                cli.quickcdc_use_hashmap,
                cli.quickcdc_front_feature_vector_length,
                cli.quickcdc_end_feature_vector_length,
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
                cli.quiet,
                cli.quickcdc_min_chunk_size,
                cli.quickcdc_use_hashmap,
                cli.quickcdc_front_feature_vector_length,
                cli.quickcdc_end_feature_vector_length,
                2,
                3,
                4,
                5,
                6,
                7,
                8,
                9,
                10,
                11,
                12,
                13,
                14,
                15,
                16,
                342,
                603,
                285,
                88,
                183,
                48
            )
        }
        Commands::Gear { target_chunk_size } => {
            let mask_bits = (target_chunk_size as f64).log2().round() as u32;
            let mask = u32::MAX << (32 - mask_bits);

            let algo = cdchunking::GearChunker::new(mask);
            chunk_with_algorithm_and_size_limit(
                f,
                algo,
                cli.max_chunk_size,
                cli.quiet,
                cli.quickcdc_min_chunk_size,
                cli.quickcdc_use_hashmap,
                cli.quickcdc_front_feature_vector_length,
                cli.quickcdc_end_feature_vector_length,
            )
        }
        Commands::Gear64 {
            allow_simd_impl,
            target_chunk_size,
        } => {
            let mask_bits = (target_chunk_size as f64).log2().round() as u64;
            let mask = u64::MAX << (64 - mask_bits);

            if allow_simd_impl {
                let algo = gear::MaybeSimdGear64::new(mask);
                chunk_with_algorithm_and_size_limit(
                    f,
                    algo,
                    cli.max_chunk_size,
                    cli.quiet,
                    cli.quickcdc_min_chunk_size,
                    cli.quickcdc_use_hashmap,
                    cli.quickcdc_front_feature_vector_length,
                    cli.quickcdc_end_feature_vector_length,
                )
            } else {
                let algo = gear::ScalarGear64::new(mask);
                chunk_with_algorithm_and_size_limit(
                    f,
                    algo,
                    cli.max_chunk_size,
                    cli.quiet,
                    cli.quickcdc_min_chunk_size,
                    cli.quickcdc_use_hashmap,
                    cli.quickcdc_front_feature_vector_length,
                    cli.quickcdc_end_feature_vector_length,
                )
            }
        }
        Commands::NCGear {
            target_chunk_size,
            level,
        } => {
            let mask_bits = (target_chunk_size as f64).log2().round() as u32;
            let mask = u32::MAX << (32 - mask_bits);
            let lower_mask = mask << level;
            let upper_mask = (mask_bits >> level) | (1u32 << (32 - level));

            let algo = cdchunking::NormalizedChunkingGearChunker::new(
                lower_mask,
                upper_mask,
                target_chunk_size,
            );
            chunk_with_algorithm_and_size_limit(
                f,
                algo,
                cli.max_chunk_size,
                cli.quiet,
                cli.quickcdc_min_chunk_size,
                cli.quickcdc_use_hashmap,
                cli.quickcdc_front_feature_vector_length,
                cli.quickcdc_end_feature_vector_length,
            )
        }
    }
}

fn chunk_with_algorithm_and_size_limit<C: ChunkerImpl + Debug>(
    f: File,
    cdc_algo: C,
    size_limit: Option<usize>,
    quiet: bool,
    quickcdc_min_chunk_size: Option<usize>,
    quickcdc_use_hashmap: bool,
    quickcdc_n: u16,
    quickcdc_m: u16,
) -> anyhow::Result<()> {
    debug!(
        "will chunk using {:?} and a size limit of {:?}",
        cdc_algo, size_limit
    );
    if let Some(quickcdc_min_chunk_size) = quickcdc_min_chunk_size {
        if size_limit.is_some() {
            return Err(anyhow!(
                "it is currently impossible to employ both a size limit and the QuickCDC wrapper"
            ));
        }
        if quickcdc_use_hashmap {
            impl_quickcdc_hashmap_for_sizes!(
                cdc_algo,
                f,
                quickcdc_min_chunk_size,
                quiet,
                quickcdc_n,
                quickcdc_m,
                (1, 1),
                (1, 2),
                (1, 3),
                (1, 4),
                (2, 1),
                (2, 2),
                (2, 3),
                (2, 4),
                (3, 1),
                (3, 2),
                (3, 3),
                (3, 4),
                (4, 1),
                (4, 2),
                (4, 3),
                (4, 4)
            )
        } else {
            impl_quickcdc_for_sizes!(
                cdc_algo,
                f,
                quickcdc_min_chunk_size,
                quiet,
                quickcdc_n,
                quickcdc_m,
                (1, 1),
                (1, 2),
                (1, 3),
                (1, 4),
                (2, 1),
                (2, 2),
                (2, 3),
                (2, 4),
                (3, 1),
                (3, 2),
                (3, 3),
                (3, 4),
                (4, 1),
                (4, 2),
                (4, 3),
                (4, 4)
            )
        }
    } else {
        if let Some(size_limit) = size_limit {
            let chunker = Chunker::new(cdc_algo).max_size(size_limit);
            process_chunk_stream(chunker.stream(f), quiet)
        } else {
            let chunker = Chunker::new(cdc_algo);
            process_chunk_stream(chunker.stream(f), quiet)
        }
    }
}

macro_rules! impl_chunk_consumer {
    ($driver:expr,$quiet:expr) => {
        let mut total_size = 0;
        let mut chunk_size = 0;
        let mut chunk_hasher = Sha1::new();

        while let Some(chunk) = $driver.read() {
            let chunk = chunk.context("unable to read file")?;

            // Make sure that the compiler cannot optimize out anything about the chunks produced.
            std::hint::black_box(&chunk);

            match chunk {
                ChunkInput::Data(d) => {
                    debug!("got {} bytes of data for the current chunk...", d.len());
                    chunk_size += d.len();
                    if !$quiet {
                        chunk_hasher.update(d);
                    }
                }
                ChunkInput::End => {
                    if !$quiet {
                        let digest = format!("{:x}", chunk_hasher.finalize_reset());
                        debug!("chunk complete, size: {}, digest: {}", chunk_size, digest);
                        println!("{},{}", digest, chunk_size);
                    } else {
                        debug!("chunk complete, size: {}", chunk_size);
                    }
                    total_size += chunk_size;
                    chunk_size = 0;
                }
            }
        }
        if $quiet {
            println!("{}", total_size);
        }
    };
}

fn process_quickcdc_wrapper<const N: usize, const M: usize, C: ChunkerImpl, R: Read>(
    algo: C,
    file: R,
    min_chunk_size: usize,
    quiet: bool,
) -> anyhow::Result<()>
where
    [(); 256_usize.pow(N as u32)]:,
    [(); 256_usize.pow(M as u32)]:,
{
    let mut wrapper = QuickCDCWrapperDeque::<N, M, _, _>::new(algo, file, min_chunk_size);

    impl_chunk_consumer!(wrapper, quiet);

    Ok(())
}

fn process_quickcdc_wrapper_hashmap<const N: usize, const M: usize, C: ChunkerImpl, R: Read>(
    algo: C,
    file: R,
    min_chunk_size: usize,
    quiet: bool,
) -> anyhow::Result<()> {
    let mut wrapper = QuickCDCWrapperWithHashMap::<N, M, _, _>::new(algo, file, min_chunk_size);

    impl_chunk_consumer!(wrapper, quiet);

    Ok(())
}

fn process_chunk_stream<C: ChunkerImpl, R: Read>(
    mut chunk_iterator: ChunkStream<R, C>,
    quiet: bool,
) -> anyhow::Result<()> {
    impl_chunk_consumer!(chunk_iterator, quiet);

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
