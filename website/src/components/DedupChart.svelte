<script>
    import { Line } from 'svelte-chartjs';
    import dedupData from '../data/dedup.json'
    import {
        Chart as ChartJS,
        Title,
        Tooltip,
        Legend,
        LineElement,
        LinearScale,
        PointElement,
        CategoryScale,
		LogarithmicScale,
    } from 'chart.js';
    
    ChartJS.register(
        Title,
        Tooltip,
        Legend,
        LineElement,
        LinearScale,
        PointElement,
        CategoryScale,
        LogarithmicScale,
    );
    
    
    const datasets = ['RAND', 'LNX', 'PDF', 'WEB', 'CODE'];
    const targets = [512, 737, 1024, 2048, 4096, 5152, 8192];
    
    export let dataset;
    export let target;
    // @ts-ignore
    let data = { labels: [], values: [] };
    
    const algorithms = {
        'Rabin': '#E6194B',    // Red
        'Buzhash': '#3CB44B',  // Green
        'Gear': '#FFE119',     // Yellow
        'Gear NC-1': '#4363D8', // Blue
        'Gear NC-2': '#F58231', // Orange
        'Gear NC-3': '#911EB4', // Purple
        'AE': '#42D4F4',       // Cyan
        'RAM': '#F032E6',      // Magenta
        'MII': '#BFEF45',      // Lime
        'PCI': '#FABED4',      // Pink
        'BFBC': '#469990',     // Teal
        'BFBC*': '#DCBEFF'     // Lavender
    };

    const dataByAlgorithm = {};
    dedupData.forEach(item => {
        if (!dataByAlgorithm[item.algorithm]) {
            dataByAlgorithm[item.algorithm] = [];
        }
        dataByAlgorithm[item.algorithm].push(item);
    });
    
    $: maxChunkSize = target;
    $: binSize = Math.ceil(maxChunkSize / 200);
    $: numBins = Math.ceil(maxChunkSize / binSize);
    
    // @ts-ignore
    $: data = {
        labels: targets,
        datasets: Object.keys(algorithms).map(algorithm => {
            const data = targets.map(target => {
                const entry = dedupData.find(item => item.algorithm == algorithm && item.dataset == dataset && item.target == target.toString());
                return entry ? parseFloat(entry.dedup_ratio) : NaN; // Use NaN for missing data points
            });

            return {
                label: algorithm,
                data: dataByAlgorithm[algorithm].map(item => parseFloat(item.dedup_ratio)),
                fill: false,
                borderColor: algorithms[algorithm],
                tension: 0.1
            };
        })
    };
    
    </script>
        
    
    <Line {data} options={{ 
        responsive: true,
        pointStyle: 'crossRot',
        scales: {
            y: {
                beginAtZero: true,
                title: {
                    display: true,
                    text: 'Probability Density'
                }
            },
            x: {
                title: {
                    display: true,
                    text: 'Chunk Size (B)'
                },
                ticks: {
                    autoSkip: true,
                    maxTicksLimit: 20
                }
            }
        }
    }} />    