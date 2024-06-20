<script>
import { Line } from 'svelte-chartjs';
import csdData from '../data/csd.json'
import {
    Chart as ChartJS,
    Title,
    Tooltip,
    Legend,
    LineElement,
    LinearScale,
    PointElement,
    CategoryScale,
} from 'chart.js';

ChartJS.register(
    Title,
    Tooltip,
    Legend,
    LineElement,
    LinearScale,
    PointElement,
    CategoryScale
);


const datasets = ['RAND', 'LNX', 'PDF', 'WEB', 'CODE'];
const targets = [512, 770, 1024, 2048, 4096, 5482, 8192];

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

$: maxChunkSize = target * 3;
$: binSize = Math.ceil(maxChunkSize / 200);
$: numBins = Math.ceil(maxChunkSize / binSize);


// @ts-ignore
$: data = {
    labels: Array.from({ length: numBins }, (_, index) => index * binSize),
    datasets: csdData.filter(item =>
        item.dataset === dataset &&
        item.target == target.toString()
    ).map(item => ({
        label: item.algorithm,
        data: item.pdf,
        fill: false,
        // @ts-ignore
        borderColor: algorithms[item.algorithm],
        tension: 0.1
    }))
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