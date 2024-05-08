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
	import CsdChart from '../components/CSDChart.svelte';
	import DedupChart from '../components/DedupChart.svelte';

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
const targets = [512, 737, 1024, 2048, 4096, 5152, 8192];

let dataset = 'RAND';
let target = 512;

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

</script>
  
<main>
    <label for="dataset-select">Dataset:</label>
    <select id="dataset-select" bind:value={dataset}>
        {#each datasets as dataset}
            <option value={dataset}>{dataset}</option>
        {/each}
    </select>

    <label for="target-chunk-size-select">Target chunk size:</label>
    <select id="target-chunk-size-select" bind:value={target}>
        {#each targets as target}
            <option value={target}>{target}</option>
        {/each}
    </select>

    <CsdChart {dataset} {target} />
    <DedupChart {dataset} {target} />
</main>
  