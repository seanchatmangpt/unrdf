# Benchmark Dashboard Integration Guide

## Overview

The Knowledge Hooks Performance Dashboard is an interactive HTML-based visualization tool that transforms raw benchmark metrics into actionable performance insights. It's automatically generated from benchmark results and integrates seamlessly with the hyper-advanced 80/20 benchmarking suite.

## Features

✨ **Interactive Visualizations**
- Real-time chart updates with Chart.js
- Multiple data dimensions (latency, throughput, memory, overhead)
- Responsive grid layout (600px+ per chart)

📊 **Comprehensive Metrics**
- Mean, P50, P95, P99 latency percentiles
- Throughput (ops/sec) tracking
- Performance overhead percentage
- Per-benchmark and aggregated statistics

📋 **Data Export**
- Download results as CSV for further analysis
- Sortable table with all benchmark details
- Status badges (PASS/FAIL) for visual clarity

🎨 **Professional Design**
- Gradient purple background (#667eea to #764ba2)
- Clean card-based layout with shadows
- Color-coded metrics for quick scanning
- Mobile-responsive design

## Quick Start

### Run Benchmarks + Generate Dashboard

```bash
# Run benchmarks with GC profiling and auto-generate dashboard
pnpm --filter unrdf-benchmarks bench

# Or from benchmarks directory
cd benchmarks
pnpm bench
```

This runs:
1. All 5 core benchmarks with GC profiling
2. Aggregates results to `benchmarks/results/benchmark-results.json`
3. Converts to dashboard format
4. Generates `benchmarks/dashboard.html`

### Generate Dashboard Only

If you already have benchmark results and want to regenerate the dashboard:

```bash
# From repository root
pnpm --filter unrdf-benchmarks dashboard

# Or from benchmarks directory
cd benchmarks
pnpm dashboard
```

### Open Dashboard in Browser

```bash
# Auto-generate and open (macOS/Linux)
pnpm --filter unrdf-benchmarks open-dashboard

# Manual: Open in your browser
file:///home/user/unrdf/benchmarks/dashboard.html
```

## Dashboard Sections

### Header Metadata
- Generated timestamp
- Node.js version
- Platform information
- Total benchmarks and tests executed

### Summary Statistics
- **Mean Latency**: Average execution time across all benchmarks
- **P95 Latency**: 95th percentile latency (indicates "bad luck" scenarios)
- **P99 Latency**: 99th percentile latency (worst case common cases)
- **Tests Passed**: Count of passing vs total tests (visual badge: green for passed, red for failed)

### Performance Charts

#### 1. Latency by Benchmark (Mean)
- Bar chart showing average latency per benchmark
- Color-coded by benchmark type
- Y-axis: Latency in milliseconds

#### 2. Throughput Performance
- Line chart showing operations/second trend
- Displays throughput for each benchmark
- Useful for identifying scalability issues

#### 3. Latency Distribution (P50/P95/P99)
- Grouped bar chart for percentile comparison
- Shows spread between mean and worst-case scenarios
- Identifies outliers and variance

#### 4. Performance Overhead %
- Percentage overhead by benchmark
- Highlights which benchmarks add most overhead
- Lower = better performance

### Results Table

Detailed table with sortable columns:
- **Benchmark**: Test name (hook-registration, hook-execution-latency, etc.)
- **Scenario**: Test scenario (small/medium/large, simple/complex, etc.)
- **Mean Latency**: Average execution time
- **P95/P99 Latency**: Percentile latencies
- **Throughput**: Operations per second
- **Overhead %**: Relative performance impact
- **Status**: PASS/FAIL badge

Click column headers to sort ascending/descending.

## Data Format

The dashboard reads from `benchmarks/results/benchmark-results.json`:

```json
{
  "timestamp": "2025-12-04T20:33:13.601Z",
  "summary": {
    "totalBenchmarks": 5,
    "totalTests": 15,
    "passed": 11,
    "failed": 4
  },
  "benchmarks": [
    {
      "name": "hook-registration",
      "tests": [
        {
          "scale": "small",
          "latency": {
            "mean": 0.37,
            "p50": 0.19,
            "p95": 1.62,
            "p99": 1.62
          },
          "throughputHooksPerSec": 2119.46,
          "passed": false
        }
      ]
    }
  ]
}
```

The `generate-dashboard.mjs` script converts this to the dashboard data format automatically.

## Interpreting Results

### Latency Analysis
- **Green (< 10ms)**: Excellent performance
- **Yellow (10-50ms)**: Good performance
- **Orange (50-100ms)**: Acceptable but monitor
- **Red (> 100ms)**: Investigate bottlenecks

### Throughput Analysis
- **Higher is better**: More operations per second = better performance
- **Compare across benchmarks**: Identify which scenarios scale best
- **Trend analysis**: Watch for degradation with larger datasets

### Overhead Percentage
- **< 10%**: Minimal overhead
- **10-20%**: Acceptable overhead
- **20-50%**: Notable overhead, investigate optimization
- **> 50%**: High overhead, priority optimization target

## Integration with CI/CD

The dashboard generation is automatic and can be integrated into GitHub Actions:

```yaml
- name: Generate Performance Dashboard
  run: pnpm --filter unrdf-benchmarks bench

- name: Upload Dashboard Artifacts
  uses: actions/upload-artifact@v3
  with:
    name: performance-dashboard
    path: benchmarks/dashboard.html
```

Results are stored in `benchmarks/results/benchmark-results.json` for:
- Baseline comparison
- Regression detection
- Historical tracking

## Custom Analysis

### CSV Export
Click "📥 Export to CSV" button to download all results in CSV format:

```csv
Benchmark,Scenario,Mean Latency (ms),P95 Latency (ms),P99 Latency (ms),Throughput (ops/sec),Overhead %,Status
hook-registration,small,0.38,1.62,1.62,2119.46,426.32,FAIL
hook-execution-latency,simple,0.07,0.08,0.16,13168.19,149.95,PASS
```

### Spreadsheet Analysis
Import CSV into Excel/Google Sheets for:
- Pivot tables by benchmark type
- Trend analysis over time
- Statistical modeling
- Regression detection

### External Visualization Tools
Use Chart.js export data for tools like:
- Grafana (with time-series data)
- Kibana (with Elasticsearch backend)
- Tableau (for advanced analytics)

## Troubleshooting

### Dashboard Not Generated
```bash
# Check if benchmark results exist
ls -la benchmarks/results/benchmark-results.json

# If missing, run benchmarks first
pnpm --filter unrdf-benchmarks bench:metrics-only
```

### Charts Not Rendering
- Ensure Chart.js CDN is accessible (requires internet)
- Check browser console for JavaScript errors
- Try opening in different browser
- Verify benchmark data format with: `cat benchmarks/results/benchmark-results.json | jq .`

### Data Seems Off
1. Clear browser cache (Ctrl+Shift+Delete)
2. Regenerate dashboard: `pnpm dashboard`
3. Verify benchmark file wasn't corrupted
4. Re-run benchmarks from scratch

## Performance Optimization Workflow

1. **Run Baseline**: `pnpm --filter unrdf-benchmarks bench`
2. **Analyze Dashboard**: Open `benchmarks/dashboard.html`
3. **Identify Bottlenecks**: Look for high overhead % or low throughput
4. **Implement Optimizations**: Based on critical paths identified
5. **Run Benchmarks Again**: Generate new dashboard
6. **Compare Results**: Look for improvement percentages

## File Structure

```
benchmarks/
├── dashboard.html                 # Generated dashboard (regenerated after each bench run)
├── generate-dashboard.mjs         # Dashboard generator script
├── results/
│   └── benchmark-results.json    # Raw benchmark results (input for dashboard)
└── README.md                      # Benchmark usage guide
```

## Next Steps

- 🎯 Set up automated CI/CD pipeline for continuous performance monitoring
- 📊 Create historical tracking dashboard (multiple benchmark runs over time)
- 🔍 Implement regression detection (alert on >10% latency increase)
- 🚀 Benchmark optimization implementations against baseline
- 📈 Export metrics to observability platform (Grafana, Datadog, etc.)

## Advanced Customization

To modify dashboard appearance or add new charts:

1. Edit `benchmarks/generate-dashboard.mjs`
2. Modify the `generateDashboard()` function
3. Add new Chart.js charts or modify existing ones
4. Regenerate: `pnpm --filter unrdf-benchmarks dashboard`

## Support

For issues with:
- **Benchmarks**: See `/benchmarks/README.md`
- **Architecture**: See `/docs/benchmark-architecture-80-20.md`
- **CLI**: See `/benchmarks/QUICK-REFERENCE.md`

---

**Last Updated**: 2025-12-04
**Dashboard Version**: 1.0.0 (80/20 Integration)
