/**
 * @fileoverview Performance Profile Reporter
 * @module profiling/reporter
 */

import { writeFileSync } from 'node:fs';
import { join } from 'node:path';

/**
 * Performance Profile Reporter
 */
export class Reporter {
  /**
   * Format profile as JSON
   * @param {Object} profile - Profile result
   * @param {Object} [options={}] - Format options
   * @param {boolean} [options.pretty=true] - Pretty print
   * @returns {string} JSON string
   */
  static toJSON(profile, options = {}) {
    const pretty = options.pretty !== false;
    return JSON.stringify(profile, null, pretty ? 2 : 0);
  }

  /**
   * Format profile as terminal output
   * @param {Object} profile - Profile result
   * @param {Object} [options={}] - Format options
   * @returns {string} Terminal formatted string
   */
  static toTerminal(profile, options = {}) {
    const lines = [];
    const { metadata, latency, memory, cpu } = profile;

    lines.push('');
    lines.push('═'.repeat(80));
    lines.push(`  Performance Profile: ${metadata.operationName}`);
    lines.push('═'.repeat(80));
    lines.push('');

    // Latency section
    if (latency) {
      lines.push('📊 Latency Metrics:');
      lines.push(`  Duration:    ${latency.duration.toFixed(2)} ms`);
      lines.push(`  Mean:        ${latency.mean.toFixed(2)} ms`);
      lines.push(`  Std Dev:     ${latency.stddev.toFixed(2)} ms`);
      lines.push('  Percentiles:');
      lines.push(`    p50:       ${latency.p50.toFixed(2)} ms`);
      lines.push(`    p90:       ${latency.p90.toFixed(2)} ms`);
      lines.push(`    p95:       ${latency.p95.toFixed(2)} ms`);
      lines.push(`    p99:       ${latency.p99.toFixed(2)} ms`);
      lines.push(`    p999:      ${latency.p999.toFixed(2)} ms`);
      lines.push('');

      // Histogram
      if (latency.histogram) {
        lines.push('  Histogram:');
        const buckets = Object.entries(latency.histogram);
        const maxCount = Math.max(...buckets.map(([, count]) => count));
        buckets.forEach(([bucket, count]) => {
          const bar = '█'.repeat(Math.ceil((count / maxCount) * 40));
          lines.push(`    ${bucket.padStart(6)} ms: ${bar} ${count}`);
        });
        lines.push('');
      }
    }

    // Memory section
    if (memory) {
      lines.push('💾 Memory Metrics:');
      lines.push(`  Heap Delta:  ${this.formatBytes(memory.heapUsedDelta)}`);
      lines.push(`  Heap Peak:   ${this.formatBytes(memory.heapUsedPeak)}`);
      lines.push(`  RSS:         ${this.formatBytes(memory.rss)}`);

      if (memory.trend) {
        const arrow = memory.trend.direction === 'growing' ? '↗' :
                     memory.trend.direction === 'shrinking' ? '↘' : '→';
        lines.push(`  Trend:       ${arrow} ${memory.trend.direction} (${this.formatBytes(memory.trend.growthRate)}/s)`);
      }

      if (memory.leakDetected) {
        lines.push('  ⚠️  WARNING: Potential memory leak detected!');
      }
      lines.push('');
    }

    // CPU section
    if (cpu && cpu.hotFunctions) {
      lines.push('🔥 CPU Hot Functions (Top 10):');
      cpu.hotFunctions.slice(0, 10).forEach((fn, index) => {
        lines.push(`  ${(index + 1).toString().padStart(2)}. ${fn.name.slice(0, 60)}`);
        lines.push(`      ${fn.selfTimePercent.toFixed(2)}% | ${(fn.selfTime / 1000).toFixed(2)} ms | ${fn.hitCount} hits`);
      });
      lines.push('');
    }

    lines.push('═'.repeat(80));
    lines.push('');

    return lines.join('\n');
  }

  /**
   * Format profile as HTML
   * @param {Object} profile - Profile result
   * @param {Object} [options={}] - Format options
   * @returns {string} HTML string
   */
  static toHTML(profile, options = {}) {
    const { metadata, latency, memory, cpu } = profile;
    const title = options.title || `Performance Profile: ${metadata.operationName}`;

    const html = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${title}</title>
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      max-width: 1200px;
      margin: 0 auto;
      padding: 20px;
      background: #f5f5f5;
    }
    .header {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 30px;
      border-radius: 8px;
      margin-bottom: 20px;
    }
    .section {
      background: white;
      padding: 20px;
      border-radius: 8px;
      margin-bottom: 20px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    .metric {
      display: flex;
      justify-content: space-between;
      padding: 10px 0;
      border-bottom: 1px solid #eee;
    }
    .metric:last-child { border-bottom: none; }
    .metric-label { font-weight: 600; color: #666; }
    .metric-value { font-family: 'Courier New', monospace; }
    .histogram {
      margin-top: 20px;
    }
    .histogram-bar {
      display: flex;
      align-items: center;
      margin: 5px 0;
    }
    .histogram-label {
      width: 80px;
      font-size: 12px;
      color: #666;
    }
    .histogram-fill {
      height: 20px;
      background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
      border-radius: 4px;
      margin: 0 10px;
    }
    .histogram-count {
      font-size: 12px;
      color: #666;
    }
    .hot-function {
      padding: 10px;
      margin: 5px 0;
      background: #f9f9f9;
      border-left: 3px solid #667eea;
      font-size: 14px;
    }
    .warning {
      background: #fff3cd;
      border: 1px solid #ffc107;
      color: #856404;
      padding: 15px;
      border-radius: 4px;
      margin: 10px 0;
    }
  </style>
</head>
<body>
  <div class="header">
    <h1>${metadata.operationName}</h1>
    <p>Profiled at: ${new Date(metadata.timestamp).toLocaleString()}</p>
  </div>

  ${latency ? `
  <div class="section">
    <h2>📊 Latency Metrics</h2>
    <div class="metric">
      <span class="metric-label">Duration</span>
      <span class="metric-value">${latency.duration.toFixed(2)} ms</span>
    </div>
    <div class="metric">
      <span class="metric-label">Mean</span>
      <span class="metric-value">${latency.mean.toFixed(2)} ms</span>
    </div>
    <div class="metric">
      <span class="metric-label">p50 (Median)</span>
      <span class="metric-value">${latency.p50.toFixed(2)} ms</span>
    </div>
    <div class="metric">
      <span class="metric-label">p90</span>
      <span class="metric-value">${latency.p90.toFixed(2)} ms</span>
    </div>
    <div class="metric">
      <span class="metric-label">p95</span>
      <span class="metric-value">${latency.p95.toFixed(2)} ms</span>
    </div>
    <div class="metric">
      <span class="metric-label">p99</span>
      <span class="metric-value">${latency.p99.toFixed(2)} ms</span>
    </div>
    ${latency.histogram ? this.renderHistogram(latency.histogram) : ''}
  </div>
  ` : ''}

  ${memory ? `
  <div class="section">
    <h2>💾 Memory Metrics</h2>
    <div class="metric">
      <span class="metric-label">Heap Delta</span>
      <span class="metric-value">${this.formatBytes(memory.heapUsedDelta)}</span>
    </div>
    <div class="metric">
      <span class="metric-label">Peak Heap</span>
      <span class="metric-value">${this.formatBytes(memory.heapUsedPeak)}</span>
    </div>
    <div class="metric">
      <span class="metric-label">RSS</span>
      <span class="metric-value">${this.formatBytes(memory.rss)}</span>
    </div>
    ${memory.trend ? `
    <div class="metric">
      <span class="metric-label">Trend</span>
      <span class="metric-value">${memory.trend.direction} (${this.formatBytes(memory.trend.growthRate)}/s)</span>
    </div>
    ` : ''}
    ${memory.leakDetected ? '<div class="warning">⚠️ Potential memory leak detected!</div>' : ''}
  </div>
  ` : ''}

  ${cpu && cpu.hotFunctions ? `
  <div class="section">
    <h2>🔥 CPU Hot Functions</h2>
    ${cpu.hotFunctions.slice(0, 10).map((fn, i) => `
      <div class="hot-function">
        <strong>${i + 1}. ${this.escapeHtml(fn.name)}</strong><br>
        ${fn.selfTimePercent.toFixed(2)}% | ${(fn.selfTime / 1000).toFixed(2)} ms | ${fn.hitCount} hits
      </div>
    `).join('')}
  </div>
  ` : ''}
</body>
</html>`;

    return html;
  }

  /**
   * Render histogram as HTML
   * @private
   */
  static renderHistogram(histogram) {
    const buckets = Object.entries(histogram);
    const maxCount = Math.max(...buckets.map(([, count]) => count));

    return `
    <div class="histogram">
      <h3>Distribution</h3>
      ${buckets.map(([bucket, count]) => {
        const width = (count / maxCount) * 100;
        return `
        <div class="histogram-bar">
          <span class="histogram-label">${bucket} ms</span>
          <div class="histogram-fill" style="width: ${width}%"></div>
          <span class="histogram-count">${count}</span>
        </div>
        `;
      }).join('')}
    </div>
    `;
  }

  /**
   * Save profile to file
   * @param {Object} profile - Profile result
   * @param {string} filename - Output filename
   * @param {Object} [options={}] - Save options
   * @param {string} [options.format='json'] - Output format: 'json', 'html', 'txt'
   * @param {string} [options.dir='.'] - Output directory
   */
  static save(profile, filename, options = {}) {
    const format = options.format || 'json';
    const dir = options.dir || '.';
    const filepath = join(dir, filename);

    let content;
    switch (format) {
      case 'json':
        content = this.toJSON(profile, options);
        break;
      case 'html':
        content = this.toHTML(profile, options);
        break;
      case 'txt':
        content = this.toTerminal(profile, options);
        break;
      default:
        throw new Error(`Unknown format: ${format}`);
    }

    writeFileSync(filepath, content, 'utf8');
    return filepath;
  }

  /**
   * Format bytes as human readable
   * @private
   */
  static formatBytes(bytes) {
    if (Math.abs(bytes) < 1024) return bytes + ' B';
    const units = ['KB', 'MB', 'GB'];
    let u = -1;
    do {
      bytes /= 1024;
      ++u;
    } while (Math.abs(bytes) >= 1024 && u < units.length - 1);
    return bytes.toFixed(2) + ' ' + units[u];
  }

  /**
   * Escape HTML
   * @private
   */
  static escapeHtml(text) {
    const map = {
      '&': '&amp;',
      '<': '&lt;',
      '>': '&gt;',
      '"': '&quot;',
      "'": '&#039;'
    };
    return text.replace(/[&<>"']/g, m => map[m]);
  }

  /**
   * Compare two profiles
   * @param {Object} baseline - Baseline profile
   * @param {Object} current - Current profile
   * @returns {Object} Comparison results
   */
  static compare(baseline, current) {
    const comparison = {
      operationName: current.metadata.operationName,
      regression: false,
      improvements: [],
      regressions: []
    };

    // Compare latency
    if (baseline.latency && current.latency) {
      const latencyChange = ((current.latency.p95 - baseline.latency.p95) / baseline.latency.p95) * 100;

      if (latencyChange > 10) {
        comparison.regression = true;
        comparison.regressions.push({
          metric: 'latency.p95',
          baseline: baseline.latency.p95,
          current: current.latency.p95,
          change: latencyChange
        });
      } else if (latencyChange < -10) {
        comparison.improvements.push({
          metric: 'latency.p95',
          baseline: baseline.latency.p95,
          current: current.latency.p95,
          change: latencyChange
        });
      }
    }

    // Compare memory
    if (baseline.memory && current.memory) {
      const memoryChange = ((current.memory.heapUsedDelta - baseline.memory.heapUsedDelta) /
                           Math.abs(baseline.memory.heapUsedDelta)) * 100;

      if (memoryChange > 20) {
        comparison.regression = true;
        comparison.regressions.push({
          metric: 'memory.heapUsedDelta',
          baseline: baseline.memory.heapUsedDelta,
          current: current.memory.heapUsedDelta,
          change: memoryChange
        });
      }
    }

    return comparison;
  }
}
