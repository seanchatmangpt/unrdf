/**
 * @fileoverview Real-time Performance Dashboard - ASCII art live monitoring
 * @module monitoring/dashboard
 *
 * @description
 * Live ASCII dashboard using ANSI escape codes:
 * - Real-time metrics display (CPU, memory, throughput)
 * - Agent status grid (α₁...α₁₀)
 * - Latency distribution sparklines
 * - Alert notifications
 * - No external dependencies (pure ANSI)
 */

/**
 * ANSI escape codes for terminal control
 */
const ANSI = {
  // Cursor control
  CLEAR_SCREEN: '\x1b[2J',
  CLEAR_LINE: '\x1b[2K',
  CURSOR_HOME: '\x1b[H',
  CURSOR_UP: (n) => `\x1b[${n}A`,
  CURSOR_DOWN: (n) => `\x1b[${n}B`,
  CURSOR_HIDE: '\x1b[?25l',
  CURSOR_SHOW: '\x1b[?25h',

  // Colors
  RESET: '\x1b[0m',
  BOLD: '\x1b[1m',
  DIM: '\x1b[2m',
  RED: '\x1b[31m',
  GREEN: '\x1b[32m',
  YELLOW: '\x1b[33m',
  BLUE: '\x1b[34m',
  MAGENTA: '\x1b[35m',
  CYAN: '\x1b[36m',
  WHITE: '\x1b[37m',
  BG_RED: '\x1b[41m',
  BG_GREEN: '\x1b[42m',
  BG_YELLOW: '\x1b[43m',
};

/**
 * Real-time performance dashboard
 */
export class Dashboard {
  /**
   * Create a new dashboard
   * @param {object} metricsCollector - MetricsCollector instance
   * @param {object} options - Configuration options
   * @param {number} options.refreshRate - Refresh rate in ms (default: 1000)
   * @param {number} options.width - Terminal width (default: 120)
   * @param {number} options.height - Terminal height (default: 40)
   */
  constructor(metricsCollector, options = {}) {
    this.metrics = metricsCollector;
    this.refreshRate = options.refreshRate || 1000;
    this.width = options.width || 120;
    this.height = options.height || 40;

    /** @type {number|null} */
    this.intervalId = null;

    /** @type {string[]} */
    this.alerts = [];

    /** @type {boolean} */
    this.isRunning = false;
  }

  /**
   * Start the dashboard
   */
  start() {
    if (this.isRunning) return;

    this.isRunning = true;

    // Clear screen and hide cursor
    process.stdout.write(ANSI.CLEAR_SCREEN + ANSI.CURSOR_HIDE);

    // Setup graceful shutdown
    process.on('SIGINT', () => this.stop());
    process.on('SIGTERM', () => this.stop());

    // Start refresh loop
    this.intervalId = setInterval(() => {
      this.render();
    }, this.refreshRate);

    // Initial render
    this.render();

    console.log('[Dashboard] Started');
  }

  /**
   * Stop the dashboard
   */
  stop() {
    if (!this.isRunning) return;

    this.isRunning = false;

    if (this.intervalId !== null) {
      clearInterval(this.intervalId);
      this.intervalId = null;
    }

    // Show cursor and clear screen
    process.stdout.write(ANSI.CURSOR_SHOW + ANSI.CLEAR_SCREEN + ANSI.CURSOR_HOME);

    console.log('[Dashboard] Stopped');
    process.exit(0);
  }

  /**
   * Render the dashboard
   */
  render() {
    const snapshot = this.metrics.getCurrentSnapshot();
    if (!snapshot) {
      return;
    }

    let output = ANSI.CURSOR_HOME;

    // Header
    output += this.renderHeader();
    output += '\n';

    // System metrics
    output += this.renderSystemMetrics(snapshot);
    output += '\n';

    // Performance metrics
    output += this.renderPerformanceMetrics(snapshot);
    output += '\n';

    // Agent grid
    output += this.renderAgentGrid();
    output += '\n';

    // Latency distribution
    output += this.renderLatencyDistribution(snapshot);
    output += '\n';

    // Alerts
    output += this.renderAlerts();
    output += '\n';

    // Footer
    output += this.renderFooter();

    process.stdout.write(output);
  }

  /**
   * Render header
   * @returns {string}
   */
  renderHeader() {
    const title = 'UNRDF Performance Monitor';
    const timestamp = new Date().toISOString();
    const uptime = this.formatDuration(this.metrics.getUptime());

    const titleLine = this.centerText(title, this.width);
    const infoLine = `${timestamp}  │  Uptime: ${uptime}`;

    return (
      ANSI.BOLD + ANSI.CYAN +
      '═'.repeat(this.width) + ANSI.RESET + '\n' +
      ANSI.BOLD + titleLine + ANSI.RESET + '\n' +
      ANSI.DIM + this.centerText(infoLine, this.width) + ANSI.RESET + '\n' +
      ANSI.BOLD + ANSI.CYAN +
      '═'.repeat(this.width) + ANSI.RESET
    );
  }

  /**
   * Render system metrics
   * @param {object} snapshot - Metrics snapshot
   * @returns {string}
   */
  renderSystemMetrics(snapshot) {
    const cpuPercent = snapshot.cpu.percentageUser + snapshot.cpu.percentageSystem;
    const memUsedMB = (snapshot.memory.heapUsed / 1024 / 1024).toFixed(1);
    const memTotalMB = (snapshot.memory.heapTotal / 1024 / 1024).toFixed(1);
    const memPercent = (snapshot.memory.heapUsed / snapshot.memory.heapTotal * 100).toFixed(1);

    const cpuBar = this.renderBar(cpuPercent, 100, 40);
    const memBar = this.renderBar(memPercent, 100, 40);

    return (
      ANSI.BOLD + '┌─ System Metrics ' + '─'.repeat(this.width - 18) + '┐' + ANSI.RESET + '\n' +
      `│ ${ANSI.BOLD}CPU:${ANSI.RESET}    ${cpuBar} ${cpuPercent.toFixed(1)}%` + ' '.repeat(this.width - 60) + '│\n' +
      `│ ${ANSI.BOLD}Memory:${ANSI.RESET} ${memBar} ${memUsedMB}/${memTotalMB} MB (${memPercent}%)` + ' '.repeat(this.width - 75) + '│\n' +
      ANSI.BOLD + '└' + '─'.repeat(this.width - 2) + '┘' + ANSI.RESET
    );
  }

  /**
   * Render performance metrics
   * @param {object} snapshot - Metrics snapshot
   * @returns {string}
   */
  renderPerformanceMetrics(snapshot) {
    const throughput = snapshot.throughput.opsPerSec.toFixed(2);
    const avgThroughput = this.metrics.getAverageThroughput(60000).toFixed(2);
    const p50 = snapshot.latency.p50.toFixed(2);
    const p95 = snapshot.latency.p95.toFixed(2);
    const p99 = snapshot.latency.p99.toFixed(2);
    const compression = snapshot.compression.ratio.toFixed(2);
    const receiptRate = snapshot.receiptChain.growthRate.toFixed(2);

    return (
      ANSI.BOLD + '┌─ Performance Metrics ' + '─'.repeat(this.width - 23) + '┐' + ANSI.RESET + '\n' +
      `│ ${ANSI.BOLD}Throughput:${ANSI.RESET}      ${this.colorize(throughput, 100, 50, 10)} ops/sec  │  ` +
      `${ANSI.BOLD}Avg (1m):${ANSI.RESET} ${avgThroughput} ops/sec` + ' '.repeat(this.width - 90) + '│\n' +
      `│ ${ANSI.BOLD}Latency:${ANSI.RESET}         P50: ${p50}ms  │  P95: ${p95}ms  │  P99: ${p99}ms` +
      ' '.repeat(this.width - 80) + '│\n' +
      `│ ${ANSI.BOLD}Compression:${ANSI.RESET}     ${compression}x  │  ` +
      `${ANSI.BOLD}Receipt Rate:${ANSI.RESET} ${receiptRate}/sec` +
      ' '.repeat(this.width - 75) + '│\n' +
      ANSI.BOLD + '└' + '─'.repeat(this.width - 2) + '┘' + ANSI.RESET
    );
  }

  /**
   * Render agent grid
   * @returns {string}
   */
  renderAgentGrid() {
    const agentMetrics = this.metrics.getAllAgentMetrics();
    const agentIds = ['α₁', 'α₂', 'α₃', 'α₄', 'α₅', 'α₆', 'α₇', 'α₈', 'α₉', 'α₁₀'];

    let output = ANSI.BOLD + '┌─ Agent Status ' + '─'.repeat(this.width - 16) + '┐' + ANSI.RESET + '\n';

    // Header
    output += '│ ' + ANSI.BOLD + 'Agent'.padEnd(8) + 'Tasks'.padEnd(10) + 'Queue'.padEnd(10) +
      'CPU (μs)'.padEnd(12) + 'Memory (KB)'.padEnd(15) + 'Avg Latency' + ANSI.RESET +
      ' '.repeat(this.width - 77) + '│\n';

    // Agent rows
    for (const agentId of agentIds) {
      const metrics = agentMetrics.get(agentId) || {
        tasksCompleted: 0,
        tasksQueued: 0,
        cpuTime: 0,
        memoryUsed: 0,
        avgLatency: 0,
      };

      const status = metrics.tasksQueued > 0 ? ANSI.GREEN + '●' + ANSI.RESET : ANSI.DIM + '○' + ANSI.RESET;
      const tasks = String(metrics.tasksCompleted).padEnd(10);
      const queue = String(metrics.tasksQueued).padEnd(10);
      const cpu = String(metrics.cpuTime).padEnd(12);
      const memory = (metrics.memoryUsed / 1024).toFixed(1).padEnd(15);
      const latency = metrics.avgLatency.toFixed(2) + ' ms';

      output += `│ ${status} ${agentId.padEnd(6)}${tasks}${queue}${cpu}${memory}${latency}` +
        ' '.repeat(this.width - 77) + '│\n';
    }

    output += ANSI.BOLD + '└' + '─'.repeat(this.width - 2) + '┘' + ANSI.RESET;
    return output;
  }

  /**
   * Render latency distribution as sparkline
   * @param {object} snapshot - Metrics snapshot
   * @returns {string}
   */
  renderLatencyDistribution(snapshot) {
    const snapshots = this.metrics.getAllSnapshots();
    const latencies = snapshots.slice(-60).map(s => s.latency.p95); // Last 60 samples

    const sparkline = this.renderSparkline(latencies, 80);

    return (
      ANSI.BOLD + '┌─ Latency Distribution (P95, last 60s) ' + '─'.repeat(this.width - 41) + '┐' + ANSI.RESET + '\n' +
      `│ ${sparkline}` + ' '.repeat(this.width - sparkline.length - 3) + '│\n' +
      ANSI.BOLD + '└' + '─'.repeat(this.width - 2) + '┘' + ANSI.RESET
    );
  }

  /**
   * Render alerts
   * @returns {string}
   */
  renderAlerts() {
    let output = ANSI.BOLD + '┌─ Alerts ' + '─'.repeat(this.width - 10) + '┐' + ANSI.RESET + '\n';

    if (this.alerts.length === 0) {
      output += `│ ${ANSI.DIM}No active alerts${ANSI.RESET}` + ' '.repeat(this.width - 20) + '│\n';
    } else {
      for (const alert of this.alerts.slice(-5)) {
        output += `│ ${ANSI.YELLOW}⚠${ANSI.RESET} ${alert}` + ' '.repeat(this.width - alert.length - 6) + '│\n';
      }
    }

    output += ANSI.BOLD + '└' + '─'.repeat(this.width - 2) + '┘' + ANSI.RESET;
    return output;
  }

  /**
   * Render footer
   * @returns {string}
   */
  renderFooter() {
    const help = 'Press Ctrl+C to exit';
    return ANSI.DIM + this.centerText(help, this.width) + ANSI.RESET;
  }

  /**
   * Render a progress bar
   * @param {number} value - Current value
   * @param {number} max - Maximum value
   * @param {number} width - Bar width
   * @returns {string}
   */
  renderBar(value, max, width) {
    const percent = Math.min(100, (value / max) * 100);
    const filled = Math.floor((percent / 100) * width);
    const empty = width - filled;

    let color = ANSI.GREEN;
    if (percent > 80) color = ANSI.RED;
    else if (percent > 60) color = ANSI.YELLOW;

    return (
      color + '█'.repeat(filled) + ANSI.DIM + '░'.repeat(empty) + ANSI.RESET
    );
  }

  /**
   * Render a sparkline
   * @param {number[]} values - Data points
   * @param {number} width - Sparkline width
   * @returns {string}
   */
  renderSparkline(values, width) {
    if (values.length === 0) return ' '.repeat(width);

    const blocks = ['▁', '▂', '▃', '▄', '▅', '▆', '▇', '█'];
    const max = Math.max(...values);
    const min = Math.min(...values);
    const range = max - min || 1;

    let sparkline = '';
    const step = values.length / width;

    for (let i = 0; i < width; i++) {
      const index = Math.floor(i * step);
      const value = values[index] || 0;
      const normalized = (value - min) / range;
      const blockIndex = Math.floor(normalized * (blocks.length - 1));
      sparkline += blocks[blockIndex];
    }

    return sparkline;
  }

  /**
   * Colorize value based on thresholds
   * @param {string|number} value - Value to colorize
   * @param {number} good - Good threshold
   * @param {number} warn - Warning threshold
   * @param {number} bad - Bad threshold
   * @returns {string}
   */
  colorize(value, good, warn, bad) {
    const num = typeof value === 'string' ? parseFloat(value) : value;

    if (num >= good) return ANSI.GREEN + value + ANSI.RESET;
    if (num >= warn) return ANSI.YELLOW + value + ANSI.RESET;
    return ANSI.RED + value + ANSI.RESET;
  }

  /**
   * Center text in width
   * @param {string} text - Text to center
   * @param {number} width - Total width
   * @returns {string}
   */
  centerText(text, width) {
    const padding = Math.floor((width - text.length) / 2);
    return ' '.repeat(padding) + text + ' '.repeat(width - text.length - padding);
  }

  /**
   * Format duration in seconds
   * @param {number} seconds - Duration in seconds
   * @returns {string}
   */
  formatDuration(seconds) {
    const h = Math.floor(seconds / 3600);
    const m = Math.floor((seconds % 3600) / 60);
    const s = Math.floor(seconds % 60);

    return `${h}h ${m}m ${s}s`;
  }

  /**
   * Add an alert
   * @param {string} message - Alert message
   */
  addAlert(message) {
    this.alerts.push(message);
    if (this.alerts.length > 20) {
      this.alerts.shift();
    }
  }

  /**
   * Clear alerts
   */
  clearAlerts() {
    this.alerts = [];
  }
}
