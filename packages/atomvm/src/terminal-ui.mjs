/**
 * Terminal UI Component
 *
 * Manages the terminal display for AtomVM output and logging.
 *
 * @module terminal-ui
 */

/**
 * Terminal UI class
 */
export class TerminalUI {
  /**
   *
   */
  constructor() {
    this.terminalEl = document.getElementById('terminal');
    this.lines = [];
  }

  /**
   * Log a message to the terminal
   *
   * @param {string} message - Message to log
   * @param {string} type - Message type (info, success, error)
   */
  log(message, type = 'info') {
    if (!this.terminalEl) {
      console.log(`[${type}] ${message}`);
      return;
    }

    const line = document.createElement('div');
    line.className = `terminal-line ${type}`;

    // Add timestamp for non-info messages
    const timestamp = new Date().toLocaleTimeString();
    const prefix = type === 'info' ? '' : `[${timestamp}] `;

    line.textContent = prefix + message;

    this.terminalEl.appendChild(line);
    this.lines.push(line);

    // Auto-scroll to bottom
    this.terminalEl.scrollTop = this.terminalEl.scrollHeight;

    // Also log to console
    const consoleMethod = type === 'error' ? 'error' : type === 'success' ? 'log' : 'info';
    console[consoleMethod](message);
  }

  /**
   * Clear the terminal
   */
  clear() {
    if (this.terminalEl) {
      this.terminalEl.innerHTML = `
        <div class="terminal-line info">AtomVM Browser Console</div>
        <div class="terminal-line info">━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━</div>
      `;
    }
    this.lines = [];
  }

  /**
   * Log multiple lines
   *
   * @param {string[]} messages - Array of messages
   * @param {string} type - Message type
   */
  logMultiple(messages, type = 'info') {
    messages.forEach(msg => this.log(msg, type));
  }

  /**
   * Create a separator line
   */
  separator() {
    this.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━', 'info');
  }
}
