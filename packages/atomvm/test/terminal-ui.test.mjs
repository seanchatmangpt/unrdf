/** @vitest-environment jsdom */
import { describe, it, expect, beforeEach } from 'vitest';
import { TerminalUI } from '../src/terminal-ui.mjs';

describe('TerminalUI — real DOM state', () => {
  let terminal;
  let terminalEl;

  beforeEach(() => {
    document.body.innerHTML = '<div id="terminal"></div>';
    terminalEl = document.getElementById('terminal');
    terminal = new TerminalUI();
  });

  it('binds to the real terminal element', () => {
    expect(terminal.terminalEl).toBe(terminalEl);
    expect(terminal.lines).toEqual([]);
  });

  it('renders ordered typed messages', () => {
    terminal.log('Info message', 'info');
    terminal.log('Success message', 'success');
    terminal.log('Error message', 'error');

    const lines = terminalEl.querySelectorAll('.terminal-line');
    expect(lines).toHaveLength(3);
    expect(lines[0].textContent).toContain('Info message');
    expect(lines[1].className).toContain('success');
    expect(lines[2].className).toContain('error');
  });

  it('clears state and renders its actual header', () => {
    terminal.logMultiple(['Message 1', 'Message 2'], 'info');
    terminal.clear();

    expect(terminal.lines).toHaveLength(0);
    expect(terminalEl.querySelectorAll('.terminal-line').length).toBeGreaterThanOrEqual(2);
  });

  it('represents a missing terminal element without fabricating one', () => {
    terminalEl.remove();
    const detached = new TerminalUI();
    expect(detached.terminalEl).toBeNull();
    expect(detached.lines).toEqual([]);
  });
});
