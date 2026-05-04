/**
 * @fileoverview Terminal UI Tests
 * @vitest-environment jsdom
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { TerminalUI } from '../src/terminal-ui.mjs';

describe('TerminalUI', () => {
  let terminal;
  let terminalEl;

  beforeEach(() => {
    // Clear any existing terminal element
    const existing = document.getElementById('terminal');
    if (existing) {
      existing.remove();
    }
    
    // Create mock terminal element
    terminalEl = document.createElement('div');
    terminalEl.id = 'terminal';
    document.body.appendChild(terminalEl);
    
    terminal = new TerminalUI();
  });

  it('should create terminal instance', () => {
    expect(terminal).toBeDefined();
    expect(terminal.terminalEl).toBe(terminalEl);
    expect(terminal.lines).toEqual([]);
  });

  it('should log messages to terminal', () => {
    terminal.log('Test message', 'info');
    
    const lines = terminalEl.querySelectorAll('.terminal-line');
    expect(lines.length).toBeGreaterThan(0);
    expect(lines[lines.length - 1].textContent).toContain('Test message');
  });

  it('should log different message types', () => {
    terminal.log('Info message', 'info');
    terminal.log('Success message', 'success');
    terminal.log('Error message', 'error');
    
    const lines = terminalEl.querySelectorAll('.terminal-line');
    expect(lines.length).toBe(3);
    expect(lines[0].className).toContain('info');
    expect(lines[1].className).toContain('success');
    expect(lines[2].className).toContain('error');
  });

  it('should clear terminal', () => {
    terminal.log('Message 1', 'info');
    terminal.log('Message 2', 'info');
    
    expect(terminal.lines.length).toBe(2);
    
    terminal.clear();
    
    expect(terminal.lines.length).toBe(0);
    const lines = terminalEl.querySelectorAll('.terminal-line');
    // After clear, there should be 2 header lines
    expect(lines.length).toBeGreaterThanOrEqual(2);
  });

  it('should log multiple messages', () => {
    const messages = ['Message 1', 'Message 2', 'Message 3'];
    terminal.logMultiple(messages, 'info');
    
    expect(terminal.lines.length).toBe(3);
  });

  it('should create separator', () => {
    terminal.separator();
    
    const lines = terminalEl.querySelectorAll('.terminal-line');
    expect(lines.length).toBeGreaterThan(0);
    const lastLine = lines[lines.length - 1];
    expect(lastLine.textContent).toContain('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  });

  it('should fallback to console when terminal element not found', () => {
    // Remove terminal element first
    terminalEl.remove();
    
    // Create new terminal instance (won't find element)
    const terminalWithoutEl = new TerminalUI();
    const consoleSpy = vi.spyOn(console, 'log');
    
    terminalWithoutEl.log('Test', 'info');
    
    expect(consoleSpy).toHaveBeenCalled();
    consoleSpy.mockRestore();
  });
});

