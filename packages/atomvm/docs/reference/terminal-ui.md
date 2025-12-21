# Terminal UI API Reference

Complete API documentation for the Terminal UI component.

## Class: TerminalUI

Terminal UI component for displaying logs and output in the browser.

```javascript
import { TerminalUI } from '@unrdf/atomvm';
```

## Constructor

### new TerminalUI()

Creates a new Terminal UI instance.

**Signature:**
```javascript
new TerminalUI(): TerminalUI
```

**Returns:** TerminalUI instance

**Behavior:**
- Looks for DOM element with id `terminal`
- Initializes empty lines array
- Falls back to console if element not found

**Example:**
```javascript
const terminal = new TerminalUI();
```

**Note:** Requires HTML element with `id="terminal"` in the DOM.

## Methods

### log(message, type)

Logs a message to the terminal.

**Signature:**
```javascript
log(message: string, type?: 'info' | 'success' | 'error'): void
```

**Parameters:**
- `message` (string) - Message to log
- `type` ('info' | 'success' | 'error', optional) - Message type (default: 'info')

**Behavior:**
1. Creates DOM element for log line
2. Applies CSS class based on type
3. Adds timestamp for non-info messages
4. Appends to terminal element
5. Auto-scrolls to bottom
6. Also logs to browser console

**Example:**
```javascript
terminal.log('Initializing...', 'info');
terminal.log('Success!', 'success');
terminal.log('Error occurred', 'error');
```

**CSS Classes:**
- `terminal-line` - Base class for all lines
- `terminal-line info` - Info messages (blue)
- `terminal-line success` - Success messages (green)
- `terminal-line error` - Error messages (red)

**Fallback:** If terminal element not found, logs to console with type prefix.

### clear()

Clears the terminal and resets to initial state.

**Signature:**
```javascript
clear(): void
```

**Behavior:**
1. Resets terminal HTML to header lines
2. Clears lines array
3. Shows default header:
   ```
   AtomVM Browser Console
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   ```

**Example:**
```javascript
terminal.clear();
// Terminal is now empty except for header
```

### logMultiple(messages, type)

Logs multiple messages at once.

**Signature:**
```javascript
logMultiple(messages: string[], type?: 'info' | 'success' | 'error'): void
```

**Parameters:**
- `messages` (string[]) - Array of messages to log
- `type` ('info' | 'success' | 'error', optional) - Message type for all messages (default: 'info')

**Behavior:**
- Calls `log()` for each message in the array
- All messages use the same type

**Example:**
```javascript
terminal.logMultiple([
  'Step 1: Initialize',
  'Step 2: Load WASM',
  'Step 3: Execute',
], 'info');
```

### separator()

Creates a visual separator line.

**Signature:**
```javascript
separator(): void
```

**Behavior:**
- Logs a separator line using `log()`
- Uses 'info' type
- Separator: `━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━`

**Example:**
```javascript
terminal.log('Before separator', 'info');
terminal.separator();
terminal.log('After separator', 'info');
```

## Properties

### terminalEl

Terminal DOM element.

**Type:** `HTMLElement | null`

**Access:** Read-only

**Note:** `null` if element with `id="terminal"` not found in DOM.

**Example:**
```javascript
if (terminal.terminalEl) {
  terminal.terminalEl.style.maxHeight = '500px';
}
```

### lines

Array of log line DOM elements.

**Type:** `HTMLElement[]`

**Access:** Read-only

**Note:** Cleared when `clear()` is called.

**Example:**
```javascript
console.log('Number of log lines:', terminal.lines.length);
```

## HTML Structure

Terminal UI expects this HTML structure:

```html
<div class="terminal" id="terminal">
  <!-- Log lines are appended here -->
</div>
```

**CSS Classes:**
- `.terminal` - Container (dark background, monospace font)
- `.terminal-line` - Individual log line
- `.terminal-line.info` - Info message styling
- `.terminal-line.success` - Success message styling
- `.terminal-line.error` - Error message styling

## Usage Example

Complete example:

```html
<!DOCTYPE html>
<html>
<head>
  <style>
    .terminal {
      background: #1e1e1e;
      color: #d4d4d4;
      font-family: monospace;
      padding: 20px;
      max-height: 400px;
      overflow-y: auto;
    }
    .terminal-line.info { color: #60a5fa; }
    .terminal-line.success { color: #4ade80; }
    .terminal-line.error { color: #f87171; }
  </style>
</head>
<body>
  <div class="terminal" id="terminal"></div>

  <script type="module">
    import { TerminalUI } from './src/terminal-ui.mjs';

    const terminal = new TerminalUI();
    
    terminal.log('Starting application...', 'info');
    terminal.separator();
    terminal.log('Step 1 complete', 'success');
    terminal.log('Step 2 complete', 'success');
    terminal.log('Error in step 3', 'error');
    terminal.separator();
    terminal.log('Application ready', 'success');
  </script>
</body>
</html>
```

## Console Integration

Terminal UI also logs to browser console:

- **Info messages:** `console.info()`
- **Success messages:** `console.log()`
- **Error messages:** `console.error()`

This ensures logs are visible even if terminal element is not found.

## Related Documentation

- [Tutorial: Getting Started](../tutorials/01-getting-started.md)
- [API Reference](./api.md) - Complete API overview


