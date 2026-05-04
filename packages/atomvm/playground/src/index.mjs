/**
 * AtomVM Playground Browser Entry Point
 *
 * **80/20 Focus**: Minimal UI that shows validation results:
 * - Module input (required)
 * - Validation status (pass/fail)
 * - Results display
 * - Error prevention indicators
 *
 * @module playground/index
 */

import { ValidationSuite } from './validation-suite.mjs';
import { registerServiceWorker, checkCrossOriginIsolation } from '@unrdf/atomvm';
import { getBridge } from './kgc4d-bridge.mjs';
import { interceptAtomVMOutput } from './bridge-interceptor.mjs';

/**
 * Terminal UI for playground
 */
class PlaygroundTerminal {
  constructor(element) {
    this.element = element;
  }

  log(message, type = 'info') {
    const line = document.createElement('div');
    line.className = `terminal-line ${type}`;
    line.textContent = message;
    this.element.appendChild(line);
    this.element.scrollTop = this.element.scrollHeight;
  }

  clear() {
    this.element.innerHTML = `
      <div class="terminal-line info">AtomVM Playground Console</div>
      <div class="terminal-line info">━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━</div>
    `;
  }
}

/**
 * Validation result display
 */
class ValidationDisplay {
  constructor(element) {
    this.element = element;
  }

  clear() {
    this.element.innerHTML = '';
  }

  addResult(result) {
    const item = document.createElement('div');
    item.className = `validation-item ${result.status}`;
    item.id = `validation-${result.name.replace(/\s+/g, '-').toLowerCase()}`;

    const title = document.createElement('h3');
    title.textContent = result.name;
    item.appendChild(title);

    const details = document.createElement('div');
    details.className = 'details';
    details.textContent = result.message || '';
    item.appendChild(details);

    // Update existing or add new
    const existing = this.element.querySelector(`#${item.id}`);
    if (existing) {
      existing.replaceWith(item);
    } else {
      this.element.appendChild(item);
    }
  }

  updateResult(result) {
    this.addResult(result);
  }
}

/**
 * Initialize playground
 */
async function init() {
  // Register service worker for COI (if needed)
  try {
    await registerServiceWorker();
    // Wait a bit for COI to activate
    await new Promise(resolve => setTimeout(resolve, 300));
    const isIsolated = checkCrossOriginIsolation();
    if (!isIsolated && navigator.serviceWorker.controller) {
      // COI requires reload, but for playground we can continue
      console.log('COI not active, but continuing for validation');
    }
  } catch (error) {
    console.warn('Service worker registration failed:', error);
  }

  const moduleInput = document.getElementById('moduleName');
  const validateBtn = document.getElementById('validateBtn');
  const clearBtn = document.getElementById('clearBtn');
  const terminalEl = document.getElementById('terminal');
  const validationResultsEl = document.getElementById('validationResults');

  const terminal = new PlaygroundTerminal(terminalEl);
  const validationDisplay = new ValidationDisplay(validationResultsEl);

  // Initialize KGC-4D bridge for Erlang integration
  const bridge = getBridge({
    log: (message) => terminal.log(`[Bridge] ${message}`, 'info')
  });
  
  // Make bridge available globally for AtomVM Module callbacks
  if (typeof window !== 'undefined') {
    window.kgc4dBridge = bridge;
    
    // Set up interceptor for when AtomVM Module is created
    // The interceptor will wrap Module.print/printErr when they're set up by AtomVMRuntime
    // We use a polling approach that checks for Module.print existence
    const setupInterceptor = () => {
      if (window.Module && window.Module.print && typeof window.Module.print === 'function') {
        // Intercept the existing print functions (wraps, doesn't replace)
        interceptAtomVMOutput(window.Module, (message) => terminal.log(message, 'info'));
        return true; // Successfully set up
      }
      return false; // Not ready yet
    };
    
    // Try to set up immediately
    if (!setupInterceptor()) {
      // If not ready, poll until Module.print is available
      let attempts = 0;
      const maxAttempts = 50; // 5 seconds total
      const pollInterval = setInterval(() => {
        attempts++;
        if (setupInterceptor() || attempts >= maxAttempts) {
          clearInterval(pollInterval);
          if (attempts >= maxAttempts) {
            terminal.log('[Bridge] Warning: Could not set up interceptor - Module.print not available after 5s', 'error');
          } else {
            terminal.log('[Bridge] Interceptor set up successfully', 'info');
          }
        }
      }, 100);
    } else {
      terminal.log('[Bridge] Interceptor set up immediately', 'info');
    }
  }

  // Create validation suite
  const validationSuite = new ValidationSuite({
    log: (message) => terminal.log(message, 'info'),
    onResult: (result) => {
      validationDisplay.updateResult(result);
      const statusEmoji = result.status === 'pass' ? '✅' : result.status === 'fail' ? '❌' : result.status === 'running' ? '🔄' : '⏳';
      terminal.log(`${statusEmoji} ${result.name}: ${result.message}`, result.status === 'pass' ? 'success' : result.status === 'fail' ? 'error' : 'info');
    }
  });

  // Validate button handler
  validateBtn.addEventListener('click', async () => {
    const moduleName = moduleInput.value.trim();

    if (!moduleName) {
      terminal.log('❌ Module name is required', 'error');
      return;
    }

    // Poka-yoke: Validate input
    if (moduleName.length === 0) {
      terminal.log('❌ Module name cannot be empty', 'error');
      return;
    }

    validateBtn.disabled = true;
    terminal.log(`Starting validation for module: ${moduleName}`, 'info');
    validationDisplay.clear();

    try {
      const results = await validationSuite.runAll(moduleName);
      const allPassed = results.every(r => r.status === 'pass');
      
      if (allPassed) {
        terminal.log('✅ All validations passed!', 'success');
      } else {
        terminal.log('❌ Some validations failed', 'error');
        results.filter(r => r.status === 'fail').forEach(r => {
          if (r.error) {
            terminal.log(`  Error: ${r.error.message}`, 'error');
          }
        });
      }
    } catch (error) {
      terminal.log(`❌ Validation suite error: ${error.message}`, 'error');
    } finally {
      validateBtn.disabled = false;
    }
  });

  // Clear button handler
  clearBtn.addEventListener('click', () => {
    terminal.clear();
    validationDisplay.clear();
    validationSuite.clear();
  });

  terminal.log('AtomVM Playground ready', 'info');
  terminal.log('Enter a module name and click "Run Validation Suite"', 'info');
}

// Initialize when DOM is ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', init);
} else {
  init();
}

