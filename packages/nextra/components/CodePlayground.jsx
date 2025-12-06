/**
 * @file CodePlayground component for interactive code examples
 * @module @unrdf/nextra/components/CodePlayground
 */

'use client';

import { useState } from 'react';

/**
 * Interactive code playground with live execution
 * @param {Object} props - Component props
 * @param {string} props.children - Code to execute
 * @param {string} props.runtime - Runtime environment (browser|node)
 * @param {string} props.package - Package to import
 * @returns {JSX.Element}
 */
export function CodePlayground({ children, runtime = 'browser', package: pkg = '@unrdf/core' }) {
  const [output, setOutput] = useState('');
  const [running, setRunning] = useState(false);

  // Extract code from children (MDX passes code as string in children)
  const code = typeof children === 'string'
    ? children
    : children?.props?.children || '';

  const executeCode = async () => {
    setRunning(true);
    setOutput('');

    try {
      if (runtime === 'browser') {
        // Create iframe sandbox for browser execution
        const iframe = document.createElement('iframe');
        iframe.style.display = 'none';
        document.body.appendChild(iframe);

        // Capture console output
        const logs = [];
        iframe.contentWindow.console.log = (...args) => {
          logs.push(args.map((a) => String(a)).join(' '));
        };

        // Load UNRDF bundle
        const script = iframe.contentDocument.createElement('script');
        script.type = 'module';
        script.textContent = `
          import * as UNRDF from 'https://cdn.jsdelivr.net/npm/${pkg}@latest/+esm';
          window.UNRDF = UNRDF;
          ${code}
        `;

        await new Promise((resolve, reject) => {
          script.onload = resolve;
          script.onerror = reject;
          iframe.contentDocument.body.appendChild(script);
          setTimeout(resolve, 1000); // Timeout for async code
        });

        setOutput(logs.join('\n') || '(no output)');
        document.body.removeChild(iframe);
      } else {
        // Node.js execution (server-side only)
        setOutput('Node.js execution not supported in browser playground');
      }
    } catch (error) {
      setOutput(`Error: ${error.message}`);
    } finally {
      setRunning(false);
    }
  };

  return (
    <div className="border border-purple-300 bg-purple-50 rounded-lg my-6 overflow-hidden">
      <div className="bg-purple-100 px-4 py-2 flex items-center justify-between">
        <span className="font-semibold text-purple-900">Interactive Playground</span>
        <button
          onClick={executeCode}
          disabled={running}
          className="px-4 py-1 bg-purple-600 text-white rounded hover:bg-purple-700 disabled:opacity-50 text-sm"
        >
          {running ? 'Running...' : '▶ Run'}
        </button>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-0">
        <div className="border-b lg:border-b-0 lg:border-r border-purple-200">
          <div className="bg-gray-800 text-gray-100 p-4">
            <div className="text-xs text-gray-400 mb-2">Code</div>
            <pre className="overflow-x-auto text-sm">
              <code>{code}</code>
            </pre>
          </div>
        </div>

        <div className="bg-white">
          <div className="p-4">
            <div className="text-xs text-gray-600 mb-2">Output</div>
            <pre className="overflow-x-auto text-sm text-gray-800">
              <code>{output || '(click "Run" to execute)'}</code>
            </pre>
          </div>
        </div>
      </div>

      <div className="bg-purple-50 px-4 py-2 text-xs text-purple-700">
        Runtime: {runtime} | Package: <code>{pkg}</code>
      </div>
    </div>
  );
}
