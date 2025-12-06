/**
 * @file FunctionAPI component for displaying function documentation
 * @module @unrdf/nextra/components/FunctionAPI
 */

import { readFileSync } from 'node:fs';
import { join } from 'node:path';

/**
 * Display function API documentation
 * @param {Object} props - Component props
 * @param {string} props.name - Function name
 * @param {string} props.package - Package name (e.g., "@unrdf/core")
 * @returns {JSX.Element}
 */
export function FunctionAPI({ name, package: pkg }) {
  // Load function data (in production, this would be from generated JSON)
  let funcData = null;

  try {
    const dataPath = join(process.cwd(), 'data/api-jsdoc.json');
    const allDocs = JSON.parse(readFileSync(dataPath, 'utf-8'));
    funcData = allDocs[pkg]?.find((f) => f.name === name);
  } catch (error) {
    console.warn(`Failed to load function data: ${error.message}`);
  }

  if (!funcData) {
    return (
      <div className="border border-red-300 bg-red-50 p-4 rounded-lg">
        <p className="text-red-800">Function not found: {name} in {pkg}</p>
      </div>
    );
  }

  return (
    <div className="border border-green-300 bg-green-50 p-6 rounded-lg my-6">
      <h3 className="text-xl font-bold text-green-900 mb-2">
        {funcData.async && <span className="text-sm mr-2">async</span>}
        {funcData.name}()
      </h3>

      {funcData.description && (
        <p className="text-gray-700 mb-4">{funcData.description}</p>
      )}

      {funcData.params && funcData.params.length > 0 && (
        <div className="mb-4">
          <h4 className="font-semibold text-gray-800 mb-2">Parameters</h4>
          <table className="w-full bg-white rounded border border-gray-200">
            <thead>
              <tr className="bg-gray-100">
                <th className="text-left p-2 font-mono text-sm">Name</th>
                <th className="text-left p-2 font-mono text-sm">Type</th>
                <th className="text-left p-2 text-sm">Description</th>
              </tr>
            </thead>
            <tbody>
              {funcData.params.map((param) => (
                <tr key={param.name} className="border-t border-gray-200">
                  <td className="p-2 font-mono text-sm">{param.name}</td>
                  <td className="p-2 font-mono text-sm text-blue-600">{param.type}</td>
                  <td className="p-2 text-sm">{param.description}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      {funcData.returns && (
        <div className="mb-4">
          <h4 className="font-semibold text-gray-800 mb-2">Returns</h4>
          <div className="bg-white p-3 rounded border border-gray-200">
            <span className="font-mono text-sm text-blue-600">{funcData.returns.type}</span>
            {funcData.returns.description && (
              <p className="text-sm text-gray-700 mt-1">{funcData.returns.description}</p>
            )}
          </div>
        </div>
      )}

      {funcData.examples && funcData.examples.length > 0 && (
        <div className="mb-4">
          <h4 className="font-semibold text-gray-800 mb-2">Example</h4>
          {funcData.examples.map((example, idx) => (
            <pre key={idx} className="bg-gray-900 text-gray-100 p-4 rounded overflow-x-auto">
              <code>{example}</code>
            </pre>
          ))}
        </div>
      )}

      <div className="flex items-center justify-between text-sm text-gray-600">
        <span><strong>Package:</strong> <code>{pkg}</code></span>
        <a
          href={`https://github.com/unrdf/unrdf/blob/main${funcData.sourceFile}#L${funcData.line}`}
          className="text-blue-600 hover:underline"
          target="_blank"
          rel="noopener noreferrer"
        >
          View Source →
        </a>
      </div>
    </div>
  );
}
