/**
 * @file TypeReference component for displaying Zod schema documentation
 * @module @unrdf/nextra/components/TypeReference
 */

import { readFileSync } from 'node:fs';
import { join } from 'node:path';

/**
 * Display Zod schema type reference
 * @param {Object} props - Component props
 * @param {string} props.schema - Schema name (e.g., "QuadSchema")
 * @param {string} props.package - Package name (e.g., "@unrdf/core")
 * @returns {JSX.Element}
 */
export function TypeReference({ schema, package: pkg }) {
  // Load schema data (in production, this would be from generated JSON)
  let schemaData = null;

  try {
    const dataPath = join(process.cwd(), 'data/api-schemas.json');
    const allSchemas = JSON.parse(readFileSync(dataPath, 'utf-8'));
    schemaData = allSchemas[pkg]?.find((s) => s.name === schema);
  } catch (error) {
    console.warn(`Failed to load schema data: ${error.message}`);
  }

  if (!schemaData) {
    return (
      <div className="border border-red-300 bg-red-50 p-4 rounded-lg">
        <p className="text-red-800">Schema not found: {schema} in {pkg}</p>
      </div>
    );
  }

  return (
    <div className="border border-blue-300 bg-blue-50 p-6 rounded-lg my-6">
      <h3 className="text-xl font-bold text-blue-900 mb-2">{schemaData.name}</h3>

      {schemaData.jsdoc && (
        <p className="text-gray-700 mb-4">{schemaData.jsdoc}</p>
      )}

      <div className="bg-white p-4 rounded border border-gray-200">
        <div className="flex items-center justify-between mb-2">
          <span className="text-sm font-mono text-gray-600">Type: {schemaData.type}</span>
          <a
            href={`https://github.com/unrdf/unrdf/blob/main${schemaData.sourceFile}#L${schemaData.line}`}
            className="text-sm text-blue-600 hover:underline"
            target="_blank"
            rel="noopener noreferrer"
          >
            View Source →
          </a>
        </div>

        <pre className="text-sm overflow-x-auto">
          <code>{schemaData.definition}</code>
        </pre>
      </div>

      <div className="mt-4 text-sm text-gray-600">
        <strong>Package:</strong> <code>{pkg}</code>
      </div>
    </div>
  );
}
