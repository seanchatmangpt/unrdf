#!/usr/bin/env node
/**
 * Script to fix unused variable warnings by prefixing with underscore
 * This script parses ESLint output and automatically fixes unused variables
 */

import { execSync } from 'node:child_process';
import { readFileSync, writeFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, resolve } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = resolve(__dirname, '..');

// Get ESLint output
console.log('Running ESLint to find unused variables...');
const lintOutput = execSync('pnpm lint 2>&1', { cwd: rootDir, encoding: 'utf-8' });

// Parse warnings
const warnings = [];
const lines = lintOutput.split('\n');
let currentFile = null;

for (const line of lines) {
  // Match file path
  const fileMatch = line.match(/^(\/[^\s]+|\w[^\s]+\.mjs)/);
  if (fileMatch) {
    currentFile = fileMatch[1].startsWith('/') ? fileMatch[1] : resolve(rootDir, fileMatch[1]);
    continue;
  }

  // Match unused variable warning
  const warningMatch = line.match(/\s+(\d+):(\d+)\s+warning\s+['"]([^'"]+)['"]\s+is\s+(defined but never used|assigned a value but never used)/);
  if (warningMatch && currentFile) {
    const [, lineNum, colNum, varName] = warningMatch;
    warnings.push({
      file: currentFile,
      line: parseInt(lineNum, 10),
      col: parseInt(colNum, 10),
      varName,
    });
  }
}

console.log(`Found ${warnings.length} unused variable warnings`);

// Group by file
const fileWarnings = {};
for (const warning of warnings) {
  if (!fileWarnings[warning.file]) {
    fileWarnings[warning.file] = [];
  }
  fileWarnings[warning.file].push(warning);
}

// Fix each file
let fixedCount = 0;
for (const [file, fileWarns] of Object.entries(fileWarnings)) {
  try {
    let content = readFileSync(file, 'utf-8');
    const lines = content.split('\n');
    let modified = false;

    // Sort warnings by line number (descending) to avoid offset issues
    const sortedWarns = [...fileWarns].sort((a, b) => b.line - a.line);

    for (const warning of sortedWarns) {
      const lineIndex = warning.line - 1;
      if (lineIndex >= 0 && lineIndex < lines.length) {
        const line = lines[lineIndex];
        const colIndex = warning.col - 1;

        // Check if variable is already prefixed with _
        if (warning.varName.startsWith('_')) {
          continue;
        }

        // Find the variable name in the line and prefix with _
        // Handle different patterns: imports, const, let, var, function, parameters
        const escapedVar = warning.varName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
        
        // Pattern 1: Import statements - import { var } or import var from
        if (line.includes('import')) {
          // Handle: import { var, ... } or import var from
          const importPattern = new RegExp(`(import\\s+\\{[^}]*?)\\b${escapedVar}\\b([^}]*?\\})`, 'g');
          if (importPattern.test(line)) {
            const newLine = line.replace(
              new RegExp(`(import\\s+\\{[^}]*?)\\b${escapedVar}\\b([^}]*?\\})`, 'g'),
              (match, before, after) => {
                // Handle comma placement
                if (before.endsWith(', ')) {
                  return `${before}_${warning.varName}${after}`;
                } else if (before.endsWith(',')) {
                  return `${before} _${warning.varName}${after}`;
                } else if (before.includes('{')) {
                  return `${before}_${warning.varName}${after}`;
                }
                return match;
              },
            );
            if (newLine !== line) {
              lines[lineIndex] = newLine;
              modified = true;
              fixedCount++;
              continue;
            }
          }
        }

        // Pattern 2: Function parameters - function name(var) or (var) =>
        if (line.includes('function') || line.includes('=>') || line.includes('(')) {
          const paramPattern = new RegExp(`\\([^)]*?\\b${escapedVar}\\b[^)]*?\\)`, 'g');
          if (paramPattern.test(line)) {
            const newLine = line.replace(
              new RegExp(`\\(([^)]*?)\\b${escapedVar}\\b([^)]*?)\\)`, 'g'),
              (match, before, after) => {
                // Handle comma placement
                if (before.trim().endsWith(',')) {
                  return `(${before.trim()}_${warning.varName}${after})`;
                } else if (before.trim()) {
                  return `(${before.trim()}, _${warning.varName}${after})`;
                } else {
                  return `(_${warning.varName}${after})`;
                }
              },
            );
            if (newLine !== line) {
              lines[lineIndex] = newLine;
              modified = true;
              fixedCount++;
              continue;
            }
          }
        }

        // Pattern 3: Destructured assignments - const [a, b] = or const { a, b } =
        // Handle React useState: const [state, setState] = useState()
        if (line.includes('[') || line.includes('{')) {
          // Array destructuring: const [a, setB] = ...
          const arrayDestructPattern = new RegExp(`\\[([^\\]]*?)\\b${escapedVar}\\b([^\\]]*?)\\]`, 'g');
          if (arrayDestructPattern.test(line)) {
            const newLine = line.replace(
              new RegExp(`\\[([^\\]]*?)\\b${escapedVar}\\b([^\\]]*?)\\]`, 'g'),
              (match, before, after) => {
                // Handle comma placement
                if (before.trim().endsWith(',')) {
                  return `[${before.trim()}_${warning.varName}${after}]`;
                } else if (before.trim()) {
                  return `[${before.trim()}, _${warning.varName}${after}]`;
                } else {
                  return `[_${warning.varName}${after}]`;
                }
              },
            );
            if (newLine !== line) {
              lines[lineIndex] = newLine;
              modified = true;
              fixedCount++;
              continue;
            }
          }
          
          // Object destructuring: const { a, b } = ...
          const objDestructPattern = new RegExp(`\\{([^}]*?)\\b${escapedVar}\\b([^}]*?)\\}`, 'g');
          if (objDestructPattern.test(line)) {
            const newLine = line.replace(
              new RegExp(`\\{([^}]*?)\\b${escapedVar}\\b([^}]*?)\\}`, 'g'),
              (match, before, after) => {
                // Handle comma placement
                if (before.trim().endsWith(',')) {
                  return `{${before.trim()}_${warning.varName}${after}}`;
                } else if (before.trim()) {
                  return `{${before.trim()}, _${warning.varName}${after}}`;
                } else {
                  return `{_${warning.varName}${after}}`;
                }
              },
            );
            if (newLine !== line) {
              lines[lineIndex] = newLine;
              modified = true;
              fixedCount++;
              continue;
            }
          }
        }

        // Pattern 4: Simple variable declarations - const/let/var var = or const var =
        const declPatterns = [
          new RegExp(`\\b(const|let|var)\\s+${escapedVar}\\s*[=,]`, 'g'),
        ];

        for (const pattern of declPatterns) {
          if (pattern.test(line)) {
            const newLine = line.replace(
              new RegExp(`\\b${escapedVar}\\b`, 'g'),
              `_${warning.varName}`,
            );
            if (newLine !== line) {
              lines[lineIndex] = newLine;
              modified = true;
              fixedCount++;
              break;
            }
          }
        }
      }
    }

    if (modified) {
      writeFileSync(file, lines.join('\n'), 'utf-8');
      console.log(`Fixed ${fileWarns.length} warnings in ${file}`);
    }
  } catch (error) {
    console.error(`Error processing ${file}:`, error.message);
  }
}

console.log(`\nFixed ${fixedCount} unused variable warnings`);
console.log('Run `pnpm lint` again to verify fixes');

