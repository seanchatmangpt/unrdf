/**
 * Domain constants for the playground CLI
 * @module domain/constants
 */

/**
 * Paper family definitions with sections
 * @type {Readonly<Array<{name: string, label: string, sections: string[]}>>}
 */
export const PAPER_FAMILIES = Object.freeze([
  {
    name: 'IMRaD',
    label: 'Introduction, Methods, Results, and Discussion',
    sections: ['Introduction', 'Methods', 'Results', 'Discussion', 'References'],
  },
  {
    name: 'Argument',
    label: 'Argumentative Essay Structure',
    sections: ['Thesis', 'Evidence', 'Counter-Arguments', 'Rebuttal', 'Conclusion'],
  },
  {
    name: 'Contribution',
    label: 'Research Contribution Paper',
    sections: ['Problem Statement', 'Related Work', 'Contribution', 'Evaluation', 'Conclusion'],
  },
  {
    name: 'DSR',
    label: 'Design Science Research',
    sections: [
      'Problem Identification',
      'Objectives',
      'Design',
      'Demonstration',
      'Evaluation',
      'Communication',
    ],
  },
  {
    name: 'Monograph',
    label: 'Monograph/Book Chapter',
    sections: [
      'Abstract',
      'Introduction',
      'Literature Review',
      'Analysis',
      'Synthesis',
      'Conclusion',
    ],
  },
  {
    name: 'Narrative',
    label: 'Narrative Review',
    sections: [
      'Introduction',
      'Background',
      'Main Body',
      'Critical Analysis',
      'Summary',
      'Future Directions',
    ],
  },
]);

/**
 * Thesis type definitions with sections
 * @type {Readonly<Array<{name: string, label: string, sections: string[]}>>}
 */
export const THESIS_TYPES = Object.freeze([
  {
    name: 'Masters',
    label: "Master's Thesis",
    sections: [
      'Abstract',
      'Introduction',
      'Literature Review',
      'Methodology',
      'Results',
      'Discussion',
      'Conclusion',
      'References',
      'Appendices',
    ],
  },
  {
    name: 'PhD',
    label: 'Doctoral Dissertation',
    sections: [
      'Abstract',
      'Introduction',
      'Literature Review',
      'Theoretical Framework',
      'Methodology',
      'Results',
      'Discussion',
      'Contributions',
      'Conclusion',
      'References',
      'Appendices',
    ],
  },
  {
    name: 'Postdoctoral',
    label: 'Postdoctoral Research',
    sections: [
      'Executive Summary',
      'Research Overview',
      'Methodology',
      'Findings',
      'Impact Assessment',
      'Future Research',
      'Publications',
    ],
  },
  {
    name: 'MBA',
    label: 'MBA Thesis/Capstone',
    sections: [
      'Executive Summary',
      'Problem Statement',
      'Industry Analysis',
      'Strategic Framework',
      'Recommendations',
      'Implementation Plan',
      'Conclusion',
    ],
  },
  {
    name: 'Professional',
    label: 'Professional Doctorate',
    sections: [
      'Abstract',
      'Introduction',
      'Problem of Practice',
      'Literature Review',
      'Methodology',
      'Findings',
      'Implications for Practice',
      'Conclusion',
    ],
  },
]);

/**
 * Supported output formats
 * @type {Readonly<Array<{name: string, label: string, description: string}>>}
 */
export const OUTPUT_FORMATS = Object.freeze([
  { name: 'json', label: 'JSON', description: 'Compact JSON output' },
  {
    name: 'json-pretty',
    label: 'JSON Pretty',
    description: 'Formatted JSON with indentation',
  },
  { name: 'yaml', label: 'YAML', description: 'YAML formatted output' },
  { name: 'table', label: 'Table', description: 'ASCII table format' },
]);

/**
 * Supported shell types for completion scripts
 * @type {Readonly<Array<{name: string, label: string, description: string}>>}
 */
export const SHELL_TYPES = Object.freeze([
  { name: 'bash', label: 'Bash', description: 'GNU Bourne-Again Shell' },
  { name: 'zsh', label: 'Zsh', description: 'Z Shell' },
  { name: 'fish', label: 'Fish', description: 'Friendly Interactive Shell' },
  {
    name: 'powershell',
    label: 'PowerShell',
    description: 'Microsoft PowerShell',
  },
]);

/**
 * Paper family names as array
 * @type {Readonly<string[]>}
 */
export const PAPER_FAMILY_NAMES = Object.freeze(PAPER_FAMILIES.map(f => f.name));

/**
 * Thesis type names as array
 * @type {Readonly<string[]>}
 */
export const THESIS_TYPE_NAMES = Object.freeze(THESIS_TYPES.map(t => t.name));

/**
 * Output format names as array
 * @type {Readonly<string[]>}
 */
export const OUTPUT_FORMAT_NAMES = Object.freeze(OUTPUT_FORMATS.map(f => f.name));

/**
 * Shell type names as array
 * @type {Readonly<string[]>}
 */
export const SHELL_TYPE_NAMES = Object.freeze(SHELL_TYPES.map(s => s.name));
