/**
 * Domain module index
 * @module domain
 */

// Constants
export {
  PAPER_FAMILIES,
  THESIS_TYPES,
  OUTPUT_FORMATS,
  SHELL_TYPES,
  PAPER_FAMILY_NAMES,
  THESIS_TYPE_NAMES,
  OUTPUT_FORMAT_NAMES,
  SHELL_TYPE_NAMES,
} from './constants.mjs';

// Zod schemas and types
export {
  PaperFamilySchema,
  ThesisTypeSchema,
  OutputFormatSchema,
  ShellTypeSchema,
  SectionSchema,
  PaperSchema,
  ScheduleItemSchema,
  ThesisSchema,
  ConfigSchema,
} from './types.mjs';

// Models
export { Paper, Thesis, Config } from './models/index.mjs';

// Formatters
export {
  formatOutput,
  getFormatter,
  isValidFormat,
  detectFormat,
  jsonFormatter,
  jsonPrettyFormatter,
  parseJSON,
  safeParseJSON,
  isValidJSON,
  yamlFormatter,
  parseYAML,
  safeParseYAML,
  isValidYAML,
  jsonToYAML,
  yamlToJSON,
  tableFormatter,
  dataToRows,
  formatCell,
  keyValueTable,
  listTable,
} from './formatters/index.mjs';
