/**
 * @file Conventions Profile - Main Exports
 * @description Machine-checkable organizational engineering conventions
 */

export {
  ConventionsProfileSchema,
  FileLayoutSchema,
  ErrorModelSchema,
  LoggingSchema,
  TestingSchema,
  DataContractsSchema,
  ViolationSchema,
  CompiledProfileSchema,
  ValidationResultSchema,
  NamingConventionSchema
} from './profile-schema.mjs';

export {
  compileProfile,
  validateAgainstProfile,
  diagnosticReport
} from './compiler.mjs';

export {
  demoProfile,
  minimalProfile,
  strictProfile
} from './demo-profile.mjs';
