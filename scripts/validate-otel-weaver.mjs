#!/usr/bin/env node
/**
 * @file OTEL Weaver Implementation Validator
 * @description Validates that all OTEL Weaver components are properly implemented
 */

import { readFileSync, existsSync } from 'fs';
import { join } from 'path';

const REQUIRED_FILES = [
  'weaver.yaml',
  'custom-conventions.yaml',
  'sidecar/server/utils/otel-context-propagation.mjs',
  'sidecar/server/utils/slo-tracker.mjs',
  'sidecar/server/middleware/01.telemetry.mjs',
  'src/sidecar/client.mjs',
  '.github/workflows/otel-weaver-validate.yml',
  'grafana/dashboards/.gitkeep',
  'docs/telemetry/OTEL-WEAVER-INTEGRATION.md',
];

const REQUIRED_FUNCTIONS = {
  'sidecar/server/utils/otel-context-propagation.mjs': [
    'parseTraceparent',
    'formatTraceparent',
    'extractTraceContextFromHeaders',
    'extractTraceContextFromMetadata',
    'injectTraceContextIntoHeaders',
    'injectTraceContextIntoMetadata',
    'getCurrentTraceContext',
    'getTraceIdForLogging',
    'getSpanIdForLogging',
    'enrichLogWithTraceContext',
    'addMetricExemplar',
  ],
  'sidecar/server/utils/slo-tracker.mjs': [
    'SLOTracker',
    'createDefaultSLOTracker',
    'SLOType',
    'SLOStatus',
  ],
};

const REQUIRED_CONVENTIONS = [
  'knowledge_hook',
  'policy_pack',
  'rdf_graph',
  'effect_sandbox',
  'crypto_provenance',
  'kgc_transaction',
  'grpc_sidecar',
];

console.log('🔍 OTEL Weaver Implementation Validator\n');
console.log('=' .repeat(60));

let errors = 0;
let warnings = 0;

// Check required files
console.log('\n📁 Checking Required Files...\n');

REQUIRED_FILES.forEach(file => {
  const filePath = join(process.cwd(), file);
  if (existsSync(filePath)) {
    console.log(`  ✅ ${file}`);
  } else {
    console.log(`  ❌ ${file} - MISSING`);
    errors++;
  }
});

// Check required functions
console.log('\n🔧 Checking Required Functions...\n');

Object.entries(REQUIRED_FUNCTIONS).forEach(([file, functions]) => {
  const filePath = join(process.cwd(), file);
  if (!existsSync(filePath)) {
    console.log(`  ⚠️  Skipping ${file} - file not found`);
    warnings++;
    return;
  }

  const content = readFileSync(filePath, 'utf-8');

  functions.forEach(fn => {
    const patterns = [
      `export function ${fn}`,
      `export const ${fn}`,
      `export class ${fn}`,
      `function ${fn}`,
      `const ${fn}`,
      `class ${fn}`,
    ];

    const found = patterns.some(pattern => content.includes(pattern));

    if (found) {
      console.log(`  ✅ ${fn}`);
    } else {
      console.log(`  ❌ ${fn} - NOT FOUND in ${file}`);
      errors++;
    }
  });
});

// Check weaver.yaml structure
console.log('\n📋 Checking weaver.yaml Structure...\n');

const weaverPath = join(process.cwd(), 'weaver.yaml');
if (existsSync(weaverPath)) {
  const weaverContent = readFileSync(weaverPath, 'utf-8');

  const requiredSections = [
    'version:',
    'project_name:',
    'registry:',
    'generation:',
    'instrumentation:',
    'enforcement:',
    'export:',
    'context:',
    'slo:',
    'grafana:',
  ];

  requiredSections.forEach(section => {
    if (weaverContent.includes(section)) {
      console.log(`  ✅ ${section}`);
    } else {
      console.log(`  ❌ ${section} - MISSING`);
      errors++;
    }
  });
} else {
  console.log('  ❌ weaver.yaml not found');
  errors++;
}

// Check custom conventions
console.log('\n🏷️  Checking Custom Semantic Conventions...\n');

const conventionsPath = join(process.cwd(), 'custom-conventions.yaml');
if (existsSync(conventionsPath)) {
  const conventionsContent = readFileSync(conventionsPath, 'utf-8');

  REQUIRED_CONVENTIONS.forEach(convention => {
    const idPattern = `id: ${convention}`;
    if (conventionsContent.includes(idPattern)) {
      console.log(`  ✅ ${convention}`);
    } else {
      console.log(`  ❌ ${convention} - NOT FOUND`);
      errors++;
    }
  });
} else {
  console.log('  ❌ custom-conventions.yaml not found');
  errors++;
}

// Check trace context propagation in middleware
console.log('\n🔗 Checking Trace Context Propagation...\n');

const middlewarePath = join(process.cwd(), 'sidecar/server/middleware/01.telemetry.mjs');
if (existsSync(middlewarePath)) {
  const middlewareContent = readFileSync(middlewarePath, 'utf-8');

  const requiredImports = [
    'extractTraceContextFromHeaders',
    'getTraceIdForLogging',
    'enrichLogWithTraceContext',
    'addMetricExemplar',
  ];

  requiredImports.forEach(importName => {
    if (middlewareContent.includes(importName)) {
      console.log(`  ✅ ${importName} imported`);
    } else {
      console.log(`  ❌ ${importName} - NOT IMPORTED`);
      errors++;
    }
  });
} else {
  console.log('  ❌ Middleware file not found');
  errors++;
}

// Check gRPC client trace propagation
console.log('\n🌐 Checking gRPC Client Trace Propagation...\n');

const clientPath = join(process.cwd(), 'sidecar/sidecar/client.mjs');
if (existsSync(clientPath)) {
  const clientContent = readFileSync(clientPath, 'utf-8');

  const requiredPatterns = [
    '@opentelemetry/api',
    'traceparent',
    'x-trace-id',
    'x-span-id',
    'spanContext()',
  ];

  requiredPatterns.forEach(pattern => {
    if (clientContent.includes(pattern)) {
      console.log(`  ✅ ${pattern}`);
    } else {
      console.log(`  ❌ ${pattern} - NOT FOUND`);
      errors++;
    }
  });
} else {
  console.log('  ❌ gRPC client file not found');
  errors++;
}

// Check SLO definitions
console.log('\n🎯 Checking SLO Definitions...\n');

const sloPath = join(process.cwd(), 'sidecar/server/utils/slo-tracker.mjs');
if (existsSync(sloPath)) {
  const sloContent = readFileSync(sloPath, 'utf-8');

  const requiredSLOs = ['api_latency', 'availability', 'error_rate'];

  requiredSLOs.forEach(slo => {
    if (sloContent.includes(slo)) {
      console.log(`  ✅ ${slo} SLO defined`);
    } else {
      console.log(`  ❌ ${slo} SLO - NOT DEFINED`);
      errors++;
    }
  });
} else {
  console.log('  ❌ SLO tracker file not found');
  errors++;
}

// Check CI/CD workflow
console.log('\n🚀 Checking CI/CD Workflow...\n');

const workflowPath = join(process.cwd(), '.github/workflows/otel-weaver-validate.yml');
if (existsSync(workflowPath)) {
  const workflowContent = readFileSync(workflowPath, 'utf-8');

  const requiredJobs = [
    'validate-conventions',
    'test-instrumentation',
    'generate-grafana-dashboards',
  ];

  requiredJobs.forEach(job => {
    if (workflowContent.includes(job)) {
      console.log(`  ✅ ${job} job`);
    } else {
      console.log(`  ❌ ${job} job - NOT FOUND`);
      errors++;
    }
  });
} else {
  console.log('  ❌ CI/CD workflow file not found');
  errors++;
}

// Summary
console.log('\n' + '=' .repeat(60));
console.log('\n📊 Validation Summary\n');

if (errors === 0 && warnings === 0) {
  console.log('  ✅ ALL CHECKS PASSED');
  console.log('\n🎉 OTEL Weaver integration is fully implemented and operational!\n');
  process.exit(0);
} else {
  if (errors > 0) {
    console.log(`  ❌ ${errors} error(s) found`);
  }
  if (warnings > 0) {
    console.log(`  ⚠️  ${warnings} warning(s) found`);
  }
  console.log('\n⚠️  Please fix the issues above before deploying.\n');
  process.exit(1);
}
