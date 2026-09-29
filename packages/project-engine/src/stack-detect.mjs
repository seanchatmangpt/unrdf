/**
 * @file Stack detection - identify React/Next/Nest/Express and Jest/Vitest/Mocha
 * @module project-engine/stack-detect
 */

import { z } from 'zod';
import { FS } from './namespaces.mjs';
import { dataFactory } from '@unrdf/oxigraph';

const { namedNode } = dataFactory;

const StackDetectOptionsSchema = z.object({
  fsStore: z.custom(val => val && typeof val.getQuads === 'function', {
    message: 'fsStore must be an RDF store with getQuads method',
  }),
});

const hasAny = (paths, names) => names.some(name => paths.has(name));

/**
 * Detect tech stack from the filesystem graph.
 *
 * @param {Object} options
 * @param {Object} options.fsStore - Store produced by scanFileSystemToStore
 * @returns {{uiFramework: string|null, webFramework: string|null, apiFramework: string|null, testFramework: string|null, packageManager: string|null}}
 */
export function detectStackFromFs(options) {
  const { fsStore } = StackDetectOptionsSchema.parse(options);

  const paths = new Set(
    fsStore.getQuads(null, namedNode(FS.relativePath), null).map(quad => quad.object.value)
  );

  const stack = {
    uiFramework: null,
    webFramework: null,
    apiFramework: null,
    testFramework: null,
    packageManager: null,
  };

  if (hasAny(paths, ['src/app', 'app', 'src/pages', 'pages', 'src/views', 'src/components'])) {
    stack.uiFramework = 'react';
  }

  const nextConfig = hasAny(paths, ['next.config.js', 'next.config.mjs', 'next.config.ts']);
  if (nextConfig || paths.has('src/app')) {
    if (paths.has('src/app')) stack.webFramework = 'next-app-router';
    else if (hasAny(paths, ['src/pages', 'pages'])) stack.webFramework = 'next-pages';
    else stack.webFramework = 'next';
  } else if (paths.has('nest-cli.json')) {
    stack.webFramework = 'nest';
    stack.apiFramework = 'nest';
  } else if (hasAny(paths, ['src/server.js', 'src/app.js', 'src/server.mjs', 'src/app.mjs'])) {
    stack.webFramework = 'express';
    stack.apiFramework = 'express';
  }

  if (hasAny(paths, ['vitest.config.js', 'vitest.config.mjs', 'vitest.config.ts'])) {
    stack.testFramework = 'vitest';
  } else if (hasAny(paths, ['jest.config.js', 'jest.config.json', 'jest.config.ts'])) {
    stack.testFramework = 'jest';
  } else if (hasAny(paths, ['.mocharc.json', '.mocharc.js'])) {
    stack.testFramework = 'mocha';
  }

  if (paths.has('pnpm-lock.yaml')) stack.packageManager = 'pnpm';
  else if (paths.has('yarn.lock')) stack.packageManager = 'yarn';
  else if (paths.has('package-lock.json')) stack.packageManager = 'npm';
  else if (paths.has('bun.lockb')) stack.packageManager = 'bun';

  return stack;
}
