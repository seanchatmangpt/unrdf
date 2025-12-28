import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['**/*.{test,spec,unit,integration,property,determinism,adversarial}.mjs'],
    exclude: ['node_modules', 'dist', '.idea', '.git', '.cache', '**/fixtures/**', '**/helpers/**'],
    testTimeout: 10000,
    hookTimeout: 10000,
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'],
      include: ['src/**/*.mjs'],
      exclude: [
        'src/**/*.test.mjs',
        'src/**/*.spec.mjs',
        'test/**/*'
      ],
      lines: 90,
      functions: 90,
      branches: 80,
      statements: 90
    },
    reporters: ['verbose'],
    bail: 1,
  }
});
