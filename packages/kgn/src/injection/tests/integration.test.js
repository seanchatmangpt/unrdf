/**
 * KGEN Injection Integration Tests
 *
 * Tests for integration with the main KGEN template engine,
 * including template processing with injection support.
 */

import { describe, test, expect, beforeEach, afterEach } from 'vitest';
import { promises as fs } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';

import { inject, dryRun, processTemplate, initializeInjection } from '../api.js';
import { enhanceKgenWithInjection } from '../integration.js';
import { INJECTION_MODES } from '../constants.js';

describe('KGEN Injection Integration', () => {
  let tempDir;
  let testFiles;

  beforeEach(async () => {
    tempDir = await fs.mkdtemp(join(tmpdir(), 'kgen-integration-test-'));
    await fs.mkdir(join(tempDir, 'src'), { recursive: true });

    testFiles = {
      routes: join(tempDir, 'src', 'routes.ts'),
      config: join(tempDir, 'src', 'config.ts'),
      template: join(tempDir, 'templates', 'route.kgen')
    };

    // Initialize injection system
    initializeInjection({
      projectRoot: tempDir,
      backupEnabled: true
    });
  });

  afterEach(async () => {
    await fs.rm(tempDir, { recursive: true, force: true });
  });

  describe('API Functions', () => {
    test('should inject content using API', async () => {
      // Setup
      const initialContent = 'export const config = { port: 3000 };';
      await fs.writeFile(testFiles.config, initialContent);

      const templateConfig = {
        to: 'src/config.ts',
        inject: true,
        mode: 'append'
      };

      const content = '\nexport const database = "mongodb://localhost";';
      const variables = {};

      // Execute
      const result = await inject(templateConfig, content, variables);

      // Verify
      expect(result.success).toBe(true);
      expect(result.operationId).toBeDefined();

      const finalContent = await fs.readFile(testFiles.config, 'utf8');
      expect(finalContent).toContain('export const database');
    });

    test('should perform dry run without modifications', async () => {
      // Setup
      const initialContent = 'const original = true;';
      await fs.writeFile(testFiles.config, initialContent);

      const templateConfig = {
        to: 'src/config.ts',
        inject: true,
        mode: 'append'
      };

      const content = 'const added = true;';

      // Execute dry run
      const result = await dryRun(templateConfig, content, {});

      // Verify
      expect(result.targets).toHaveLength(1);
      expect(result.targets[0].valid).toBe(true);

      // File should not be modified
      const finalContent = await fs.readFile(testFiles.config, 'utf8');
      expect(finalContent).toBe(initialContent);
    });

    test('should process template with frontmatter', async () => {
      // Setup template with frontmatter
      const template = `---
to: src/routes.ts
inject: true
mode: before
target: "export default"
---
router.{{method}}('{{path}}', {{handler}});`;

      // Create target file
      const initialContent = `const router = Router();

export default router;`;
      await fs.writeFile(testFiles.routes, initialContent);

      const data = {
        method: 'get',
        path: '/users',
        handler: 'getUsers'
      };

      // Execute
      const result = await processTemplate(template, data);

      // Verify
      expect(result.success).toBe(true);
      expect(result.operationId).toBeDefined();

      const finalContent = await fs.readFile(testFiles.routes, 'utf8');
      expect(finalContent).toContain("router.get('/users', getUsers);");
    });

    test('should handle template without frontmatter', async () => {
      const template = 'Simple template without frontmatter';
      const data = {};

      // Should throw error since only injection templates are supported
      await expect(processTemplate(template, data))
        .rejects.toThrow('Regular template processing not implemented');
    });
  });

  describe('Template Engine Enhancement', () => {
    test('should enhance existing KGEN engine with injection', async () => {
      // Mock KGEN engine
      const mockKgenEngine = {
        render: async (templatePath, data) => `Rendered: ${templatePath}`,
        renderString: async (templateString, data) => `Rendered: ${templateString}`,
        getTemplate: async (templatePath) => ({
          frontmatter: { inject: true, to: 'src/test.ts', mode: 'append' },
          content: 'test content {{name}}'
        })
      };

      // Enhance with injection
      const enhancedEngine = enhanceKgenWithInjection(mockKgenEngine, {
        projectRoot: tempDir
      });

      // Verify new methods are added
      expect(enhancedEngine.inject).toBeDefined();
      expect(enhancedEngine.dryRunInjection).toBeDefined();
      expect(enhancedEngine.getInjectionHistory).toBeDefined();
      expect(enhancedEngine.undoInjection).toBeDefined();
      expect(enhancedEngine.processBatch).toBeDefined();

      // Test injection method
      await fs.writeFile(join(tempDir, 'src', 'test.ts'), 'original content');

      const result = await enhancedEngine.inject(
        { to: 'src/test.ts', inject: true, mode: 'append' },
        'injected content',
        {}
      );

      expect(result.success).toBe(true);
    });

    test('should process batch templates with mixed injection/regular', async () => {
      const mockKgenEngine = {
        render: async () => 'Regular render result',
        renderString: async () => 'Regular renderString result',
        getTemplate: async (templatePath) => {
          if (templatePath.includes('injection')) {
            return {
              frontmatter: { inject: true, to: 'src/test.ts', mode: 'append' },
              content: 'injected {{content}}'
            };
          }
          return { frontmatter: {}, content: 'regular template' };
        }
      };

      const enhancedEngine = enhanceKgenWithInjection(mockKgenEngine, {
        projectRoot: tempDir
      });

      // Setup target file
      await fs.writeFile(join(tempDir, 'src', 'test.ts'), 'original');

      const templates = [
        {
          path: 'injection-template.kgen',
          data: { content: 'test' }
        },
        {
          path: 'regular-template.kgen',
          data: { name: 'test' }
        }
      ];

      const result = await enhancedEngine.processBatch(templates);

      expect(result.total).toBe(2);
      expect(result.successful).toBeGreaterThan(0);
    });
  });

  describe('Error Handling', () => {
    test('should handle missing template variables', async () => {
      const templateConfig = {
        to: 'src/{{filename}}.ts', // Missing filename variable
        inject: true,
        mode: 'create'
      };

      const content = 'test content';
      const variables = {}; // Missing 'filename'

      await expect(inject(templateConfig, content, variables))
        .rejects.toThrow(/Variable 'filename' not provided/);
    });

    test('should handle invalid injection mode', async () => {
      const templateConfig = {
        to: 'src/test.ts',
        inject: true,
        mode: 'invalid-mode'
      };

      const content = 'test content';
      const variables = {};

      await expect(inject(templateConfig, content, variables))
        .rejects.toThrow(/Unknown injection mode/);
    });

    test('should handle path traversal attempts', async () => {
      const templateConfig = {
        to: '../../../etc/passwd', // Path traversal attempt
        inject: true,
        mode: 'create'
      };

      const content = 'malicious content';

      await expect(inject(templateConfig, content, {}))
        .rejects.toThrow(/Path traversal blocked/);
    });
  });

  describe('Complex Scenarios', () => {
    test('should handle multi-target injection', async () => {
      // Setup multiple target files
      await fs.writeFile(join(tempDir, 'src', 'routes.ts'), 'export const routes = [];');
      await fs.writeFile(join(tempDir, 'src', 'handlers.ts'), 'export const handlers = {};');

      const templateConfig = {
        targets: [
          {
            to: 'src/routes.ts',
            inject: true,
            mode: 'before',
            target: 'export const routes'
          },
          {
            to: 'src/handlers.ts',
            inject: true,
            mode: 'before',
            target: 'export const handlers'
          }
        ]
      };

      const content = '// Auto-generated code';

      const result = await inject(templateConfig, content, {});

      expect(result.success).toBe(true);
      expect(result.targets).toBe(2);

      // Verify both files were modified
      const routesContent = await fs.readFile(join(tempDir, 'src', 'routes.ts'), 'utf8');
      const handlersContent = await fs.readFile(join(tempDir, 'src', 'handlers.ts'), 'utf8');

      expect(routesContent).toContain('// Auto-generated code');
      expect(handlersContent).toContain('// Auto-generated code');
    });

    test('should handle glob patterns with exclusions', async () => {
      // Setup multiple TypeScript files
      await fs.mkdir(join(tempDir, 'src', 'components'), { recursive: true });
      await fs.writeFile(join(tempDir, 'src', 'components', 'Button.ts'), 'export class Button {}');
      await fs.writeFile(join(tempDir, 'src', 'components', 'Input.ts'), 'export class Input {}');
      await fs.writeFile(join(tempDir, 'src', 'components', 'Button.test.ts'), 'test code');
      await fs.writeFile(join(tempDir, 'src', 'components', 'Input.spec.ts'), 'spec code');

      const templateConfig = {
        to: 'src/components/*.ts',
        inject: true,
        mode: 'prepend',
        exclude: ['*.test.ts', '*.spec.ts']
      };

      const content = '// Auto-generated imports';

      const result = await inject(templateConfig, content, {});

      expect(result.success).toBe(true);
      expect(result.targets).toBe(2); // Only Button.ts and Input.ts, not test files

      // Verify only non-test files were modified
      const buttonContent = await fs.readFile(join(tempDir, 'src', 'components', 'Button.ts'), 'utf8');
      const inputContent = await fs.readFile(join(tempDir, 'src', 'components', 'Input.ts'), 'utf8');
      const testContent = await fs.readFile(join(tempDir, 'src', 'components', 'Button.test.ts'), 'utf8');

      expect(buttonContent).toContain('// Auto-generated imports');
      expect(inputContent).toContain('// Auto-generated imports');
      expect(testContent).not.toContain('// Auto-generated imports');
    });

    test('should handle conditional injection with complex skipIf', async () => {
      // Setup file with existing content
      const existingContent = `import express from 'express';
import cors from 'cors';

const app = express();
app.use(cors());`;

      await fs.writeFile(testFiles.routes, existingContent);

      const templateConfig = {
        to: 'src/routes.ts',
        inject: true,
        mode: 'prepend',
        skipIf: ['/cors/', 'express'],
        skipIfLogic: 'OR'
      };

      const content = 'import helmet from "helmet";';

      const result = await inject(templateConfig, content, {});

      // Should be skipped because file contains 'cors'
      expect(result.success).toBe(true);
      expect(result.skipped).toBe(true);

      // File should not be modified
      const finalContent = await fs.readFile(testFiles.routes, 'utf8');
      expect(finalContent).toBe(existingContent);
    });

    test('should handle rollback on partial multi-target failure', async () => {
      // Setup target files
      await fs.writeFile(join(tempDir, 'src', 'file1.ts'), 'original content 1');
      await fs.writeFile(join(tempDir, 'src', 'file2.ts'), 'original content 2');

      // Make file2 read-only to cause failure
      await fs.chmod(join(tempDir, 'src', 'file2.ts'), 0o444);

      const templateConfig = {
        targets: [
          {
            to: 'src/file1.ts',
            inject: true,
            mode: 'append'
          },
          {
            to: 'src/file2.ts',
            inject: true,
            mode: 'append'
          }
        ]
      };

      const content = 'new content';

      // Should fail due to read-only file2
      await expect(inject(templateConfig, content, {}))
        .rejects.toThrow();

      // file1 should be rolled back to original content
      const file1Content = await fs.readFile(join(tempDir, 'src', 'file1.ts'), 'utf8');
      expect(file1Content).toBe('original content 1');

      // Restore permissions for cleanup
      await fs.chmod(join(tempDir, 'src', 'file2.ts'), 0o644);
    });
  });
});