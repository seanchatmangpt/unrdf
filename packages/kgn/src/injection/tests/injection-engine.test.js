/**
 * KGEN Injection Engine Tests
 *
 * Comprehensive tests for the injection engine covering all
 * atomic operations, idempotency, and error handling.
 */

import { describe, test, expect, beforeEach, afterEach, vi as _vi } from 'vitest';
import { promises as fs } from 'fs';
import { join, dirname as _dirname } from 'path';
import { tmpdir } from 'os';

import { InjectionEngine } from '../injection-engine.js';
import { INJECTION_MODES, ERROR_CODES as _ERROR_CODES } from '../constants.js';

describe('InjectionEngine', () => {
  let tempDir;
  let engine;
  let testFiles;

  beforeEach(async () => {
    // Create temporary directory for tests
    tempDir = await fs.mkdtemp(join(tmpdir(), 'kgen-injection-test-'));

    // Initialize injection engine
    engine = new InjectionEngine({
      projectRoot: tempDir,
      backupEnabled: true,
      atomicWrites: true
    });

    testFiles = {
      routes: join(tempDir, 'src', 'routes.ts'),
      config: join(tempDir, 'src', 'config.ts'),
      index: join(tempDir, 'src', 'index.ts')
    };

    // Create test directory structure
    await fs.mkdir(join(tempDir, 'src'), { recursive: true });
  });

  afterEach(async () => {
    // Clean up temporary directory
    await fs.rm(tempDir, { recursive: true, force: true });
  });

  describe('Basic Injection Operations', () => {
    test('should append content to file', async () => {
      // Setup
      const initialContent = 'export { Router } from "./router";\n';
      await fs.writeFile(testFiles.index, initialContent);

      const templateConfig = {
        to: 'src/index.ts',
        inject: true,
        mode: INJECTION_MODES.APPEND
      };

      const injectionContent = 'export { UserService } from "./services/user";';
      const variables = {};

      // Execute
      const result = await engine.inject(templateConfig, injectionContent, variables);

      // Verify
      expect(result.success).toBe(true);
      expect(result.operationId).toMatch(/^injection-[a-f0-9]{16}$/);

      const finalContent = await fs.readFile(testFiles.index, 'utf8');
      expect(finalContent).toContain(initialContent.trim());
      expect(finalContent).toContain(injectionContent);
    });

    test('should prepend content to file', async () => {
      // Setup
      const initialContent = 'import express from "express";\n\nconst app = express();';
      await fs.writeFile(testFiles.routes, initialContent);

      const templateConfig = {
        to: 'src/routes.ts',
        inject: true,
        mode: INJECTION_MODES.PREPEND
      };

      const injectionContent = 'import { cors } from "cors";';
      const variables = {};

      // Execute
      const result = await engine.inject(templateConfig, injectionContent, variables);

      // Verify
      expect(result.success).toBe(true);

      const finalContent = await fs.readFile(testFiles.routes, 'utf8');
      expect(finalContent).toMatch(/^import { cors } from "cors";\nimport express from "express";/);
    });

    test('should inject before specific pattern', async () => {
      // Setup
      const initialContent = `import { Router } from 'express';

const router = Router();

// Existing routes
router.get('/health', healthCheck);

export default router;`;
      await fs.writeFile(testFiles.routes, initialContent);

      const templateConfig = {
        to: 'src/routes.ts',
        inject: true,
        mode: INJECTION_MODES.BEFORE,
        target: 'export default router;'
      };

      const injectionContent = "router.get('/users', getUsers);";
      const variables = {};

      // Execute
      const result = await engine.inject(templateConfig, injectionContent, variables);

      // Verify
      expect(result.success).toBe(true);

      const finalContent = await fs.readFile(testFiles.routes, 'utf8');
      expect(finalContent).toContain("router.get('/users', getUsers);");
      expect(finalContent.indexOf("router.get('/users', getUsers);"))
        .toBeLessThan(finalContent.indexOf('export default router;'));
    });

    test('should inject after specific pattern', async () => {
      // Setup
      const initialContent = `const router = Router();

// Existing routes
router.get('/health', healthCheck);

export default router;`;
      await fs.writeFile(testFiles.routes, initialContent);

      const templateConfig = {
        to: 'src/routes.ts',
        inject: true,
        mode: INJECTION_MODES.AFTER,
        target: '// Existing routes'
      };

      const injectionContent = "router.post('/users', createUser);";
      const variables = {};

      // Execute
      const result = await engine.inject(templateConfig, injectionContent, variables);

      // Verify
      expect(result.success).toBe(true);

      const finalContent = await fs.readFile(testFiles.routes, 'utf8');
      expect(finalContent).toContain("router.post('/users', createUser);");
      expect(finalContent.indexOf('// Existing routes'))
        .toBeLessThan(finalContent.indexOf("router.post('/users', createUser);"));
    });

    test('should replace specific content', async () => {
      // Setup
      const initialContent = `export const config = {
  port: 3000,
  host: 'localhost'
};`;
      await fs.writeFile(testFiles.config, initialContent);

      const templateConfig = {
        to: 'src/config.ts',
        inject: true,
        mode: INJECTION_MODES.REPLACE,
        target: 'port: 3000',
        exact: true
      };

      const injectionContent = 'port: 8080';
      const variables = {};

      // Execute
      const result = await engine.inject(templateConfig, injectionContent, variables);

      // Verify
      expect(result.success).toBe(true);

      const finalContent = await fs.readFile(testFiles.config, 'utf8');
      expect(finalContent).toContain('port: 8080');
      expect(finalContent).not.toContain('port: 3000');
    });

    test('should create new file when mode is create', async () => {
      const templateConfig = {
        to: 'src/models/User.ts',
        inject: true,
        mode: INJECTION_MODES.CREATE,
        createIfMissing: true,
        createDirectories: true
      };

      const injectionContent = `export interface User {
  id: string;
  name: string;
}`;
      const variables = {};

      // Execute
      const result = await engine.inject(templateConfig, injectionContent, variables);

      // Verify
      expect(result.success).toBe(true);

      const userModelPath = join(tempDir, 'src', 'models', 'User.ts');
      const exists = await fs.access(userModelPath).then(() => true, () => false);
      expect(exists).toBe(true);

      const content = await fs.readFile(userModelPath, 'utf8');
      expect(content).toContain('export interface User');
    });
  });

  describe('Idempotency', () => {
    test('should skip injection when content already exists', async () => {
      // Setup
      const initialContent = `export const config = {
  port: 3000,
  database: 'mongodb://localhost:27017'
};`;
      await fs.writeFile(testFiles.config, initialContent);

      const templateConfig = {
        to: 'src/config.ts',
        inject: true,
        mode: INJECTION_MODES.BEFORE,
        target: '};',
        skipIf: 'database:'
      };

      const injectionContent = "  database: 'mongodb://localhost:27017'";
      const variables = {};

      // Execute
      const result = await engine.inject(templateConfig, injectionContent, variables);

      // Verify
      expect(result.success).toBe(true);
      expect(result.skipped).toBe(true);

      const finalContent = await fs.readFile(testFiles.config, 'utf8');
      // Should not have duplicate database entries
      const databaseMatches = (finalContent.match(/database:/g) || []).length;
      expect(databaseMatches).toBe(1);
    });

    test('should skip injection with regex pattern', async () => {
      // Setup
      const initialContent = `import express from 'express';
import cors from 'cors';`;
      await fs.writeFile(testFiles.routes, initialContent);

      const templateConfig = {
        to: 'src/routes.ts',
        inject: true,
        mode: INJECTION_MODES.PREPEND,
        skipIf: '/import.*cors/'
      };

      const injectionContent = 'import cors from "cors";';
      const variables = {};

      // Execute
      const result = await engine.inject(templateConfig, injectionContent, variables);

      // Verify
      expect(result.success).toBe(true);
      expect(result.skipped).toBe(true);
    });

    test('should handle multiple skipIf conditions with OR logic', async () => {
      // Setup
      const initialContent = 'router.get("/users", getUsers);';
      await fs.writeFile(testFiles.routes, initialContent);

      const templateConfig = {
        to: 'src/routes.ts',
        inject: true,
        mode: INJECTION_MODES.APPEND,
        skipIf: ['/users', 'getUsers'],
        skipIfLogic: 'OR'
      };

      const injectionContent = 'router.post("/users", createUser);';
      const variables = {};

      // Execute
      const result = await engine.inject(templateConfig, injectionContent, variables);

      // Verify - should skip because either condition matches
      expect(result.success).toBe(true);
      expect(result.skipped).toBe(true);
    });

    test('should handle force flag to override skipIf', async () => {
      // Setup
      const initialContent = 'router.get("/users", getUsers);';
      await fs.writeFile(testFiles.routes, initialContent);

      const templateConfig = {
        to: 'src/routes.ts',
        inject: true,
        mode: INJECTION_MODES.APPEND,
        skipIf: '/users',
        force: true
      };

      const injectionContent = 'router.post("/users", createUser);';
      const variables = {};

      // Execute
      const result = await engine.inject(templateConfig, injectionContent, variables);

      // Verify - should proceed despite skipIf because of force
      expect(result.success).toBe(true);
      expect(result.skipped).toBe(false);

      const finalContent = await fs.readFile(testFiles.routes, 'utf8');
      expect(finalContent).toContain('router.post("/users", createUser);');
    });
  });

  describe('Atomic Operations', () => {
    test('should rollback changes on validation failure', async () => {
      // Setup
      const initialContent = 'const validCode = true;';
      await fs.writeFile(testFiles.config, initialContent);

      const templateConfig = {
        to: 'src/config.ts',
        inject: true,
        mode: INJECTION_MODES.APPEND,
        validateSyntax: true
      };

      // Invalid JavaScript that should fail validation
      const injectionContent = 'const invalid = {';
      const variables = {};

      // Execute & Verify
      await expect(engine.inject(templateConfig, injectionContent, variables))
        .rejects.toThrow();

      // Original content should be preserved
      const finalContent = await fs.readFile(testFiles.config, 'utf8');
      expect(finalContent).toBe(initialContent);
    });

    test('should handle concurrent operations safely', async () => {
      // Setup
      const initialContent = 'let counter = 0;\n';
      await fs.writeFile(testFiles.config, initialContent);

      const templateConfig = {
        to: 'src/config.ts',
        inject: true,
        mode: INJECTION_MODES.APPEND
      };

      // Execute multiple concurrent injections
      const promises = Array.from({ length: 5 }, (_, i) => {
        return engine.inject(templateConfig, `// Injection ${i}\n`, {});
      });

      const results = await Promise.all(promises);

      // Verify all operations succeeded
      results.forEach(result => {
        expect(result.success).toBe(true);
      });

      // Verify final content has all injections
      const finalContent = await fs.readFile(testFiles.config, 'utf8');
      for (let i = 0; i < 5; i++) {
        expect(finalContent).toContain(`// Injection ${i}`);
      }
    });

    test('should create backups when enabled', async () => {
      // Setup
      const initialContent = 'const original = true;';
      await fs.writeFile(testFiles.config, initialContent);

      const templateConfig = {
        to: 'src/config.ts',
        inject: true,
        mode: INJECTION_MODES.APPEND
      };

      const injectionContent = 'const added = true;';
      const variables = {};

      // Execute
      const result = await engine.inject(templateConfig, injectionContent, variables);

      // Verify
      expect(result.success).toBe(true);
      expect(result.results[0].backup).toBeDefined();

      // Check backup file exists
      const backupExists = await fs.access(result.results[0].backup)
        .then(() => true, () => false);
      expect(backupExists).toBe(true);

      // Check backup contains original content
      const backupContent = await fs.readFile(result.results[0].backup, 'utf8');
      expect(backupContent).toBe(initialContent);
    });
  });

  describe('Variable Interpolation', () => {
    test('should interpolate variables in injection content', async () => {
      // Setup
      await fs.writeFile(testFiles.routes, 'const router = Router();\n');

      const templateConfig = {
        to: 'src/routes.ts',
        inject: true,
        mode: INJECTION_MODES.APPEND
      };

      const injectionContent = 'router.{{method}}("{{path}}", {{handler}});';
      const variables = {
        method: 'get',
        path: '/users',
        handler: 'getUsers'
      };

      // Execute
      const result = await engine.inject(templateConfig, injectionContent, variables);

      // Verify
      expect(result.success).toBe(true);

      const finalContent = await fs.readFile(testFiles.routes, 'utf8');
      expect(finalContent).toContain('router.get("/users", getUsers);');
    });

    test('should interpolate variables in skipIf conditions', async () => {
      // Setup
      const initialContent = 'router.get("/users", getUsers);';
      await fs.writeFile(testFiles.routes, initialContent);

      const templateConfig = {
        to: 'src/routes.ts',
        inject: true,
        mode: INJECTION_MODES.APPEND,
        skipIf: '{{path}}'
      };

      const injectionContent = 'router.post("{{path}}", createUser);';
      const variables = { path: '/users' };

      // Execute
      const result = await engine.inject(templateConfig, injectionContent, variables);

      // Verify - should skip because /users already exists
      expect(result.success).toBe(true);
      expect(result.skipped).toBe(true);
    });
  });

  describe('Error Handling', () => {
    test('should handle target file not found', async () => {
      const templateConfig = {
        to: 'src/nonexistent.ts',
        inject: true,
        mode: INJECTION_MODES.BEFORE,
        target: 'some pattern'
      };

      const injectionContent = 'some content';
      const variables = {};

      // Execute & Verify
      await expect(engine.inject(templateConfig, injectionContent, variables))
        .rejects.toThrow(/ENOENT|Target file does not exist/);
    });

    test('should handle injection pattern not found', async () => {
      // Setup
      const initialContent = 'const existing = true;';
      await fs.writeFile(testFiles.config, initialContent);

      const templateConfig = {
        to: 'src/config.ts',
        inject: true,
        mode: INJECTION_MODES.BEFORE,
        target: 'NONEXISTENT_PATTERN'
      };

      const injectionContent = 'const added = true;';
      const variables = {};

      // Execute & Verify
      await expect(engine.inject(templateConfig, injectionContent, variables))
        .rejects.toThrow('Target pattern not found');
    });

    test('should handle read-only files', async () => {
      // Setup
      const initialContent = 'const readonly = true;';
      await fs.writeFile(testFiles.config, initialContent);
      await fs.chmod(testFiles.config, 0o444); // Read-only

      const templateConfig = {
        to: 'src/config.ts',
        inject: true,
        mode: INJECTION_MODES.APPEND
      };

      const injectionContent = 'const added = true;';
      const variables = {};

      // Execute & Verify
      await expect(engine.inject(templateConfig, injectionContent, variables))
        .rejects.toThrow();

      // Restore permissions for cleanup
      await fs.chmod(testFiles.config, 0o644);
    });
  });

  describe('Dry Run', () => {
    test('should perform dry run without modifying files', async () => {
      // Setup
      const initialContent = 'const original = true;';
      await fs.writeFile(testFiles.config, initialContent);

      const templateConfig = {
        to: 'src/config.ts',
        inject: true,
        mode: INJECTION_MODES.APPEND
      };

      const injectionContent = 'const added = true;';
      const variables = {};

      // Execute dry run
      const dryRunResult = await engine.dryRun(templateConfig, injectionContent, variables);

      // Verify dry run results
      expect(dryRunResult.targets).toHaveLength(1);
      expect(dryRunResult.targets[0].path).toBe(testFiles.config);
      expect(dryRunResult.targets[0].valid).toBe(true);

      // Verify file was not modified
      const finalContent = await fs.readFile(testFiles.config, 'utf8');
      expect(finalContent).toBe(initialContent);
    });
  });

  describe('Operation History', () => {
    test('should maintain operation history', async () => {
      // Setup
      await fs.writeFile(testFiles.config, 'const original = true;\n');

      const templateConfig = {
        to: 'src/config.ts',
        inject: true,
        mode: INJECTION_MODES.APPEND
      };

      const injectionContent = 'const added = true;';
      const variables = {};

      // Execute
      await engine.inject(templateConfig, injectionContent, variables);

      // Verify history
      const history = engine.getOperationHistory();
      expect(history).toHaveLength(1);
      expect(history[0].phase).toBe('committed');
      expect(history[0].results).toBeDefined();
      expect(history[0].metadata.timestamp).toBeDefined();
    });

    test('should record failed operations in history', async () => {
      const templateConfig = {
        to: 'src/nonexistent.ts',
        inject: true,
        mode: INJECTION_MODES.APPEND
      };

      const injectionContent = 'const added = true;';
      const variables = {};

      // Execute & expect failure
      try {
        await engine.inject(templateConfig, injectionContent, variables);
      } catch (error) {
        // Expected failure
      }

      // Verify failed operation is in history
      const history = engine.getOperationHistory();
      expect(history).toHaveLength(1);
      expect(history[0].error).toBeDefined();
    });
  });
});