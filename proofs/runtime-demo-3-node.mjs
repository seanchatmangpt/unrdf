/**
 * @file Demo 3: Hook Execution (Node.js Version)
 * @description Execute a knowledge hook in Node.js
 */

// Minimal hook implementation for Node.js
class NodeHookExecutor {
  constructor() {
    this.metrics = {
      executions: 0,
      errors: 0,
      totalDuration: 0,
    };
  }

  async execute(hook, event, _options = {}) {
    const startTime = Date.now();
    this.metrics.executions++;

    try {
      let result;

      if (hook.run) {
        result = await hook.run(event);
      } else {
        result = { success: true };
      }

      const duration = Date.now() - startTime;
      this.metrics.totalDuration += duration;

      return {
        success: true,
        result,
        duration,
        timestamp: Date.now(),
      };
    } catch (error) {
      this.metrics.errors++;
      const duration = Date.now() - startTime;
      this.metrics.totalDuration += duration;

      return {
        success: false,
        error: error.message,
        duration,
        timestamp: Date.now(),
      };
    }
  }

  getMetrics() {
    return { ...this.metrics };
  }
}

async function main() {
  console.log('=== Demo 3: Hook Execution (Node.js) ===\n');

  try {
    // Create hook executor
    const executor = new NodeHookExecutor();
    console.log('✓ Created hook executor');

    // Define a sample hook
    const validationHook = {
      meta: {
        name: 'validate-user-data',
        version: '1.0.0',
        description: 'Validates user data format',
      },
      run: async event => {
        const { user } = event;

        if (!user || !user.name || !user.age) {
          throw new Error('Invalid user data: missing name or age');
        }

        if (user.age < 0 || user.age > 150) {
          throw new Error('Invalid age: must be between 0 and 150');
        }

        return {
          valid: true,
          user,
          validatedAt: new Date().toISOString(),
        };
      },
    };

    console.log(`✓ Registered hook: ${validationHook.meta.name}\n`);

    // Test 1: Valid user
    console.log('Test 1: Valid user data');
    const result1 = await executor.execute(validationHook, {
      user: { name: 'Alice Smith', age: 30 },
    });
    console.log(`  Success: ${result1.success}`);
    console.log(`  Valid: ${result1.result?.valid}`);
    console.log(`  Duration: ${result1.duration}ms`);

    // Test 2: Invalid user (missing age)
    console.log('\nTest 2: Invalid user data (missing age)');
    const result2 = await executor.execute(validationHook, {
      user: { name: 'Bob Jones' },
    });
    console.log(`  Success: ${result2.success}`);
    console.log(`  Error: ${result2.error}`);
    console.log(`  Duration: ${result2.duration}ms`);

    // Test 3: Invalid age
    console.log('\nTest 3: Invalid user data (age out of range)');
    const result3 = await executor.execute(validationHook, {
      user: { name: 'Charlie Brown', age: 200 },
    });
    console.log(`  Success: ${result3.success}`);
    console.log(`  Error: ${result3.error}`);
    console.log(`  Duration: ${result3.duration}ms`);

    // Show metrics
    const metrics = executor.getMetrics();
    console.log('\nMetrics:');
    console.log(`  Total executions: ${metrics.executions}`);
    console.log(`  Errors: ${metrics.errors}`);
    console.log(`  Average duration: ${(metrics.totalDuration / metrics.executions).toFixed(2)}ms`);

    console.log('\n✅ Success: Hook execution completed in Node.js');

    return metrics;
  } catch (error) {
    console.error('❌ Error:', error.message);
    throw error;
  }
}

main().catch(err => {
  console.error('Fatal error:', err);
  process.exit(1);
});
