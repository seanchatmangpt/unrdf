/**
 * @fileoverview Tests for Multi-pass Compilation Controller
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { executePassLoop, needsMultiplePass, getRecommendedPasses } from './pass-loop.mjs';

// =============================================================================
// Test Utilities
// =============================================================================

/**
 * Create mock compilation function
 * @param {Array<CompileResult>} results - Sequence of results to return
 * @returns {Function} Mock compile function
 */
function createMockCompile(results) {
  let callCount = 0;
  return async (_pass) => {
    const result = results[callCount] || results[results.length - 1];
    callCount++;
    return result;
  };
}

/**
 * Create minimal successful result
 */
function successResult(artifacts = null) {
  return {
    ok: true,
    pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]), // "%PDF" header
    log: 'Output written on main.pdf (1 page).',
    artifacts: artifacts !== undefined ? artifacts : new Map([
      ['main.log', new TextEncoder().encode('This is pdfTeX')], // .log is NOT a convergence file
    ]),
  };
}

/**
 * Create result with changed artifacts
 */
function changedResult(iteration = 1) {
  return {
    ok: true,
    pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
    log: 'Output written on main.pdf (1 page).\nLabel(s) may have changed. Rerun to get cross-references right.',
    artifacts: new Map([
      ['main.aux', new TextEncoder().encode(`\\relax\\iteration${iteration}`)],
    ]),
  };
}

/**
 * Create failure result
 */
function failureResult(missingInputs = []) {
  return {
    ok: false,
    log: missingInputs.length > 0
      ? `! LaTeX Error: File \`${missingInputs[0]}' not found.`
      : '! Undefined control sequence.',
    missingInputs,
    error: 'Compilation failed',
  };
}

// =============================================================================
// Test Suite: Single-Pass Success
// =============================================================================

describe('Pass Loop - Single-pass success', () => {
  it('should succeed in one pass for simple document', async () => {
    const compile = createMockCompile([successResult()]);
    const vfs = new Map();

    const result = await executePassLoop({
      compile,
      vfs,
      maxPasses: 5,
    });

    assert.equal(result.success, true);
    assert.equal(result.passes, 1);
    assert.ok(result.pdf);
    assert.match(result.terminationReason, /Single-pass document/);
  });

  it('should emit progress events', async () => {
    const compile = createMockCompile([successResult()]);
    const vfs = new Map();
    const events = [];

    await executePassLoop({
      compile,
      vfs,
      maxPasses: 5,
      onProgress: (event) => events.push(event),
    });

    // Should emit 'compiling' for pass 1, then 'converged' when done
    assert.equal(events.length, 2);
    assert.equal(events[0].pass, 1);
    assert.equal(events[0].total, 5);
    assert.equal(events[0].status, 'compiling');
    assert.equal(events[1].status, 'converged');
  });
});

// =============================================================================
// Test Suite: Two-Pass Convergence
// =============================================================================

describe('Pass Loop - Two-pass convergence', () => {
  it('should detect convergence after 2 passes', async () => {
    const auxContent = new TextEncoder().encode('\\newlabel{sec:intro}{{1}{1}}');

    const compile = createMockCompile([
      // Pass 1: Changed artifacts, rerun requested
      {
        ok: true,
        pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
        log: 'Label(s) may have changed. Rerun to get cross-references right.',
        artifacts: new Map([
          ['main.aux', auxContent],
          ['main.log', new TextEncoder().encode('pass 1')],
        ]),
      },
      // Pass 2: Same artifacts, no rerun message
      {
        ok: true,
        pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
        log: 'Output written on main.pdf (1 page).',
        artifacts: new Map([
          ['main.aux', auxContent],
          ['main.log', new TextEncoder().encode('pass 2')],
        ]),
      },
    ]);

    const vfs = new Map();

    const result = await executePassLoop({
      compile,
      vfs,
      maxPasses: 5,
    });

    assert.equal(result.success, true);
    assert.equal(result.passes, 2);
    assert.match(result.terminationReason, /Fixed point reached/);
  });

  it('should continue if artifacts change', async () => {
    const aux3 = new TextEncoder().encode('\\relax\\iteration3');

    const compile = createMockCompile([
      changedResult(1),
      changedResult(2),
      // Pass 3: Converged (same as pass 2 but no rerun message)
      {
        ok: true,
        pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
        log: 'Output written on main.pdf (1 page).',
        artifacts: new Map([['main.aux', aux3]]),
      },
      // Pass 4: Same as pass 3 (converged)
      {
        ok: true,
        pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
        log: 'Output written on main.pdf (1 page).',
        artifacts: new Map([['main.aux', aux3]]),
      },
    ]);

    const vfs = new Map();

    const result = await executePassLoop({
      compile,
      vfs,
      maxPasses: 5,
    });

    assert.equal(result.success, true);
    assert.equal(result.passes, 4); // Needs 4 passes because each has rerun message
  });

  it('should detect rerun messages in log', async () => {
    const rerunMessages = [
      'Rerun to get cross-references right',
      'Please rerun LaTeX',
      'Table widths have changed. Rerun LaTeX',
    ];

    for (const message of rerunMessages) {
      const auxContent = new TextEncoder().encode('\\relax');

      const compile = createMockCompile([
        {
          ok: true,
          pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
          log: message,
          artifacts: new Map([['main.aux', auxContent]]),
        },
        // Second pass: same aux, no rerun message
        {
          ok: true,
          pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
          log: 'Output written',
          artifacts: new Map([['main.aux', auxContent]]),
        },
      ]);

      const vfs = new Map();

      const result = await executePassLoop({
        compile,
        vfs,
        maxPasses: 5,
      });

      assert.equal(result.passes, 2, `Should run 2 passes for: ${message}`);
    }
  });
});

// =============================================================================
// Test Suite: Maximum Passes
// =============================================================================

describe('Pass Loop - Maximum passes', () => {
  it('should terminate at max passes', async () => {
    // Create a compile function that always requests rerun
    const compile = createMockCompile([changedResult(1)]);

    const vfs = new Map();

    const result = await executePassLoop({
      compile,
      vfs,
      maxPasses: 3,
    });

    assert.equal(result.success, true); // Still returns PDF
    assert.equal(result.passes, 3);
    assert.match(result.terminationReason, /Maximum passes reached/);
  });

  it('should warn if rerun still requested at max passes', async () => {
    const compile = createMockCompile([
      {
        ok: true,
        pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
        log: 'Rerun to get cross-references right',
        artifacts: new Map([['main.aux', new TextEncoder().encode('unstable')]]),
      },
    ]);

    const vfs = new Map();

    const result = await executePassLoop({
      compile,
      vfs,
      maxPasses: 2,
    });

    assert.equal(result.passes, 2);
    assert.match(result.terminationReason, /may need more passes/);
  });
});

// =============================================================================
// Test Suite: Missing Input Resolution
// =============================================================================

describe('Pass Loop - Missing input resolution', () => {
  it('should resolve missing inputs and retry', async () => {
    let compileCount = 0;
    const compile = async (_pass) => {
      compileCount++;
      if (compileCount === 1) {
        return failureResult(['fancyhdr.sty']);
      }
      // After resolution, return simple success (no convergence files)
      return {
        ok: true,
        pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
        log: 'Output written on main.pdf (1 page).',
        artifacts: new Map([
          ['main.log', new TextEncoder().encode('success')],
        ]),
      };
    };

    const vfs = new Map();
    const resolvedFiles = new Map([
      ['texmf/tex/latex/fancyhdr/fancyhdr.sty', new TextEncoder().encode('% fancyhdr')],
    ]);

    const onResolve = async (missingInputs) => {
      assert.deepEqual(missingInputs, ['fancyhdr.sty']);
      return resolvedFiles;
    };

    const result = await executePassLoop({
      compile,
      vfs,
      maxPasses: 5,
      maxResolveRetries: 3,
      onResolve,
    });

    assert.equal(result.success, true);
    assert.equal(result.passes, 1); // Should not count retry as separate pass
    assert.ok(vfs.has('texmf/tex/latex/fancyhdr/fancyhdr.sty'));
  });

  it('should emit resolving progress event', async () => {
    const compile = createMockCompile([
      failureResult(['tikz.sty']),
      successResult(),
    ]);

    const vfs = new Map();
    const events = [];

    const onResolve = async () => {
      return new Map([['texmf/tex/latex/tikz/tikz.sty', new Uint8Array()]]);
    };

    await executePassLoop({
      compile,
      vfs,
      onResolve,
      onProgress: (event) => events.push(event),
    });

    const resolvingEvent = events.find(e => e.status === 'resolving');
    assert.ok(resolvingEvent);
    assert.deepEqual(resolvingEvent.missing, ['tikz.sty']);
  });

  it('should fail after max resolve retries', async () => {
    const compile = createMockCompile([failureResult(['nonexistent.sty'])]);

    const vfs = new Map();
    let _resolveAttempts = 0;

    const onResolve = async () => {
      _resolveAttempts++;
      throw new Error('Package not found');
    };

    const result = await executePassLoop({
      compile,
      vfs,
      maxPasses: 5,
      maxResolveRetries: 2,
      onResolve,
    });

    assert.equal(result.success, false);
    assert.match(result.error, /Resolution failed/);
    assert.equal(result.terminationReason, 'resolution_error');
  });

  it('should fail if missing inputs without resolver', async () => {
    const compile = createMockCompile([failureResult(['missing.sty'])]);
    const vfs = new Map();

    const result = await executePassLoop({
      compile,
      vfs,
      maxPasses: 5,
      // No onResolve provided
    });

    assert.equal(result.success, false);
    assert.match(result.error, /Compilation failed/);
  });
});

// =============================================================================
// Test Suite: Error Handling
// =============================================================================

describe('Pass Loop - Error handling', () => {
  it('should handle compilation exception', async () => {
    const compile = async () => {
      throw new Error('Engine crashed');
    };

    const vfs = new Map();

    const result = await executePassLoop({
      compile,
      vfs,
      maxPasses: 5,
    });

    assert.equal(result.success, false);
    assert.match(result.error, /Compilation threw exception/);
    assert.equal(result.terminationReason, 'compile_exception');
  });

  it('should handle compilation success without PDF', async () => {
    const compile = createMockCompile([
      {
        ok: true,
        // No pdf field
        log: 'Compilation completed',
        artifacts: new Map(),
      },
    ]);

    const vfs = new Map();

    const result = await executePassLoop({
      compile,
      vfs,
      maxPasses: 5,
    });

    assert.equal(result.success, false);
    assert.match(result.error, /no PDF generated/);
  });

  it('should validate options', async () => {
    const result = await executePassLoop({
      // Missing required fields
      vfs: new Map(),
    });

    assert.equal(result.success, false);
    assert.equal(result.terminationReason, 'validation_error');
  });

  it('should handle invalid maxPasses', async () => {
    const compile = createMockCompile([successResult()]);

    const result = await executePassLoop({
      compile,
      vfs: new Map(),
      maxPasses: 100, // Exceeds max allowed
    });

    assert.equal(result.success, false);
    assert.match(result.log, /Invalid options|Too big/);
  });
});

// =============================================================================
// Test Suite: Fixed-Point Detection
// =============================================================================

describe('Pass Loop - Fixed-point detection', () => {
  it('should compare .aux files byte-for-byte', async () => {
    const aux1 = new TextEncoder().encode('\\newlabel{eq:1}{{1}{1}}');
    const aux2 = new TextEncoder().encode('\\newlabel{eq:1}{{1}{1}}'); // Identical

    const compile = createMockCompile([
      {
        ok: true,
        pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
        log: 'Rerun LaTeX', // First pass requests rerun
        artifacts: new Map([['main.aux', aux1]]),
      },
      {
        ok: true,
        pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
        log: 'Output written', // Second pass clean
        artifacts: new Map([['main.aux', aux2]]),
      },
    ]);

    const vfs = new Map();

    const result = await executePassLoop({
      compile,
      vfs,
      maxPasses: 5,
    });

    assert.equal(result.passes, 2);
    assert.match(result.terminationReason, /Fixed point/);
  });

  it('should check .toc files for convergence', async () => {
    const toc1 = new TextEncoder().encode('\\contentsline {section}{Intro}{1}');
    const toc2 = new TextEncoder().encode('\\contentsline {section}{Intro}{2}'); // Page changed

    const compile = createMockCompile([
      {
        ok: true,
        pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
        log: '',
        artifacts: new Map([['main.toc', toc1]]),
      },
      {
        ok: true,
        pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
        log: '',
        artifacts: new Map([['main.toc', toc2]]),
      },
      {
        ok: true,
        pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
        log: '',
        artifacts: new Map([['main.toc', toc2]]), // Stabilized
      },
    ]);

    const vfs = new Map();

    const result = await executePassLoop({
      compile,
      vfs,
      maxPasses: 5,
    });

    assert.equal(result.passes, 3);
    assert.match(result.terminationReason, /Fixed point/);
  });

  it('should ignore non-convergence files', async () => {
    // .log files change every run but shouldn't affect convergence
    const compile = createMockCompile([
      {
        ok: true,
        pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]),
        log: '',
        artifacts: new Map([
          ['main.aux', new TextEncoder().encode('\\relax')],
          ['main.log', new TextEncoder().encode('Log from pass 1')],
        ]),
      },
      {
        ok: true,
        pdf: new Uint8Array([0x25, 0x50, 0x46]),
        log: '',
        artifacts: new Map([
          ['main.aux', new TextEncoder().encode('\\relax')], // Same
          ['main.log', new TextEncoder().encode('Log from pass 2')], // Different
        ]),
      },
    ]);

    const vfs = new Map();

    const result = await executePassLoop({
      compile,
      vfs,
      maxPasses: 5,
    });

    // Should converge despite .log changing
    assert.equal(result.passes, 2);
    assert.match(result.terminationReason, /Fixed point/);
  });
});

// =============================================================================
// Test Suite: Utility Functions
// =============================================================================

describe('Utility - needsMultiplePass', () => {
  it('should detect cross-references', () => {
    const tex = '\\section{Introduction}\\label{sec:intro}\nSee \\ref{sec:intro}.';
    assert.equal(needsMultiplePass(tex), true);
  });

  it('should detect table of contents', () => {
    const tex = '\\tableofcontents';
    assert.equal(needsMultiplePass(tex), true);
  });

  it('should detect citations', () => {
    const tex = 'According to \\cite{einstein1905}.';
    assert.equal(needsMultiplePass(tex), true);
  });

  it('should return false for simple documents', () => {
    const tex = '\\documentclass{article}\\begin{document}Hello\\end{document}';
    assert.equal(needsMultiplePass(tex), false);
  });
});

describe('Utility - getRecommendedPasses', () => {
  it('should recommend 1 pass for simple doc', () => {
    const tex = '\\documentclass{article}\\begin{document}Hello\\end{document}';
    assert.equal(getRecommendedPasses(tex), 1);
  });

  it('should recommend 4 passes for bibliography', () => {
    const tex = '\\cite{ref1}\\bibliography{refs}';
    assert.equal(getRecommendedPasses(tex), 4);
  });

  it('should recommend 3 passes for cross-refs', () => {
    const tex = '\\tableofcontents\\ref{sec:intro}';
    assert.equal(getRecommendedPasses(tex), 3);
  });
});
