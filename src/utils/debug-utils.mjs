import { inspect } from "node:util";

/**
 * Pretty print first N quads
 * @param {import('n3').Store} store - RDF store to inspect
 * @param {number} [n=5] - Number of quads to show
 * @returns {string[]} Array of formatted quad strings
 */
export const previewQuads = (store, n = 5) =>
  [...store].slice(0, n).map((q) => 
    `${q.subject.value} ${q.predicate.value} ${q.object.value}${q.graph.termType === "DefaultGraph" ? "" : ` ${q.graph.value}`}`
  );

/**
 * Console.log Turtle for inspection
 * @param {import('n3').Store} store - RDF store to serialize
 * @param {import('n3').Parser} engine - RDF engine for serialization
 * @param {Object} [prefixes={}] - Optional prefix mappings
 * @returns {Promise<void>}
 */
export const dumpTurtle = async (store, engine, prefixes = {}) => {
  try {
    const turtle = await engine.serializeTurtle(store, { prefixes });
    console.log("=== Turtle Dump ===");
    console.log(turtle);
    console.log("==================");
  } catch (error) {
    console.error("Failed to dump Turtle:", error.message);
  }
};

/**
 * Quick Turtle dump with minimal prefixes
 * @param {import('n3').Store} store - RDF store to serialize
 * @param {Object} [options={}] - Optional options
 * @param {Object} [options.prefixes] - Optional prefix mappings
 * @param {number} [options.maxTriples] - Maximum number of triples to show
 * @returns {Promise<string>} Turtle string for debugging
 *
 * @example
 * const turtle = await debugTurtle(store, {
 *   prefixes: { ex: "http://example.org/" },
 *   maxTriples: 100
 * });
 * console.log(turtle);
 */
export const debugTurtle = async (store, options = {}) => {
  const { prefixes = { ex: "http://example.org/" }, maxTriples } = options;

  try {
    const engine = { serializeTurtle: async (s, opts) => {
      // Simple turtle serialization for debugging
      const turtleOptions = {
        prefixes,
        format: "text/turtle",
        ...opts
      };

      // For now, return a simple representation
      // In a real implementation, this would use proper Turtle serialization
      let turtle = "";
      const quads = [...store];
      const limitedQuads = maxTriples ? quads.slice(0, maxTriples) : quads;

      for (const quad of limitedQuads) {
        const subject = quad.subject.termType === "BlankNode"
          ? `_:${quad.subject.value}`
          : `<${quad.subject.value}>`;
        const predicate = `<${quad.predicate.value}>`;
        const object = quad.object.termType === "Literal"
          ? `"${quad.object.value}"`
          : quad.object.termType === "BlankNode"
          ? `_:${quad.object.value}`
          : `<${quad.object.value}>`;
        const graph = quad.graph.termType === "DefaultGraph"
          ? ""
          : ` <${quad.graph.value}>`;

        turtle += `${subject} ${predicate} ${object}${graph} .\n`;
      }

      if (maxTriples && quads.length > maxTriples) {
        turtle += `# ... ${quads.length - maxTriples} more triples\n`;
      }

      return turtle;
    }};

    return await engine.serializeTurtle(store, { prefixes });
  } catch (error) {
    return `Error serializing to Turtle: ${error.message}`;
  }
};

/**
 * Get store statistics
 * @param {import('n3').Store} store - RDF store to analyze
 * @returns {Object} Store statistics
 */
export const getStoreStats = (store) => {
  const subjects = new Set();
  const predicates = new Set();
  const objects = new Set();
  const graphs = new Set();
  
  for (const q of store) {
    subjects.add(q.subject.value);
    predicates.add(q.predicate.value);
    objects.add(q.object.value);
    if (q.graph.termType !== "DefaultGraph") {
      graphs.add(q.graph.value);
    }
  }
  
  return {
    quadCount: store.size,
    subjectCount: subjects.size,
    predicateCount: predicates.size,
    objectCount: objects.size,
    graphCount: graphs.size,
    subjects: [...subjects],
    predicates: [...predicates],
    objects: [...objects],
    graphs: [...graphs]
  };
};

/**
 * Print store statistics to console
 * @param {import('n3').Store} store - RDF store to analyze
 * @returns {void}
 */
export const printStoreStats = (store) => {
  const stats = getStoreStats(store);
  console.log("=== Store Statistics ===");
  console.log(`Quads: ${stats.quadCount}`);
  console.log(`Subjects: ${stats.subjectCount}`);
  console.log(`Predicates: ${stats.predicateCount}`);
  console.log(`Objects: ${stats.objectCount}`);
  console.log(`Graphs: ${stats.graphCount}`);
  console.log("========================");
};

/**
 * Deep inspect any object with custom formatting
 * @param {any} obj - Object to inspect
 * @param {Object} [options={}] - Inspection options
 * @returns {string} Formatted string representation
 */
export const deepInspect = (obj, options = {}) => {
  const defaultOptions = {
    depth: 3,
    colors: true,
    showHidden: false,
    maxArrayLength: 10,
    maxStringLength: 100,
    ...options
  };
  
  return inspect(obj, defaultOptions);
};

/**
 * Log object with deep inspection
 * @param {any} obj - Object to log
 * @param {string} [label] - Optional label for the log
 * @param {Object} [options] - Inspection options
 * @returns {void}
 */
export const logDeep = (obj, label = "Object", options = {}) => {
  console.log(`=== ${label} ===`);
  console.log(deepInspect(obj, options));
  console.log("==================");
};

/**
 * Time a function execution
 * @param {Function} fn - Function to time
 * @param {string} [label] - Optional label for timing
 * @returns {any} Function result
 */
export const timeExecution = async (fn, label = "Execution") => {
  const start = performance.now();
  const result = await fn();
  const end = performance.now();
  console.log(`${label}: ${(end - start).toFixed(2)}ms`);
  return result;
};

/**
 * Create a performance timer
 * @param {string} [label] - Timer label
 * @returns {Object} Timer object with start/end methods
 */
export const createTimer = (label = "Timer") => {
  let startTime = null;
  
  return {
    start: () => {
      startTime = performance.now();
      console.log(`${label} started`);
    },
    end: () => {
      if (startTime === null) {
        console.warn(`${label} was not started`);
        return;
      }
      const endTime = performance.now();
      const duration = endTime - startTime;
      console.log(`${label}: ${duration.toFixed(2)}ms`);
      startTime = null;
      return duration;
    }
  };
};

/**
 * Log memory usage
 * @returns {void}
 */
export const logMemoryUsage = () => {
  const usage = process.memoryUsage();
  console.log("=== Memory Usage ===");
  console.log(`RSS: ${(usage.rss / 1024 / 1024).toFixed(2)} MB`);
  console.log(`Heap Used: ${(usage.heapUsed / 1024 / 1024).toFixed(2)} MB`);
  console.log(`Heap Total: ${(usage.heapTotal / 1024 / 1024).toFixed(2)} MB`);
  console.log(`External: ${(usage.external / 1024 / 1024).toFixed(2)} MB`);
  console.log("===================");
};

/**
 * Create a debug logger with levels
 * @param {string} [name] - Logger name
 * @returns {Object} Logger object
 */
export const createDebugLogger = (name = "DEBUG") => {
  const levels = {
    ERROR: 0,
    WARN: 1,
    INFO: 2,
    DEBUG: 3,
    TRACE: 4
  };
  
  let currentLevel = levels.INFO;
  
  const log = (level, message, ...args) => {
    if (levels[level] <= currentLevel) {
      const timestamp = new Date().toISOString();
      console.log(`[${timestamp}] [${name}] [${level}] ${message}`, ...args);
    }
  };
  
  return {
    setLevel: (level) => {
      if (levels[level] !== undefined) {
        currentLevel = levels[level];
      }
    },
    error: (message, ...args) => log("ERROR", message, ...args),
    warn: (message, ...args) => log("WARN", message, ...args),
    info: (message, ...args) => log("INFO", message, ...args),
    debug: (message, ...args) => log("DEBUG", message, ...args),
    trace: (message, ...args) => log("TRACE", message, ...args)
  };
};

/**
 * Pretty print JSON with syntax highlighting
 * @param {any} obj - Object to pretty print
 * @param {number} [indent=2] - Indentation level
 * @returns {string} Pretty printed JSON
 */
export const prettyJSON = (obj, indent = 2) => {
  return JSON.stringify(obj, null, indent);
};

/**
 * Log JSON with pretty printing
 * @param {any} obj - Object to log
 * @param {string} [label] - Optional label
 * @returns {void}
 */
export const logJSON = (obj, label = "JSON") => {
  console.log(`=== ${label} ===`);
  console.log(prettyJSON(obj));
  console.log("================");
};

/**
 * Create a progress tracker
 * @param {number} total - Total number of items
 * @param {string} [label] - Progress label
 * @returns {Object} Progress tracker
 */
export const createProgressTracker = (total, label = "Progress") => {
  let current = 0;
  
  return {
    update: (increment = 1) => {
      current += increment;
      const percentage = ((current / total) * 100).toFixed(1);
      const bar = "█".repeat(Math.floor(percentage / 2)) + "░".repeat(50 - Math.floor(percentage / 2));
      process.stdout.write(`\r${label}: [${bar}] ${percentage}% (${current}/${total})`);
      
      if (current >= total) {
        console.log(); // New line when complete
      }
    },
    complete: () => {
      current = total;
      const bar = "█".repeat(50);
      console.log(`\r${label}: [${bar}] 100.0% (${total}/${total})`);
    }
  };
};

/**
 * Measure quad processing performance
 * @param {import('n3').Store} store - Store to measure
 * @param {Function} processor - Function to process quads
 * @returns {Promise<Object>} Performance metrics
 */
export const measureQuadProcessing = async (store, processor) => {
  const start = performance.now();
  const startMemory = process.memoryUsage();
  
  const result = await processor(store);
  
  const end = performance.now();
  const endMemory = process.memoryUsage();
  
  return {
    duration: end - start,
    quadCount: store.size,
    quadsPerSecond: (store.size / (end - start)) * 1000,
    memoryDelta: {
      rss: endMemory.rss - startMemory.rss,
      heapUsed: endMemory.heapUsed - startMemory.heapUsed,
      heapTotal: endMemory.heapTotal - startMemory.heapTotal
    },
    result
  };
};
