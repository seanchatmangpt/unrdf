# @unrdf/streaming Diataxis Content Roadmap

**Package:** @unrdf/streaming (Process large RDF graphs)
**Type:** Feature Extension (builds on @unrdf/core)
**Phase:** 2 (Week 3, Team A)
**Expected effort:** 60-76 hours
**Estimated words:** 18,000-22,000

---

## Package Overview

**Purpose:** Process large RDF graphs (millions/billions of triples) without loading entire dataset into memory

**Key concepts:**
- Streaming parser (event-driven)
- Backpressure (slow consumer)
- Buffering and windowing
- Memory-efficient processing
- Real-time data ingestion

**Audience:** Developers with large-scale RDF data
- Know: SPARQL, @unrdf/core basics
- Need: Memory-efficient processing
- Problem: "I have 1B triples, 16GB RAM"

---

## Tutorials (3 files, 16-20 hours)

### Tutorial 1: Processing Large Files (6-8 hours)

**Title:** "Processing Large RDF Files Without Memory Bloat"

**Goal:** "Load and process 1 million triple file using <500MB memory"

**Learning outcomes:**
- Understand streaming vs full-load approach
- Use streaming parser API
- Process quads as they arrive
- Measure memory usage

**Content structure:**

1. **Problem Statement** (200 words)
   - "You have a 1GB RDF file"
   - "Memory is 16GB but you want to use < 1GB"
   - "Need to process each triple, not just load and query"

2. **Solution Overview** (200 words)
   - Streaming parser loads one triple at a time
   - You process each triple immediately
   - Never load entire file

3. **Part 1: Compare Approaches** (400 words, with code)
   - **Traditional:** `core.parseRdf(hugeFile)` → crashes
   - **Streaming:** `for await (const quad of stream)` → efficient
   - Memory comparison table
   - Performance comparison (time vs memory)

4. **Part 2: Set Up Streaming** (500 words, with code)
   - Initialize streaming parser
   - Open file stream
   - Set up event handlers
   - Start consuming quads

5. **Part 3: Process Incrementally** (500 words, with code)
   - Process each triple
   - Update statistics
   - Add to database in batches
   - Real example: Word count in RDF labels

6. **Part 4: Measure Success** (300 words, with code)
   - How to measure memory
   - Performance metrics
   - Before/after comparison
   - Expected results: 500MB for 1B triples

7. **Part 5: Error Handling** (300 words, with code)
   - Parser errors on malformed data
   - Partial file handling
   - Recovery strategies

8. **Verification & Next Steps** (200 words)
   - How to test
   - Expected output
   - Link to Tutorial 2

**Code examples needed:**
- Full streaming setup
- Processing loop
- Batch insertion pattern
- Memory measurement code
- Error handling pattern

**Key insight:** This tutorial answers "I have a real memory problem" not "how does streaming work conceptually"

---

### Tutorial 2: Backpressure Handling (6-8 hours)

**Title:** "Handling Backpressure in RDF Streams"

**Goal:** "Understand and implement backpressure for producer/consumer mismatch"

**Learning outcomes:**
- What is backpressure (conceptual)
- Why it matters (prevents memory bloat)
- How to implement in UNRDF
- Real-world scenario

**Content structure:**

1. **Problem: Producer is Faster** (300 words)
   - Parser produces triples quickly
   - Your processor is slow
   - Triples buffer in memory
   - Eventually runs out of memory

2. **Solution: Backpressure** (300 words)
   - Tell producer to wait
   - Consumer sets pace
   - Memory stays bounded

3. **Part 1: Understand Backpressure** (400 words)
   - Real-world analogy: water in pipe
   - Pipe diameter = buffer size
   - Flow rate = processing speed
   - Backpressure = throttle valve

4. **Part 2: Implement Backpressure** (600 words, with code)
   - Async iteration (for await)
   - Promise-based flow control
   - Buffer size monitoring
   - Pause/resume pattern

5. **Part 3: Real Scenario** (500 words, with code)
   - Slow database insert
   - Fast RDF parsing
   - Backpressure keeps memory low
   - Code example: Insert to database during parse

6. **Verification & Next Steps** (200 words)

**Code examples:**
- Without backpressure (breaks)
- With backpressure (works)
- Monitoring buffer size
- Database insertion pattern

---

### Tutorial 3: Real-Time Data Sync (4-6 hours)

**Title:** "Real-Time RDF Data Synchronization"

**Goal:** "Stream new RDF data into store while keeping application updated"

**Learning outcomes:**
- Use streaming for live data
- Integrate with knowledge hooks
- Keep UI/application updated
- Production patterns

**Content structure:**

1. **Use Case** (200 words)
   - New RDF data arriving continuously
   - Must stay synchronized
   - Keep application responsive

2. **Architecture** (300 words)
   - Source: RDF file, SPARQL endpoint, message queue
   - Stream: Parse incrementally
   - Store: Update in real-time
   - Application: Gets notified

3. **Implementation** (600 words, with code)
   - Open stream from source
   - Add each quad to store
   - Knowledge hooks notify app
   - Application re-renders

4. **Example: Stock Updates** (500 words, with code)
   - New stock quotes arrive
   - Stream into RDF store
   - UI shows latest prices
   - Full working example

5. **Verification & Next Steps** (200 words)

**Code examples:**
- Connect to data source
- Stream parsing
- Real-time insertion
- Hook notification

---

## How-To Guides (4 files, 16-20 hours)

### How-To 1: Optimize Memory Usage (4-6 hours)

**Problem:** "My streaming is still using too much memory. How do I reduce it?"

**Sections:**

1. **Diagnosis** (200 words)
   - Measure current memory
   - Identify bottleneck
   - Is it buffer? Storage? Indexes?

2. **Solution 1: Reduce Buffer** (300 words)
   - Decrease internal buffer size
   - Trade off: higher overhead, lower memory
   - Code example

3. **Solution 2: Filter Early** (300 words)
   - Don't add unnecessary triples
   - Filter before storage
   - Pattern: selective insertion
   - Code example

4. **Solution 3: Batch to Disk** (400 words)
   - Instead of in-memory store
   - Batch to database
   - Trade off: slower reads, lower memory
   - Code example with Oxigraph

5. **Solution 4: Windowing** (300 words)
   - Keep only recent triples
   - Drop old data
   - Sliding window pattern
   - Use case: time-series data

6. **Performance Comparison Table** (100 words)

---

### How-To 2: Handle Parser Errors (4-6 hours)

**Problem:** "My RDF file has syntax errors. How do I recover?"

**Sections:**

1. **Error Types** (200 words)
   - Syntax error in triple
   - Malformed URI
   - Invalid literal
   - Encoding problem

2. **Skip & Continue** (400 words, with code)
   - Catch parser error
   - Log and skip
   - Continue processing
   - Count errors for reporting

3. **Partial File Handling** (400 words, with code)
   - File got corrupted midway
   - Process what's valid
   - Report error location
   - Example: Recover from 99% valid file

4. **Validation & Recovery** (300 words, with code)
   - Pre-validate file format
   - Use lenient parser options
   - Post-validate loaded data

5. **Debugging** (300 words)
   - Enable debug logging
   - Line number + error context
   - Common mistakes checklist

---

### How-To 3: Batch Processing Large Datasets (4-6 hours)

**Problem:** "I need to process 100 RDF files. How do I do it efficiently?"

**Sections:**

1. **Batch Strategy** (300 words)
   - Sequential vs parallel
   - Memory constraints
   - When to batch

2. **Pattern 1: Sequential** (400 words, with code)
   - Process file 1, then file 2
   - Lower memory
   - Slower overall

3. **Pattern 2: Parallel with Limit** (500 words, with code)
   - Process up to N files concurrently
   - Bounded memory growth
   - Code: Promise.all with limit

4. **Pattern 3: Worker Threads** (400 words, with code)
   - Off-thread processing
   - Shared storage
   - Performance improvement

5. **Monitoring & Reporting** (300 words, with code)
   - Progress tracking
   - Error reporting
   - Summary statistics

---

### How-To 4: Monitor Streaming Performance (4-6 hours)

**Problem:** "How fast is my streaming? Where are the bottlenecks?"

**Sections:**

1. **Metrics to Measure** (200 words)
   - Triples per second
   - Memory usage
   - CPU usage
   - Backpressure events

2. **Simple Monitoring** (400 words, with code)
   - Count triples
   - Timer for duration
   - Calculate throughput
   - Memory snapshots

3. **Detailed Profiling** (500 words, with code)
   - Parse time
   - Processing time
   - Storage time
   - Identify bottleneck

4. **Performance Baseline** (300 words, with table)
   - Expected throughput by size
   - Memory growth pattern
   - Common bottlenecks

5. **Optimization Based on Metrics** (300 words)
   - If parsing is slow: upgrade parser
   - If processing is slow: optimize code
   - If storage is slow: use database

---

## Reference (5 files, 12-16 hours)

### API.md (3-4 hours)

**Streaming Parser API**

```javascript
function streamRdf(
  source: Stream | File | URL,
  format?: 'turtle' | 'jsonld' | 'ntriples',
  options?: StreamOptions
): AsyncIterable<Quad>
```

**Parameters:**
- source: File stream, ReadableStream, or URL
- format: Auto-detected if omitted
- options:
  - bufferSize: number (default 1000)
  - timeout: number (default 30000)
  - strict: boolean (default false)

**Returns:** AsyncIterable of RDF quads

**Examples:**
- Stream from file
- Stream from URL
- Stream from pipe
- Custom format handler

**Streaming Writer API**

```javascript
function createStreamWriter(
  options?: WriterOptions
): WriteStream
```

**Other functions:**
- setBufferSize(size: number)
- pauseStream(stream)
- resumeStream(stream)
- onBackpressure(handler)

---

### Types.md (2-3 hours)

**Quad interface** (same as core, reference only)

**Stream options**
```typescript
interface StreamOptions {
  bufferSize: number;
  timeout: number;
  strict: boolean;
  errorHandler?: (error) => void;
}
```

**Progress events**
```typescript
interface StreamProgress {
  quadsProcessed: number;
  bytesRead: number;
  memoryUsage: number;
}
```

---

### Configuration.md (2-3 hours)

**Global defaults**

```javascript
setDefaultStreamOptions({
  bufferSize: 1000,
  timeout: 30000,
  strict: false
});
```

**Per-stream config:**
- Buffer management
- Error handling
- Progress reporting
- Performance tuning

---

### Errors.md (2-3 hours)

**Error types**

| Error | When | Solution |
|-------|------|----------|
| ParseError | Malformed RDF | Check syntax, use lenient mode |
| BackpressureError | Buffer exceeded | Slow down processing |
| TimeoutError | Stream timeout | Increase timeout or check source |
| EncodingError | Invalid encoding | Specify charset |

---

### Performance.md (2-3 hours)

**Streaming vs full parse**
- Memory comparison
- Speed comparison
- When to use each

**Throughput targets**
- 10K triples/sec (typical)
- 100K triples/sec (optimized)
- Factors affecting speed

**Memory characteristics**
- Linear with buffer size
- Not with data size
- Graphs

---

## Explanation (4 files, 12-16 hours)

### architecture.md (3-4 hours)

**Architecture diagram**
- Parser: event-driven, async
- Handler: processes quads
- Storage: optional (user-managed)
- Backpressure: flow control

**Components:**
- StreamParser: Turtle/JSON-LD/N-Triples
- QuadHandler: Your code
- BackpressureController: Pace setting
- ProgressReporter: Statistics

**Data flow:**
1. File → Parser
2. Parser → Quad events
3. Quads → Handler
4. Handler → Storage (or skip)

---

### design-decisions.md (3-4 hours)

**Why async iteration?**
- Native JavaScript streaming
- Easy to pause/resume
- Backpressure built-in

**Why buffer?**
- Performance (batch processing)
- Backpressure (prevent overflow)
- Trade-off: configurable

**Why not full parsing first?**
- Memory efficiency (key benefit)
- Real-time processing possible
- Trade-off: can't random access

---

### concepts.md (2-3 hours)

**Streaming (vs batch)**
- Continuous processing
- Event-driven
- Memory bounded
- Real-time capable

**Backpressure (why it's important)**
- Prevents memory bloat
- Keeps pace with source
- Handles slow processors
- Essential for large data

**Buffering (why it exists)**
- Performance (batch ops faster)
- Backpressure (pause/resume)
- Trade-off: memory vs speed

---

### performance.md (3-4 hours)

**Throughput characteristics**
- Parsing: format dependent
- Processing: application dependent
- Storage: backend dependent

**Memory behavior**
- Linear with buffer size
- Not with total data size
- Example graphs

**Optimization opportunities**
- Increase buffer (faster, more memory)
- Batch operations (faster)
- Use indexes (faster reads)

---

## Summary

**Total effort:** 60-76 hours
**Total files:** 16
**Total words:** 18,000-22,000

**Breakdown:**
- Tutorials: 3 (16-20 hours)
- How-To: 4 (16-20 hours)
- Reference: 5 (12-16 hours)
- Explanation: 4 (12-16 hours)

**Quality metrics:**
- Code examples: 25+
- Real-world scenarios: 5+
- Performance comparisons: 3+
- Diagrams: 2+

**Success criteria:**
- [ ] All code examples tested
- [ ] No TODO/FIXME
- [ ] Peer review: passed
- [ ] Validation score: 100%
- [ ] Link check: all valid
