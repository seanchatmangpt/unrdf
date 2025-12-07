# @unrdf/atomvm

Run AtomVM (Erlang/BEAM VM) in the browser and Node.js using WebAssembly.

## Quick Start

### Browser

1. **Build an Erlang module:**
   ```bash
   pnpm run build:erlang mymodule
   ```

2. **Start dev server:**
   ```bash
   pnpm dev
   ```

3. **Open browser with module name:**
   ```
   http://localhost:3000?module=mymodule
   ```

4. **Click "Initialize AtomVM" then "Run Example"**

The page may auto-reload once to enable Cross-Origin-Isolation (required for SharedArrayBuffer).

### Node.js

1. **Build an Erlang module:**
   ```bash
   pnpm run build:erlang mymodule
   ```

2. **Execute the .avm file:**
   ```bash
   node src/cli.mjs public/mymodule.avm
   ```

## Features

- **State Machine Design**: Poka-yoke error prevention - invalid operations are impossible
- **Real AtomVM WASM**: Uses actual AtomVM v0.6.6 compiled to WebAssembly
- **Dual Runtime**: Works in both browser and Node.js environments
- **Cross-Origin-Isolation**: Automatic COI setup via service workers
- **Module-Based**: Explicit module naming (no defaults)
- **SLA Tracking**: Strict SLA for JS→Erlang→JS roundtrips (<10ms latency, <0.1% error rate)
- **Poka-Yoke SLA**: Prevents operations that would violate SLA thresholds

## Installation

```bash
pnpm install
```

## Usage

### Browser Runtime

The browser runtime requires a module name in the URL:

```javascript
// URL: ?module=mymodule
// Code automatically:
// 1. Registers service worker for COI
// 2. Creates AtomVMRuntime with module name
// 3. Enables UI controls
```

**API:**
```javascript
import { AtomVMRuntime } from '@unrdf/atomvm';

const runtime = new AtomVMRuntime(terminal, 'mymodule');
await runtime.loadWASM();
await runtime.executeBeam('/mymodule.avm');
```

### Node.js Runtime

```javascript
import { AtomVMNodeRuntime } from '@unrdf/atomvm/src/node-runtime.mjs';

const runtime = new AtomVMNodeRuntime();
await runtime.load();
await runtime.execute('/path/to/file.avm');
```

### CLI

```bash
# Execute .avm file
node src/cli.mjs public/mymodule.avm
```

### Build Scripts

```bash
# Build Erlang module to .avm
pnpm run build:erlang mymodule

# Complete workflow (build + instructions)
pnpm run build:erlang:workflow mymodule

# Clean build artifacts
pnpm run build:erlang:clean
```

## Browser Compatibility

- **Chrome/Edge**: 92+ ✅
- **Firefox**: 95+ ✅
- **Safari**: 15.2+ ✅

Requires service worker support and Cross-Origin-Isolation (automatic via coi-serviceworker).

## SLA Requirements

**Strict SLA for JS→Erlang→JS Roundtrips**:
- **Latency**: <10ms per roundtrip (end-to-end)
- **Error Rate**: <0.1% (1 error per 1000 roundtrips)

**Poka-Yoke Enforcement**:
- Operations rejected if error rate would exceed 0.1%
- Latency warnings logged if >10ms (but allowed - may be transient)
- SLA metrics tracked in OTEL spans

See [SLA Roundtrip Documentation](../../docs/SLA-ROUNDTRIP.md) for details.

## Documentation

Complete documentation is organized using the [Diataxis](https://diataxis.fr/) framework:

- **[Tutorials](./docs/tutorials/)** - Learn how to use AtomVM in the browser
- **[How-To Guides](./docs/how-to/)** - Solve specific problems
- **[Reference](./docs/reference/)** - Complete API documentation
- **[Explanations](./docs/explanation/)** - Understand the design and architecture

Start with: [Getting Started Tutorial](./docs/tutorials/01-getting-started.md)

## Development

```bash
# Run tests
pnpm test

# Run tests in watch mode
pnpm test:watch

# Run browser integration tests
pnpm test:browser

# Run Playwright E2E tests
pnpm test:playwright

# Build for production
pnpm build

# Preview production build
pnpm preview
```

## Architecture

- **Browser**: Service worker enables COI → SharedArrayBuffer → AtomVM WASM execution
- **Node.js**: Spawns Node.js process with AtomVM-node-v0.6.6.js
- **State Machine**: Prevents invalid operations (poka-yoke design)
- **Module-Based**: Explicit module naming required (no defaults)

See [Architecture Explanation](./docs/explanation/architecture.md) for details.

## Requirements

- **Browser**: Module name in URL (`?module=<name>`)
- **Node.js**: Node.js 18+, Erlang toolchain for building modules
- **Build Tools**: `erlc` and `packbeam` in PATH

## License

MIT
