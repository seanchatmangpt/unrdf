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

### Browser Runtime
- **State Machine Design**: Poka-yoke error prevention - invalid operations are impossible
- **Real AtomVM WASM**: Uses actual AtomVM v0.6.6 compiled to WebAssembly
- **Dual Runtime**: Works in both browser and Node.js environments
- **Cross-Origin-Isolation**: Automatic COI setup via service workers
- **Module-Based**: Explicit module naming (no defaults)
- **SLA Tracking**: Strict SLA for JS→Erlang→JS roundtrips (<10ms latency, <0.1% error rate)
- **Poka-Yoke SLA**: Prevents operations that would violate SLA thresholds

### Distributed Macroframework (Production)
- **Docker Swarm Orchestration**: Overlay networking with automatic service discovery
- **Erlang Distribution**: Full BEAM clustering with EPMD (Erlang Port Mapper Daemon)
- **Circuit Breaker**: Telecom-grade failure protection (configurable thresholds)
- **Supervisor Trees**: OTP-style supervision for automatic process restart
- **Message Passing**: RPC-based distributed messaging across nodes
- **Chaos Tested**: 10 random container kills, 0 cascading failures, 100% recovery
- **100% Connectivity**: Verified with `net_adm:ping` across Docker Swarm overlay network

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

### Production Macroframework

**Quick Start:**
```bash
# Run complete production demo (Docker Swarm + Circuit Breaker + Supervisor)
node examples/production-messaging.mjs
```

**Manual Setup:**
```bash
# 1. Initialize Docker Swarm
docker swarm init

# 2. Deploy AtomVM cluster (3 replicas)
docker stack deploy -c experiments/docker-swarm-messaging/docker-stack-fixed.yml atomvm

# 3. Verify connectivity (returns 'pong')
CONT=$(docker ps --filter "name=atomvm" --format "{{.ID}}" | head -1)
docker exec $CONT sh -c "erl -noshell -sname test -setcookie atomvm_secret_cookie -eval \"net_adm:ping('atomvm_node2@atomvm-2'), init:stop().\""

# 4. Send message
docker exec $CONT sh -c "erl -noshell -sname sender -setcookie atomvm_secret_cookie -eval \"rpc:call('atomvm_node2@atomvm-2', msg_handler, send_msg, ['atomvm_node2@atomvm-2', 'Hello', node()]), init:stop().\""

# 5. Verify reception
CONT2=$(docker ps --filter "name=atomvm_atomvm-node.2" --format "{{.ID}}")
docker logs $CONT2 2>&1 | grep RECEIVED
# Output: [RECEIVED] From: 'sender@atomvm-1', Content: Hello
```

**API:**
```javascript
import { CircuitBreaker } from '@unrdf/atomvm/src/circuit-breaker.mjs';
import { SupervisorTree } from '@unrdf/atomvm/src/supervisor-tree.mjs';

// Circuit breaker protecting distributed calls
const breaker = new CircuitBreaker({
  failureThreshold: 3,  // Open circuit after 3 failures
  resetTimeout: 5000    // Try to close after 5 seconds
});

const result = await breaker.call(async () => {
  // Your distributed operation here
  return await sendMessageToNode('atomvm_node2@atomvm-2', 'Hello');
});

// Supervisor tree for automatic restart
const supervisor = new SupervisorTree('my_app', 'one_for_one');
supervisor.addChild('worker1', async () => {
  // Worker process
}, 'one_for_one');

await supervisor.start();
```

See [Complete Macroframework Documentation](./experiments/ATOMVM-MACROFRAMEWORK-COMPLETE.md) for details.

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
