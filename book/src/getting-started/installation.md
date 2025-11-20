# Installation

This guide covers installing UNRDF for both development and production environments.

## Requirements

Before installing UNRDF, ensure you have:

- **Node.js** ≥ 18.0.0 (LTS recommended)
- **pnpm** ≥ 8.0.0 (recommended) or npm ≥ 9.0.0

```admonish info
UNRDF uses ES modules and requires Node.js 18+ for native ESM support and performance features.
```

### Check Your Environment

```bash
# Check Node.js version
node --version
# Should print: v18.x.x or higher

# Check pnpm version (if using pnpm)
pnpm --version
# Should print: 8.x.x or higher
```

## Installing UNRDF

### Using pnpm (Recommended)

```bash
pnpm add unrdf
```

### Using npm

```bash
npm install unrdf
```

### Using yarn

```bash
yarn add unrdf
```

```admonish success title="Why pnpm?"
UNRDF recommends pnpm for:
- **Faster installs** - Shared dependency store
- **Less disk space** - Content-addressed storage
- **Stricter** - Better dependency resolution
```

## Verify Installation

Create a test file `test-install.mjs`:

```javascript
import { createDarkMatterCore } from 'unrdf';

console.log('✅ UNRDF imported successfully');

const system = await createDarkMatterCore();
console.log('✅ Dark Matter core created');

await system.cleanup();
console.log('✅ Cleanup successful');
console.log('\n🎉 UNRDF is installed and working!');
```

Run it:

```bash
node test-install.mjs
```

**Expected output:**

```
✅ UNRDF imported successfully
🌌 Initializing Dark Matter 80/20 Core...
🔧 Initializing transactionManager (25% value weight)...
✅ transactionManager initialized (contributes 25% of system value)
🔧 Initializing knowledgeHookManager (20% value weight)...
✅ knowledgeHookManager initialized (contributes 20% of system value)
... [more initialization logs]
✅ Dark Matter 80/20 Core initialized successfully
✅ Dark Matter core created
🧹 Cleaning up Dark Matter 80/20 Core...
✅ Cleanup successful

🎉 UNRDF is installed and working!
```

## Development vs Production Setup

### Development Setup

For local development, install UNRDF with all dev dependencies:

```bash
# Clone the repository
git clone https://github.com/unrdf/unrdf.git
cd unrdf

# Install dependencies
pnpm install

# Run tests to verify
pnpm test

# Run OTEL validation
node validation/run-all.mjs comprehensive
```

This gives you:
- Source code for debugging
- Test suites for learning
- Validation tools
- Development scripts

### Production Setup

For production applications, install only the runtime package:

```bash
pnpm add unrdf --save-prod
```

**Production configuration example:**

```javascript
import { createDarkMatterCore } from 'unrdf';

const system = await createDarkMatterCore({
  // Enable all core components
  enableTransactionManager: true,
  enableKnowledgeHookManager: true,
  enableEffectSandbox: true,
  enableObservability: true,
  enablePerformanceOptimizer: true,
  enableLockchainWriter: true,

  // Production performance targets
  performanceTargets: {
    p50PreHookPipeline: 0.2,    // 200µs
    p99PreHookPipeline: 2,      // 2ms
    receiptWriteMedian: 5,      // 5ms
    hookEngineExecPerMin: 10000 // 10k/min
  },

  // Optimization flags
  enableFastPath: true,
  enableCaching: true,
  enableBatchProcessing: true,

  // Resource limits
  maxConcurrency: 10,
  cacheSize: 10000,
  batchSize: 1000,
  timeoutMs: 2000
});
```

```admonish warning
In production, always configure timeouts, cache sizes, and concurrency limits based on your workload and infrastructure.
```

## Optional Dependencies

UNRDF has optional features that require additional dependencies:

### Cryptographic Lockchain

For cryptographic audit trails with Merkle verification:

```bash
pnpm add isomorphic-git simple-git
```

```javascript
import { LockchainWriter } from 'unrdf';

const lockchain = new LockchainWriter({
  repoPath: './audit-trail',
  enableMerkle: true
});

await lockchain.init();
```

### OpenTelemetry Exporters

For production observability, install OTEL exporters:

```bash
# Jaeger exporter
pnpm add @opentelemetry/exporter-jaeger

# Prometheus exporter
pnpm add @opentelemetry/exporter-prometheus

# OTLP exporter
pnpm add @opentelemetry/exporter-trace-otlp-http
```

```javascript
import { NodeTracerProvider } from '@opentelemetry/sdk-trace-node';
import { JaegerExporter } from '@opentelemetry/exporter-jaeger';

const provider = new NodeTracerProvider();
provider.addSpanProcessor(
  new SimpleSpanProcessor(new JaegerExporter())
);
provider.register();
```

## Platform-Specific Notes

### Node.js on macOS

UNRDF works out of the box on macOS with Node.js 18+:

```bash
# Install Node.js via Homebrew
brew install node@18

# Or use nvm
nvm install 18
nvm use 18

# Install UNRDF
pnpm add unrdf
```

### Node.js on Linux

For Linux, ensure you have the latest Node.js:

```bash
# Ubuntu/Debian
curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt-get install -y nodejs

# Or use nvm
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
nvm install 18

# Install UNRDF
pnpm add unrdf
```

### Node.js on Windows

```bash
# Using nvm-windows
nvm install 18.0.0
nvm use 18.0.0

# Install UNRDF
pnpm add unrdf
```

```admonish warning title="Windows Path Issues"
On Windows, if you encounter Git path errors with lockchain, ensure Git is in your PATH:
```

```powershell
# Add Git to PATH (PowerShell as Admin)
$env:Path += ";C:\Program Files\Git\cmd"
```

## Docker Setup

For containerized deployments:

```dockerfile
FROM node:18-alpine

WORKDIR /app

# Install pnpm
RUN npm install -g pnpm

# Copy package files
COPY package.json pnpm-lock.yaml ./

# Install dependencies
RUN pnpm install --frozen-lockfile

# Copy application code
COPY . .

# Run application
CMD ["node", "index.mjs"]
```

**Build and run:**

```bash
docker build -t my-unrdf-app .
docker run -p 3000:3000 my-unrdf-app
```

## Troubleshooting

### Module Resolution Errors

```admonish danger title="Error"
Error [ERR_MODULE_NOT_FOUND]: Cannot find package '@rdfjs/data-model'
```

**Solution:** Ensure all peer dependencies are installed:

```bash
pnpm add @rdfjs/data-model n3 zod
```

### Version Conflicts

```admonish danger title="Error"
npm ERR! ERESOLVE unable to resolve dependency tree
```

**Solution:** Use pnpm or install with legacy peer deps:

```bash
npm install unrdf --legacy-peer-deps
```

### Git Not Found (Lockchain)

```admonish danger title="Error"
Error: Command failed: git init
```

**Solution:** Install Git and ensure it's in your PATH:

```bash
# macOS
brew install git

# Ubuntu/Debian
sudo apt-get install git

# Windows
# Download from https://git-scm.com/download/win
```

### Permission Errors

```admonish danger title="Error"
Error: EACCES: permission denied
```

**Solution:** Don't use sudo with npm/pnpm. Fix permissions:

```bash
# macOS/Linux
sudo chown -R $USER:$USER ~/.npm
sudo chown -R $USER:$USER ~/.pnpm-store
```

## Next Steps

Now that UNRDF is installed, continue to:

1. **[Basic Usage](basic-usage.md)** - Learn core RDF operations
2. **[First Hook](first-hook.md)** - Create your first Knowledge Hook
3. **API Reference** - Explore the complete API

```admonish tip
For production deployments, see our [Production Deployment Guide](../guides/production-deployment.md) for best practices on scaling, monitoring, and security.
```

## Getting Help

If you encounter issues:

1. **Check the [FAQ](../troubleshooting/faq.md)** - Common questions and solutions
2. **Search [GitHub Issues](https://github.com/unrdf/unrdf/issues)** - Known issues and workarounds
3. **Ask on [GitHub Discussions](https://github.com/unrdf/unrdf/discussions)** - Community support
4. **Report bugs** - [Create an issue](https://github.com/unrdf/unrdf/issues/new)

---

**Installation complete!** Head to [Basic Usage](basic-usage.md) to start building with UNRDF.
