 # Enterprise Demo: unrdf + kgen Fusion
 
 This repository contains a full working demo of the **unrdf** substrate and **kgen** artifact generator, fully integrated with:
 - ESM modules (.mjs) and JSDoc documentation
 - London-style BDD tests (citty-test-utils)
 - OpenTelemetry tracing (+ in-memory tests)
 - Performance benchmarks (10000-triple parse)
 - Testcontainers integration for clean-room validation
 - Slidev presentation deck
 
 ## Prerequisites
 - Node.js >= 18
 - Docker (for Testcontainers integration)
 - pnpm installed globally
 
 ## Quick Start
 1. Install dependencies and run all tests:
    ```bash
    ./run-all.sh
    ```
 2. Run the enterprise demo:
    ```bash
    pnpm start:demo
    # view generated output: examples/demo/out.md
    ```
 3. Preview the Slidev deck:
    ```bash
    npx slidev slides/presentation.md --open
    ```
 
 ## Directory Layout
 ```
 enterprise-demo/
 ├── packages/unrdf/       # core RDF composables
 ├── packages/kgen/        # CLI and generator
 ├── telemetry/            # OpenTelemetry setup & helpers
 ├── examples/demo/        # sample graph, template, and demo script
 ├── slides/               # Slidev presentation
 ├── run-all.sh            # install & test script
 ├── README.md             # this guide
 ├── package.json          # workspace scripts
 └── pnpm-workspaces.yaml  # pnpm workspace definition
 ```
 
 Feel free to explore, modify, and run the demo!