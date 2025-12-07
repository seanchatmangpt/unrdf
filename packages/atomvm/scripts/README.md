# AtomVM Build Scripts

Scripts for building Erlang modules and packaging them into `.avm` files for AtomVM execution.

## Prerequisites

1. **Erlang/OTP** - For compiling `.erl` files to `.beam` bytecode
   ```bash
   # macOS
   brew install erlang
   
   # Ubuntu/Debian
   sudo apt-get install erlang
   ```

2. **AtomVM packbeam** - For packaging `.beam` files into `.avm` files
   - Build from [AtomVM source](https://github.com/atomvm/AtomVM)
   - Or download pre-built binary and add to PATH

## Usage

### Build Default Module (hello)

```bash
pnpm run build:erlang
```

This will:
1. Create `src/erlang/hello.erl` if it doesn't exist
2. Compile it to `src/erlang/hello.beam` using `erlc`
3. Package it to `public/hello.avm` using `packbeam`

### Build Custom Module

```bash
pnpm run build:erlang:module mymodule
```

Or directly:
```bash
node scripts/index.mjs build mymodule
```

### Clean Build Artifacts

```bash
pnpm run build:erlang:clean
```

This removes all `.beam` files from `src/erlang/` directory.

## Scripts

- `scripts/index.mjs` - Main entry point, routes commands
- `scripts/build.mjs` - Build logic (create .erl, compile to .beam, package to .avm)
- `scripts/clean.mjs` - Clean build artifacts

## Generated Files

- **Source**: `src/erlang/<module>.erl` - Erlang source code
- **Bytecode**: `src/erlang/<module>.beam` - Compiled BEAM bytecode
- **Package**: `public/<module>.avm` - AtomVM package (ready for execution)

## Example Workflow

```bash
# 1. Build the hello module
pnpm run build:erlang

# 2. Test in browser
pnpm run dev
# Then click "Run Example" in the browser

# 3. Or test with Node.js
pnpm run atomvm:node public/hello.avm

# 4. Clean up when done
pnpm run build:erlang:clean
```

## Troubleshooting

### `erlc: command not found`
- Install Erlang/OTP (see Prerequisites)
- Ensure `erlc` is in your PATH

### `packbeam: command not found`
- Build AtomVM from source to get `packbeam` tool
- Or download pre-built `packbeam` binary
- Add `packbeam` to your PATH

### Build succeeds but .avm file not created
- Check that `packbeam` executed successfully
- Verify `public/` directory exists and is writable
- Check file permissions

