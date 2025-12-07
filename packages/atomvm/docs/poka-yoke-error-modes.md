# Poka-Yoke Error Modes Inventory

## Error Modes for AtomVM Node.js and Browser Runtimes

### Invalid State Errors

- [x] **Runtime not initialized before use**
  - Error: Calling `executeBeam()` or `runExample()` before `loadWASM()`
  - Current: Runtime check throws error
  - Prevention: State machine prevents invalid operations

- [x] **Module name missing**
  - Error: `moduleName` is null/undefined/empty
  - Current: Constructor throws error
  - Prevention: Validation at construction time

- [x] **AtomVM path not set**
  - Error: Calling `execute()` before `load()` in Node runtime
  - Current: Runtime check throws error
  - Prevention: State machine prevents invalid operations

- [x] **Runtime in wrong state**
  - Error: Calling operations when runtime is destroyed/error state
  - Current: No state tracking
  - Prevention: State machine with explicit states

### Invalid Input Errors

- [x] **Empty/null moduleName**
  - Error: `moduleName` is empty string or null
  - Current: Constructor throws error
  - Prevention: NonEmptyString validation type

- [x] **Empty/null avmPath**
  - Error: `avmPath` is empty string or null
  - Current: Runtime check throws error
  - Prevention: NonEmptyString validation type

- [x] **Invalid file path**
  - Error: File path doesn't exist or not accessible
  - Current: Runtime check throws error
  - Prevention: File existence validation before use

### Invalid Operation Errors

- [x] **Execute before load**
  - Error: Calling `executeBeam()` before `loadWASM()`
  - Current: Runtime check throws error
  - Prevention: State machine prevents operation in Uninitialized state

- [x] **Load after destroy**
  - Error: Calling `loadWASM()` after `destroy()`
  - Current: No state tracking
  - Prevention: State machine prevents operation in Destroyed state

- [x] **Multiple loads**
  - Error: Calling `loadWASM()` multiple times
  - Current: May cause issues
  - Prevention: State machine prevents operation in Ready state

### Resource Errors

- [x] **AtomVM file not found (Node)**
  - Error: `AtomVM-node-v0.6.6.js` not found in public directory
  - Current: Runtime check throws error
  - Prevention: File existence validation at load time

- [x] **AtomVM script not found (Browser)**
  - Error: `AtomVM-web-v0.6.6.js` not found
  - Current: Network error
  - Prevention: Script existence check before loading

- [x] **.avm file not found**
  - Error: `.avm` file doesn't exist
  - Current: HTTP 404 or file not found error
  - Prevention: File existence validation before execution

- [x] **Node command not found**
  - Error: `node` command not in PATH
  - Current: Runtime check throws error
  - Prevention: Command existence check before spawn

- [x] **SharedArrayBuffer not available**
  - Error: COI not enabled, SharedArrayBuffer undefined
  - Current: Runtime check throws error
  - Prevention: Environment check before initialization

### Logic Errors

- [x] **Timeout during execution**
  - Error: Execution doesn't complete within timeout
  - Current: Timeout error thrown
  - Prevention: Proper timeout handling with state cleanup

- [x] **Module initialization timeout**
  - Error: AtomVM Module.ready never becomes true
  - Current: Timeout error thrown
  - Prevention: Proper timeout handling with state cleanup

- [x] **State inconsistency**
  - Error: `isLoaded` true but `atomvmModule` is null
  - Current: No validation
  - Prevention: State machine ensures consistency

## Prevention Strategy

### Type-Level Prevention

1. **State Machine Pattern**: Use explicit states (Uninitialized, Loading, Ready, Executing, Error, Destroyed)
2. **Validation Types**: NonEmptyString for moduleName and avmPath
3. **Type Guards**: Functions to check state before operations
4. **Compile-Time Checks**: JSDoc types and validation functions

### Runtime Prevention

1. **Constructor Validation**: Validate all required inputs at construction
2. **State Checks**: Check state before all operations
3. **Resource Validation**: Validate file/script existence before use
4. **Environment Checks**: Check browser/Node.js environment before operations

