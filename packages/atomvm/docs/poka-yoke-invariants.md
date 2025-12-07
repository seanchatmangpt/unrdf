# Poka-Yoke Type Invariants and State Machine Documentation

## Type Invariants

### AtomVMRuntime

#### Constructor Invariants

```javascript
/**
 * **Poka-Yoke Invariant**: moduleName must be non-empty string
 * 
 * Validation: validateNonEmptyString(moduleName, 'moduleName')
 * Prevents: Empty, null, or undefined moduleName
 */
constructor(terminal, moduleName)
```

**Invariant**: `moduleName` is always a non-empty string after construction.

#### State Machine Invariants

```javascript
/**
 * **Poka-Yoke Invariant**: State is always one of: Uninitialized, Loading, Ready, Executing, Error, Destroyed
 * 
 * Valid transitions:
 * - Uninitialized => Loading => Ready
 * - Uninitialized => Loading => Error
 * - Ready => Executing => Ready
 * - Ready => Executing => Error
 * - Any => Destroyed (terminal state)
 * 
 * Invalid transitions prevented:
 * - Cannot execute before load (Uninitialized => Executing)
 * - Cannot load after destroy (Destroyed => Loading)
 * - Cannot execute after destroy (Destroyed => Executing)
 */
state: RuntimeState
```

**Invariant**: State is always valid and transitions follow valid paths only.

#### Type Guard Invariants

```javascript
/**
 * **Poka-Yoke Invariant**: isReady() returns true only when:
 * - state === 'Ready'
 * - atomvmModule !== null
 * 
 * Prevents: Operations when runtime is not ready
 */
isReady(): boolean
```

**Invariant**: `isReady()` accurately reflects runtime readiness.

```javascript
/**
 * **Poka-Yoke Invariant**: isLoaded() returns true only when:
 * - state === 'Ready' OR state === 'Executing'
 * 
 * Prevents: State inconsistency
 */
isLoaded(): boolean
```

**Invariant**: `isLoaded()` accurately reflects load state.

#### Operation Invariants

```javascript
/**
 * **Poka-Yoke Invariant**: loadWASM() can only be called when:
 * - state !== 'Destroyed'
 * - state !== 'Loading' (prevents multiple loads)
 * 
 * Prevents: Multiple loads, loads after destroy
 */
async loadWASM(): Promise<void>
```

**Invariant**: `loadWASM()` only succeeds in valid states.

```javascript
/**
 * **Poka-Yoke Invariant**: executeBeam(avmPath) can only be called when:
 * - isReady() === true
 * - avmPath is non-empty string
 * 
 * Prevents: Execution before load, execution with invalid path
 */
async executeBeam(avmPath: string): Promise<any>
```

**Invariant**: `executeBeam()` only succeeds when ready and with valid input.

```javascript
/**
 * **Poka-Yoke Invariant**: destroy() sets state to 'Destroyed' (terminal state)
 * 
 * Prevents: All operations after destroy
 */
destroy(): void
```

**Invariant**: After `destroy()`, no operations are possible.

### AtomVMNodeRuntime

#### Constructor Invariants

```javascript
/**
 * **Poka-Yoke Invariant**: Options are optional, defaults provided
 * 
 * Prevents: Undefined behavior from missing options
 */
constructor(options?: { log?: Function, errorLog?: Function })
```

**Invariant**: Runtime always has valid logging functions.

#### State Machine Invariants

```javascript
/**
 * **Poka-Yoke Invariant**: State is always one of: Uninitialized, Loading, Ready, Executing, Error, Destroyed
 * 
 * Valid transitions:
 * - Uninitialized => Loading => Ready
 * - Uninitialized => Loading => Error
 * - Ready => Executing => Ready
 * - Ready => Executing => Error
 * - Any => Destroyed (terminal state)
 * 
 * Invalid transitions prevented:
 * - Cannot execute before load (Uninitialized => Executing)
 * - Cannot load after destroy (Destroyed => Loading)
 * - Cannot execute after destroy (Destroyed => Executing)
 */
state: NodeRuntimeState
```

**Invariant**: State is always valid and transitions follow valid paths only.

#### Type Guard Invariants

```javascript
/**
 * **Poka-Yoke Invariant**: isReady() returns true only when:
 * - state === 'Ready'
 * - atomvmPath !== null
 * 
 * Prevents: Operations when runtime is not ready
 */
isReady(): boolean
```

**Invariant**: `isReady()` accurately reflects runtime readiness.

```javascript
/**
 * **Poka-Yoke Invariant**: isLoaded() returns true only when:
 * - state === 'Ready' OR state === 'Executing'
 * 
 * Prevents: State inconsistency
 */
isLoaded(): boolean
```

**Invariant**: `isLoaded()` accurately reflects load state.

#### Operation Invariants

```javascript
/**
 * **Poka-Yoke Invariant**: load() can only be called when:
 * - state !== 'Destroyed'
 * - state !== 'Loading' (prevents multiple loads)
 * 
 * Prevents: Multiple loads, loads after destroy
 */
async load(): Promise<void>
```

**Invariant**: `load()` only succeeds in valid states.

```javascript
/**
 * **Poka-Yoke Invariant**: execute(avmPath) can only be called when:
 * - isReady() === true
 * - avmPath is non-empty string
 * 
 * Prevents: Execution before load, execution with invalid path
 */
async execute(avmPath: string): Promise<any>
```

**Invariant**: `execute()` only succeeds when ready and with valid input.

```javascript
/**
 * **Poka-Yoke Invariant**: destroy() sets state to 'Destroyed' (terminal state)
 * 
 * Prevents: All operations after destroy
 */
destroy(): void
```

**Invariant**: After `destroy()`, no operations are possible.

## State Machine Transitions

### AtomVMRuntime State Machine

```
Uninitialized
    ↓ (loadWASM)
Loading
    ↓ (success)
Ready
    ↓ (executeBeam)
Executing
    ↓ (success)
Ready
    ↓ (error)
Error
    ↓ (destroy)
Destroyed (terminal)

Any State
    ↓ (destroy)
Destroyed (terminal)
```

### AtomVMNodeRuntime State Machine

```
Uninitialized
    ↓ (load)
Loading
    ↓ (success)
Ready
    ↓ (execute)
Executing
    ↓ (success)
Ready
    ↓ (error)
Error
    ↓ (destroy)
Destroyed (terminal)

Any State
    ↓ (destroy)
Destroyed (terminal)
```

## Usage Patterns

### Safe Usage Pattern

```javascript
// ✅ CORRECT: Follow state machine transitions
const runtime = new AtomVMRuntime(terminal, 'mymodule');
await runtime.loadWASM(); // Uninitialized => Loading => Ready
await runtime.executeBeam('/mymodule.avm'); // Ready => Executing => Ready
runtime.destroy(); // Any => Destroyed
```

### Invalid Usage Patterns (Prevented)

```javascript
// ❌ PREVENTED: Execute before load
const runtime = new AtomVMRuntime(terminal, 'mymodule');
await runtime.executeBeam('/mymodule.avm'); // Throws: Runtime not ready

// ❌ PREVENTED: Load after destroy
const runtime = new AtomVMRuntime(terminal, 'mymodule');
runtime.destroy();
await runtime.loadWASM(); // Throws: Runtime has been destroyed

// ❌ PREVENTED: Empty moduleName
const runtime = new AtomVMRuntime(terminal, ''); // Throws: moduleName is required

// ❌ PREVENTED: Empty avmPath
await runtime.executeBeam(''); // Throws: avmPath is required
```

## Poka-Yoke Principles Applied

1. **Make Invalid States Unrepresentable**: State machine ensures only valid states exist
2. **Validate at Entry Points**: All inputs validated at construction and method entry
3. **Type Guards**: Functions check state before operations
4. **Explicit State Transitions**: All state changes are explicit and validated
5. **Terminal States**: Destroyed state prevents all further operations

