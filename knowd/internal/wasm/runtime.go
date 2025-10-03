package wasm

import (
	"context"
	"fmt"
	"os"

	"github.com/tetratelabs/wazero"
	"github.com/tetratelabs/wazero/api"
	"github.com/tetratelabs/wazero/imports/wasi_snapshot_preview1"
)

// Runtime represents a WebAssembly runtime for executing hook effects.
type Runtime struct {
	runtime wazero.Runtime
	config  Config
}

// Config holds configuration for the WASM runtime.
type Config struct {
	MaxMemoryMB   int    // Maximum memory per module (default 64MB)
	EnableWASI    bool   // Enable WASI support
	EnableLogging bool   // Enable WASM stdout/stderr logging
	WorkingDir    string // Working directory for modules
}

// Module represents a compiled WASM module.
type Module struct {
	name   string
	module api.Module
	config ModuleConfig
}

// ModuleConfig holds configuration for a specific module.
type ModuleConfig struct {
	Exports []string          // Exported functions to expose
	Args    []string          // Command line arguments
	Env     map[string]string // Environment variables
}

// HookEffect represents the result of executing a hook effect.
type HookEffect struct {
	Success bool        `json:"success"`
	Result  interface{} `json:"result,omitempty"`
	Error   string      `json:"error,omitempty"`
	Logs    string      `json:"logs,omitempty"`
}

// NewRuntime creates a new WASM runtime with the given configuration.
func NewRuntime(config Config) (*Runtime, error) {
	ctx := context.Background()

	// Set defaults
	if config.MaxMemoryMB == 0 {
		config.MaxMemoryMB = 64
	}
	if config.WorkingDir == "" {
		config.WorkingDir = "/tmp"
	}

	// Create wazero runtime
	runtime := wazero.NewRuntime(ctx)

	// If WASI is enabled, instantiate it
	if config.EnableWASI {
		_, err := wasi_snapshot_preview1.Instantiate(ctx, runtime)
		if err != nil {
			return nil, fmt.Errorf("failed to instantiate WASI: %w", err)
		}
	}

	return &Runtime{
		runtime: runtime,
		config:  config,
	}, nil
}

// LoadModule loads a WASM module from bytes.
func (r *Runtime) LoadModule(ctx context.Context, name string, wasmBytes []byte, moduleConfig ModuleConfig) (*Module, error) {
	// Compile the module
	compiledModule, err := r.runtime.CompileModule(ctx, wasmBytes)
	if err != nil {
		return nil, fmt.Errorf("failed to compile WASM module %s: %w", name, err)
	}

	// Create module config
	config := wazero.NewModuleConfig().
		WithName(name).
		WithArgs(moduleConfig.Args...)

	// Set environment variables
	for key, value := range moduleConfig.Env {
		config = config.WithEnv(key, value)
	}

	// Instantiate the module
	module, err := r.runtime.InstantiateModule(ctx, compiledModule, config)
	if err != nil {
		return nil, fmt.Errorf("failed to instantiate WASM module %s: %w", name, err)
	}

	return &Module{
		name:   name,
		module: module,
		config: moduleConfig,
	}, nil
}

// LoadModuleFromFile loads a WASM module from a file path.
func (r *Runtime) LoadModuleFromFile(ctx context.Context, name, filePath string, moduleConfig ModuleConfig) (*Module, error) {
	wasmBytes, err := os.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read WASM file %s: %w", filePath, err)
	}

	return r.LoadModule(ctx, name, wasmBytes, moduleConfig)
}

// ExecuteHook executes a hook effect using a WASM module.
func (r *Runtime) ExecuteHook(ctx context.Context, module *Module, hookData []byte) (*HookEffect, error) {
	// Get the exported function (default to "run")
	exportName := "run"
	if len(module.config.Exports) > 0 {
		exportName = module.config.Exports[0]
	}

	// Get the exported function
	runFn := module.module.ExportedFunction(exportName)
	if runFn == nil {
		return nil, fmt.Errorf("exported function %s not found in module %s", exportName, module.name)
	}

	// For now, call the function with no parameters
	// In a real implementation, you'd pass hook data as memory or parameters
	_, err := runFn.Call(ctx)
	if err != nil {
		return &HookEffect{
			Success: false,
			Error:   fmt.Sprintf("WASM execution failed: %v", err),
		}, nil
	}

	return &HookEffect{
		Success: true,
		Result:  "WASM hook executed successfully",
		Logs:    "Hook effect completed",
	}, nil
}

// writeToMemory writes data to the module's linear memory.
// This is a placeholder for future implementation when we need to pass data to WASM modules.
func (r *Runtime) writeToMemory(ctx context.Context, module api.Module, data []byte) (uint32, error) {
	// For now, return a dummy pointer
	// In a real implementation, you'd call an allocator function exported by the WASM module
	return 0, nil
}

// readFromMemory reads data from the module's linear memory.
// This is a placeholder for future implementation when we need to read data from WASM modules.
func (r *Runtime) readFromMemory(ctx context.Context, module api.Module, offset, length uint32) ([]byte, error) {
	// For now, return empty data
	// In a real implementation, you'd read from the specified memory location
	return []byte{}, nil
}

// Close closes the WASM runtime and releases resources.
func (r *Runtime) Close(ctx context.Context) error {
	return r.runtime.Close(ctx)
}

// GetModule returns information about a loaded module.
func (m *Module) GetModule() api.Module {
	return m.module
}

// GetName returns the module name.
func (m *Module) GetName() string {
	return m.name
}

// GetConfig returns the module configuration.
func (m *Module) GetConfig() ModuleConfig {
	return m.config
}

// ListExports returns the exported functions from the module.
func (m *Module) ListExports() []string {
	// For now, return the configured exports
	// In a real implementation, you'd query the actual module
	return m.config.Exports
}

// CallFunction calls a specific exported function in the module.
func (m *Module) CallFunction(ctx context.Context, functionName string, params ...uint64) ([]uint64, error) {
	fn := m.module.ExportedFunction(functionName)
	if fn == nil {
		return nil, fmt.Errorf("function %s not found", functionName)
	}

	return fn.Call(ctx, params...)
}
