package wasm

import (
	"context"
	"encoding/json"
	"fmt"
	"path/filepath"
	"strings"
)

// Loader manages WASM module loading and caching for policy packs.
type Loader struct {
	runtime *Runtime
	modules map[string]*Module
}

// WASMEffectConfig represents a WASM effect configuration in a policy pack.
type WASMEffectConfig struct {
	WASM    string            `json:"wasm"`    // Path to WASM file
	Exports []string          `json:"exports"` // Exported functions to use
	Args    []string          `json:"args"`    // Command line arguments
	Env     map[string]string `json:"env"`     // Environment variables
}

// PackWASMEffects represents WASM effects defined in a policy pack.
type PackWASMEffects struct {
	Effects map[string]WASMEffectConfig `json:"effects"`
}

// NewLoader creates a new WASM loader.
func NewLoader(runtime *Runtime) *Loader {
	return &Loader{
		runtime: runtime,
		modules: make(map[string]*Module),
	}
}

// LoadPackEffects loads WASM effects from a policy pack configuration.
func (l *Loader) LoadPackEffects(ctx context.Context, packDir string, effects PackWASMEffects) error {
	for effectName, config := range effects.Effects {
		// Build full path to WASM file
		wasmPath := filepath.Join(packDir, config.WASM)

		// Create module configuration
		moduleConfig := ModuleConfig{
			Exports: config.Exports,
			Args:    config.Args,
			Env:     config.Env,
		}

		// Load the module
		module, err := l.runtime.LoadModuleFromFile(ctx, effectName, wasmPath, moduleConfig)
		if err != nil {
			return fmt.Errorf("failed to load WASM module for effect %s: %w", effectName, err)
		}

		// Store the module
		l.modules[effectName] = module
	}

	return nil
}

// GetEffectModule returns a loaded WASM module for a specific effect.
func (l *Loader) GetEffectModule(effectName string) (*Module, error) {
	module, exists := l.modules[effectName]
	if !exists {
		return nil, fmt.Errorf("WASM effect module %s not found", effectName)
	}
	return module, nil
}

// ListEffectModules returns a list of loaded effect module names.
func (l *Loader) ListEffectModules() []string {
	var names []string
	for name := range l.modules {
		names = append(names, name)
	}
	return names
}

// ExecuteEffect executes a specific WASM effect with the given hook data.
func (l *Loader) ExecuteEffect(ctx context.Context, effectName string, hookData []byte) (*HookEffect, error) {
	module, err := l.GetEffectModule(effectName)
	if err != nil {
		return nil, err
	}

	return l.runtime.ExecuteHook(ctx, module, hookData)
}

// UnloadModule unloads a specific WASM module.
func (l *Loader) UnloadModule(effectName string) error {
	if _, exists := l.modules[effectName]; !exists {
		return fmt.Errorf("module %s not found", effectName)
	}

	// Close the module (if needed)
	delete(l.modules, effectName)
	return nil
}

// UnloadAll unloads all loaded WASM modules.
func (l *Loader) UnloadAll() {
	for name := range l.modules {
		delete(l.modules, name)
	}
}

// ParsePackEffects parses WASM effects from JSON configuration.
func ParsePackEffects(jsonData []byte) (PackWASMEffects, error) {
	var effects PackWASMEffects
	if err := json.Unmarshal(jsonData, &effects); err != nil {
		return effects, fmt.Errorf("failed to parse WASM effects: %w", err)
	}
	return effects, nil
}

// ValidateEffectConfig validates a WASM effect configuration.
func ValidateEffectConfig(config WASMEffectConfig) error {
	if config.WASM == "" {
		return fmt.Errorf("WASM file path is required")
	}

	if !strings.HasSuffix(config.WASM, ".wasm") {
		return fmt.Errorf("WASM file must have .wasm extension")
	}

	if len(config.Exports) == 0 {
		return fmt.Errorf("at least one export function must be specified")
	}

	return nil
}

// GetRuntime returns the underlying WASM runtime.
func (l *Loader) GetRuntime() *Runtime {
	return l.runtime
}
