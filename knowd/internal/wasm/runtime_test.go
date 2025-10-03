package wasm

import (
	"context"
	"testing"
)

func TestRuntime_NewRuntime(t *testing.T) {
	config := Config{
		MaxMemoryMB:   64,
		EnableWASI:    true,
		EnableLogging: false,
		WorkingDir:    "/tmp",
	}

	runtime, err := NewRuntime(config)
	if err != nil {
		t.Errorf("NewRuntime() error = %v", err)
		return
	}

	if runtime.config.MaxMemoryMB != 64 {
		t.Errorf("NewRuntime() MaxMemoryMB = %v, want 64", runtime.config.MaxMemoryMB)
	}

	if !runtime.config.EnableWASI {
		t.Error("NewRuntime() EnableWASI should be true")
	}

	// Clean up
	runtime.Close(context.Background())
}

func TestRuntime_NewRuntimeDefaults(t *testing.T) {
	config := Config{} // Empty config should use defaults

	runtime, err := NewRuntime(config)
	if err != nil {
		t.Errorf("NewRuntime() error = %v", err)
		return
	}

	if runtime.config.MaxMemoryMB != 64 {
		t.Errorf("NewRuntime() default MaxMemoryMB = %v, want 64", runtime.config.MaxMemoryMB)
	}

	if runtime.config.WorkingDir != "/tmp" {
		t.Errorf("NewRuntime() default WorkingDir = %v, want /tmp", runtime.config.WorkingDir)
	}

	runtime.Close(context.Background())
}

func TestRuntime_LoadModule(t *testing.T) {
	config := Config{
		MaxMemoryMB:   64,
		EnableWASI:    true,
		EnableLogging: false,
		WorkingDir:    "/tmp",
	}

	runtime, err := NewRuntime(config)
	if err != nil {
		t.Errorf("NewRuntime() error = %v", err)
		return
	}
	defer runtime.Close(context.Background())

	// For testing, we'll use a minimal WASM module that just exports a function
	// In a real scenario, this would be actual compiled WASM bytes
	wasmBytes := []byte{} // Empty for now - would need actual WASM

	moduleConfig := ModuleConfig{
		Exports: []string{"run"},
		Args:    []string{},
		Env:     map[string]string{"TEST": "value"},
	}

	// This will likely fail with empty WASM bytes, but tests the API
	_, err = runtime.LoadModule(context.Background(), "test-module", wasmBytes, moduleConfig)
	// We expect this to fail since we're not providing valid WASM
	if err == nil {
		t.Error("LoadModule() should fail with empty WASM bytes")
	}
}

func TestModule_ListExports(t *testing.T) {
	// This test would require a valid WASM module
	// For now, we'll just test the API structure

	module := &Module{
		name:   "test-module",
		module: nil, // Would be a real wazero module
		config: ModuleConfig{
			Exports: []string{"run", "initialize"},
		},
	}

	exports := module.ListExports()
	// In a real implementation, this would return actual exports
	// For now, just verify the method exists
	_ = exports
}

func TestModule_CallFunction(t *testing.T) {
	// This test would require a valid WASM module
	// For now, we'll just test the API structure

	module := &Module{
		name:   "test-module",
		module: nil, // Would be a real wazero module
		config: ModuleConfig{},
	}

	// This will fail since we don't have a real module
	_, err := module.CallFunction(context.Background(), "run", 1, 2, 3)
	if err == nil {
		t.Error("CallFunction() should fail with nil module")
	}
}

func TestHookEffect_Structure(t *testing.T) {
	effect := &HookEffect{
		Success: true,
		Result:  "test result",
		Error:   "",
		Logs:    "test logs",
	}

	if !effect.Success {
		t.Error("HookEffect Success should be true")
	}

	if effect.Result != "test result" {
		t.Errorf("HookEffect Result = %v, want test result", effect.Result)
	}

	if effect.Error != "" {
		t.Errorf("HookEffect Error = %v, want empty", effect.Error)
	}

	if effect.Logs != "test logs" {
		t.Errorf("HookEffect Logs = %v, want test logs", effect.Logs)
	}
}

func TestModule_GetName(t *testing.T) {
	module := &Module{
		name:   "test-module",
		module: nil,
		config: ModuleConfig{},
	}

	if module.GetName() != "test-module" {
		t.Errorf("GetName() = %v, want test-module", module.GetName())
	}
}

func TestModule_GetConfig(t *testing.T) {
	config := ModuleConfig{
		Exports: []string{"run"},
		Args:    []string{"arg1"},
		Env:     map[string]string{"ENV": "value"},
	}

	module := &Module{
		name:   "test-module",
		module: nil,
		config: config,
	}

	retrieved := module.GetConfig()
	if retrieved.Exports[0] != "run" {
		t.Errorf("GetConfig() Exports = %v, want [run]", retrieved.Exports)
	}

	if retrieved.Args[0] != "arg1" {
		t.Errorf("GetConfig() Args = %v, want [arg1]", retrieved.Args)
	}

	if retrieved.Env["ENV"] != "value" {
		t.Errorf("GetConfig() Env = %v, want ENV=value", retrieved.Env)
	}
}
