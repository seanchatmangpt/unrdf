package hooks

import (
	"context"
	"fmt"
	"sync"
)

// Registry manages hook registration and evaluation.
type Registry struct {
	mu     sync.RWMutex
	hooks  map[string]*HookDefinition
	config Config
}

// Config holds registry configuration.
type Config struct {
	MaxHooks int
}

// HookDefinition represents a registered hook.
type HookDefinition struct {
	ID       string
	Name     string
	Type     string
	Query    string
	Schedule string
	Config   map[string]interface{}
	Enabled  bool
}

// HookResult represents the result of hook evaluation.
type HookResult struct {
	HookID string
	Name   string
	Fired  bool
	Data   map[string]interface{}
}

// NewRegistry creates a new hook registry.
func NewRegistry(config Config) *Registry {
	return &Registry{
		hooks:  make(map[string]*HookDefinition),
		config: config,
	}
}

// Register adds a hook to the registry.
func (r *Registry) Register(hook *HookDefinition) error {
	r.mu.Lock()
	defer r.mu.Unlock()

	// Check limits
	if r.config.MaxHooks > 0 && len(r.hooks) >= r.config.MaxHooks {
		return fmt.Errorf("registry limit reached: %d hooks", r.config.MaxHooks)
	}

	// Validate hook
	if hook.ID == "" {
		return fmt.Errorf("hook ID is required")
	}
	if hook.Name == "" {
		return fmt.Errorf("hook name is required")
	}
	if hook.Type == "" {
		return fmt.Errorf("hook type is required")
	}

	r.hooks[hook.ID] = hook
	return nil
}

// Unregister removes a hook from the registry.
func (r *Registry) Unregister(hookID string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	delete(r.hooks, hookID)
}

// GetHook retrieves a hook by ID.
func (r *Registry) GetHook(hookID string) (*HookDefinition, bool) {
	r.mu.RLock()
	defer r.mu.RUnlock()
	hook, exists := r.hooks[hookID]
	return hook, exists
}

// ListHooks returns all registered hooks.
func (r *Registry) ListHooks() []*HookDefinition {
	r.mu.RLock()
	defer r.mu.RUnlock()

	hooks := make([]*HookDefinition, 0, len(r.hooks))
	for _, hook := range r.hooks {
		hooks = append(hooks, hook)
	}
	return hooks
}

// Evaluate runs all enabled hooks for a given context.
func (r *Registry) Evaluate(ctx context.Context, actor string) ([]HookResult, error) {
	r.mu.RLock()
	defer r.mu.RUnlock()

	var results []HookResult
	for _, hook := range r.hooks {
		if !hook.Enabled {
			continue
		}

		result := r.evaluateHook(ctx, hook, actor)
		results = append(results, result)
	}

	return results, nil
}

// evaluateHook evaluates a single hook.
func (r *Registry) evaluateHook(ctx context.Context, hook *HookDefinition, actor string) HookResult {
	result := HookResult{
		HookID: hook.ID,
		Name:   hook.Name,
		Fired:  false,
		Data:   make(map[string]interface{}),
	}

	// Simple evaluation logic based on hook type
	switch hook.Type {
	case "sparql-ask":
		result.Fired = true
		result.Data["query"] = hook.Query
		result.Data["actor"] = actor
	case "threshold":
		result.Fired = true
		result.Data["threshold"] = hook.Config["threshold"]
	case "count":
		result.Fired = true
		result.Data["count"] = hook.Config["count"]
	case "window":
		result.Fired = true
		result.Data["window"] = hook.Schedule
	default:
		result.Fired = false
	}

	return result
}

// GetHookCount returns the number of registered hooks.
func (r *Registry) GetHookCount() int {
	r.mu.RLock()
	defer r.mu.RUnlock()
	return len(r.hooks)
}

// Clear removes all hooks from the registry.
func (r *Registry) Clear() {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.hooks = make(map[string]*HookDefinition)
}
