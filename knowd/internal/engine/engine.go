// Package engine provides core orchestration for knowd.
// It wraps store, runs queries, executes hooks, and coordinates responses.
package engine

import (
	"context"

	"github.com/google/uuid"
	"github.com/unrdf/knowd/internal/hooks"
	"github.com/unrdf/knowd/internal/policy"
	"github.com/unrdf/knowd/internal/sparql"
	"github.com/unrdf/knowd/internal/store"
)

// Engine represents the core orchestration engine.
type Engine struct {
	store        store.Interface
	hooks        *hooks.Registry
	parser       *sparql.Parser
	cache        *sparql.PlanCache
	executor     *sparql.Executor
	loader       *policy.Loader
	packRegistry map[string]*policy.Pack
}

// TxRequest represents a transaction request.
type TxRequest struct {
	Delta struct {
		Add []Quad `json:"add"`
		Rem []Quad `json:"rem"`
	} `json:"delta"`
	Actor string `json:"actor"`
}

// Quad represents an RDF quad (subject, predicate, object, graph).
type Quad struct {
	Subject   string `json:"subject"`
	Predicate string `json:"predicate"`
	Object    string `json:"object"`
	Graph     string `json:"graph"`
}

// TxResult represents the result of a transaction.
type TxResult struct {
	ReceiptID string `json:"receiptId"`
	Added     int    `json:"added"`
	Removed   int    `json:"removed"`
}

// QueryRequest represents a query request.
type QueryRequest struct {
	Query string `json:"query"`
	Kind  string `json:"kind"`
}

// QueryResponse represents a query response.
type QueryResponse struct {
	Rows []map[string]interface{} `json:"rows"`
	Kind string                   `json:"kind"`
}

// HookEvalRequest represents a hook evaluation request.
type HookEvalRequest struct {
	Hook    map[string]interface{} `json:"hook"`
	Persist bool                   `json:"persist"`
}

// HookEvalResponse represents a hook evaluation response.
type HookEvalResponse struct {
	Fired  bool                   `json:"fired"`
	Result map[string]interface{} `json:"result,omitempty"`
}

// Config holds engine configuration.
type Config struct {
	Store store.Config
	Cache sparql.CacheConfig
	Hooks hooks.Config
}

// New creates a new engine instance.
func New(config Config) (*Engine, error) {
	// Create store
	s, err := store.NewMemoryStore(config.Store)
	if err != nil {
		return nil, err
	}

	// Create SPARQL parser
	parser := sparql.NewParser()

	// Create plan cache
	cache := sparql.NewPlanCache(config.Cache)

	// Create executor
	executor := sparql.NewExecutor()

	// Create hooks registry
	hooksRegistry := hooks.NewRegistry(config.Hooks)

	// Create policy loader
	loader := policy.NewLoader()

	return &Engine{
		store:        s,
		hooks:        hooksRegistry,
		parser:       parser,
		cache:        cache,
		executor:     executor,
		loader:       loader,
		packRegistry: make(map[string]*policy.Pack),
	}, nil
}

// Tx processes a transaction by adding/removing quads and running hooks.
func (e *Engine) Tx(ctx context.Context, req TxRequest) (*TxResult, error) {
	// Add quads
	var added int
	for _, quad := range req.Delta.Add {
		err := e.store.AddQuad(ctx, store.Quad{
			Subject:   quad.Subject,
			Predicate: quad.Predicate,
			Object:    quad.Object,
			Graph:     quad.Graph,
		})
		if err != nil {
			continue
		}
		exists, _ := e.store.HasQuad(ctx, store.Quad{
			Subject:   quad.Subject,
			Predicate: quad.Predicate,
			Object:    quad.Object,
			Graph:     quad.Graph,
		})
		if exists {
			added++
		}
	}

	// Remove quads
	var removed int
	for _, quad := range req.Delta.Rem {
		err := e.store.RemoveQuad(ctx, store.Quad{
			Subject:   quad.Subject,
			Predicate: quad.Predicate,
			Object:    quad.Object,
			Graph:     quad.Graph,
		})
		if err != nil {
			continue
		}
		removed++
	}

	// Run hooks
	_, err := e.hooks.Evaluate(ctx, req.Actor)
	if err != nil {
		// Error evaluating hooks - silently continue for now
	}

	// Generate receipt ID (simplified)
	receiptID := ctx.Value("receipt-id")
	if receiptID == nil {
		receiptID = "tx-receipt-" + generateID()
	}

	return &TxResult{
		ReceiptID: receiptID.(string),
		Added:     added,
		Removed:   removed,
	}, nil
}

// Query executes a SPARQL query against the store.
func (e *Engine) Query(ctx context.Context, req QueryRequest) (*QueryResponse, error) {

	// Try to get plan from cache
	var plan *sparql.Plan
	cachedPlan := e.cache.Get(req.Query)
	if cachedPlan != nil {
		plan = cachedPlan
	} else {
		// Parse and compile query
		var err error
		plan, err = e.parser.Parse(req.Query)
		if err != nil {
			return nil, err
		}

		// Cache the plan
		e.cache.Put(req.Query, plan)
	}

	// Execute plan against store using executor
	sparqlResult, err := e.executor.Execute(ctx, e.store, req.Kind, plan)
	if err != nil {
		return nil, err
	}

	// Convert sparql.QueryResponse to engine.QueryResponse
	result := &QueryResponse{
		Rows: sparqlResult.Rows,
		Kind: sparqlResult.Kind,
	}

	return result, nil
}

// EvaluateHooks evaluates all registered hooks.
func (e *Engine) EvaluateHooks(ctx context.Context, req HookEvalRequest) (*HookEvalResponse, error) {

	// Extract actor from request, default to "api-user"
	actor := "api-user"
	if actorInterface, ok := req.Hook["actor"]; ok {
		if actorStr, ok := actorInterface.(string); ok {
			actor = actorStr
		}
	}

	results, err := e.hooks.Evaluate(ctx, actor)
	if err != nil {
		return &HookEvalResponse{Fired: false}, err
	}

	if len(results) > 0 {
		// Convert HookResult to map for JSON response
		result := map[string]interface{}{
			"hookId": results[0].HookID,
			"name":   results[0].Name,
			"fired":  results[0].Fired,
			"data":   results[0].Data,
		}
		return &HookEvalResponse{
			Fired:  true,
			Result: result,
		}, nil
	}

	return &HookEvalResponse{Fired: false}, nil
}

// LoadPolicyPacks loads policy packs into the engine.
func (e *Engine) LoadPolicyPacks(ctx context.Context, paths []string) error {
	for _, path := range paths {

		// Load pack using policy loader
		pack, err := e.loader.LoadPack(path)
		if err != nil {
			continue
		}

		// Store in registry
		e.packRegistry[pack.Name] = pack

		// Register hooks from the pack
		for _, hook := range pack.Hooks {
			hookDef := &hooks.HookDefinition{
				ID:       hook.ID,
				Name:     hook.Name,
				Type:     hook.Type,
				Query:    hook.Query,
				Schedule: hook.Schedule,
				Config:   hook.Config,
				Enabled:  hook.Enabled,
			}

			if err := e.hooks.Register(hookDef); err != nil {
				// Failed to register hook - silently continue for now
			}
		}
	}

	return nil
}

// GetLoadedPacks returns all loaded policy packs.
func (e *Engine) GetLoadedPacks() map[string]*policy.Pack {
	return e.packRegistry
}

// GetPack returns a specific loaded pack.
func (e *Engine) GetPack(name string) (*policy.Pack, bool) {
	pack, exists := e.packRegistry[name]
	return pack, exists
}

// ReloadPack reloads a specific pack from disk.
func (e *Engine) ReloadPack(ctx context.Context, path string) error {
	pack, err := e.loader.ReloadPack(path)
	if err != nil {
		return err
	}

	// Update registry
	e.packRegistry[pack.Name] = pack

	// Re-register hooks
	for _, hook := range pack.Hooks {
		hookDef := &hooks.HookDefinition{
			ID:       hook.ID,
			Name:     hook.Name,
			Type:     hook.Type,
			Query:    hook.Query,
			Schedule: hook.Schedule,
			Config:   hook.Config,
			Enabled:  hook.Enabled,
		}

		// Unregister old hook first
		e.hooks.Unregister(hook.ID)

		// Register new hook
		if err := e.hooks.Register(hookDef); err != nil {
			// Failed to reload hook - silently continue for now
		}
	}

	return nil
}

// GetStats returns engine statistics.
func (e *Engine) GetStats(ctx context.Context) map[string]interface{} {
	stats := map[string]interface{}{
		"quads":     e.store.GetQuadCount(),
		"hooks":     e.hooks.GetHookCount(),
		"plans":     e.cache.Size(),
		"packCount": len(e.packRegistry),
	}

	return stats
}

// generateID generates a unique ID using UUID v4.
func generateID() string {
	return uuid.New().String()
}
