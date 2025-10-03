// Package engine provides core orchestration for knowd.
// It wraps store, runs queries, executes hooks, and coordinates responses.
package engine

import (
	"context"
	"fmt"
	"log"
	"time"

	"github.com/unrdf/knowd/internal/hooks"
	"github.com/unrdf/knowd/internal/sparql"
	"github.com/unrdf/knowd/internal/store"
)

// Engine represents the core orchestration engine.
type Engine struct {
	store  store.Interface
	hooks  *hooks.Registry
	parser *sparql.Parser
	cache  *sparql.PlanCache
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
	Store  store.Config
	Cache  sparql.CacheConfig
	Hooks  hooks.Config
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

	// Create hooks registry
	hooksRegistry := hooks.NewRegistry(config.Hooks)

	return &Engine{
		store:  s,
		hooks:  hooksRegistry,
		parser: parser,
		cache:  cache,
	}, nil
}

// Tx processes a transaction by adding/removing quads and running hooks.
func (e *Engine) Tx(ctx context.Context, req TxRequest) (*TxResult, error) {
	log.Printf("Processing transaction for actor: %s", req.Actor)

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
			log.Printf("Error adding quad: %v", err)
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
			log.Printf("Error removing quad: %v", err)
			continue
		}
		removed++
	}

	// Run hooks
	log.Printf("Running hooks after transaction")
	hookResults, err := e.hooks.Evaluate(ctx, req.Actor)
	if err != nil {
		log.Printf("Error evaluating hooks: %v", err)
	}

	log.Printf("Hook evaluation produced %d results", len(hookResults))

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
	log.Printf("Executing query: %s (kind: %s)", req.Query, req.Kind)

	// Try to get plan from cache
	var plan *sparql.Plan
	cachedPlan := e.cache.Get(req.Query)
	if cachedPlan != nil {
		plan = cachedPlan
		log.Printf("Using cached plan")
	} else {
		// Parse and compile query
		var err error
		plan, err = e.parser.Parse(req.Query)
		if err != nil {
			log.Printf("Query parse error: %v", err)
			return nil, err
		}

		// Cache the plan
		e.cache.Put(req.Query, plan)
		log.Printf("Cached new plan")
	}

	// Execute plan against store
	result, err := plan.Execute(ctx, e.store, req.Kind)
	if err != nil {
		log.Printf("Query execution error: %v", err)
		return nil, err
	}

	return result, nil
}

// EvaluateHooks evaluates all registered hooks.
func (e *Engine) EvaluateHooks(ctx context.Context, req HookEvalRequest) (*HookEvalResponse, error) {
	log.Printf("Evaluating hooks")

	results, err := e.hooks.Evaluate(ctx, req.Hook["actor"].(string))
	if err != nil {
		log.Printf("Hook evaluation error: %v", err)
		return &HookEvalResponse{Fired: false}, err
	}

	if len(results) > 0 {
		return &HookEvalResponse{
			Fired:  true,
			Result: results[0],
		}, nil
	}

	return &HookEvalResponse{Fired: false}, nil
}

// LoadPolicyPacks loads policy packs into the engine.
func (e *Engine) LoadPolicyPacks(ctx context.Context, paths []string) error {
	log.Printf("Loading policy packs from %d paths", len(paths))
	
	for _, path := range paths {
		log.Printf("Loading pack: %s", path)
		// TODO: Implement pack loading
	}
	
	return nil
}

// GetStats returns engine statistics.
func (e *Engine) GetStats(ctx context.Context) map[string]interface{} {
	stats := map[string]interface{}{
		"quads":  e.store.GetQuadCount(),
		"hooks":  e.hooks.GetHookCount(),
		"plans":  e.cache.Size(),
	}
	
	return stats
}

// generateID generates a simple unique ID (placeholder implementation).
func generateID() string {
	// Simple implementation - in real version would use proper UUID
	return "id-" + fmt.Sprintf("%d", time.Now().UnixNano())
}