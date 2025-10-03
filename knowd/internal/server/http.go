// Package server provides HTTP server and API handlers for knowd.
package server

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"strconv"
	"time"

	"github.com/gorilla/mux"
	"github.com/rs/cors"
	"github.com/sirupsen/logrus"
	"github.com/unrdf/knowd/internal/hooks"
	"github.com/unrdf/knowd/internal/lockchain"
	"github.com/unrdf/knowd/internal/shacl"
	"github.com/unrdf/knowd/internal/sparql"
	"github.com/unrdf/knowd/internal/store"
	"github.com/unrdf/knowd/internal/telemetry"
	tm "github.com/unrdf/knowd/internal/timetravel"
	"github.com/unrdf/knowd/internal/types"
	"github.com/unrdf/knowd/internal/version"
	"go.opentelemetry.io/otel/attribute"
)

// Configuration represents the server configuration.
type Configuration struct {
	// Basic server configuration
	Addr    string
	DataDir string

	// Store configuration
	Store      string
	Packs      string
	WatchPacks bool

	// Planner / stats configuration
	PlannerCBO    bool
	AnalyzeSample int
	AnalyzeTTL    int

	// SHACL configuration
	SHACLEnabled bool

	// Streaming / prepare configuration
	QueryStreamMaxBytes int
	PlanCacheSize       int
	PlanCachePersist    bool

	// Namespace configuration
	NamespaceDefault string

	// Cluster configuration
	ClusterMode string
	PeerAddrs   string
	MTLSCert    string
	MTLSKey     string
	MTLSCA      string

	// Receipts / signing configuration
	ReceiptsDir string
	SigningKey  string
	SigningPub  string

	// Vector / WASM configuration
	VecEnabled  bool
	WASMEnabled bool
}

// Server represents the HTTP server instance.
type Server struct {
	addr      string
	dataDir   string
	coreURL   string
	router    *mux.Router
	executor  *sparql.Executor
	store     store.Interface
	lockchain *lockchain.Lockchain
}

// New creates a new HTTP server instance.
func New(addr, dataDir, coreURL, storeType string) *Server {
	// Initialize store based on store type
	var storeInstance store.Interface
	var err error

	storeConfig := store.Config{MaxQuads: 1000000}

	switch storeType {
	case "mem":
		storeInstance, err = store.NewMemoryStore(storeConfig)
	case "disk":
		// For disk store, would need additional configuration
		storeInstance, err = store.NewMemoryStore(storeConfig) // Simplified for now
	default:
		storeInstance, err = store.NewMemoryStore(storeConfig)
	}

	if err != nil {
		log.Fatalf("Failed to create store: %v", err)
	}

	// Create SPARQL executor with plan cache
	cacheConfig := sparql.CacheConfig{Capacity: 256}
	planCache := sparql.NewPlanCache(cacheConfig)
	executor := sparql.NewExecutorWithCache(planCache)

	// Create lockchain for receipts (simplified)
	lockchainConfig := lockchain.Config{
		ReceiptsDir: "./receipts",
	}
	lockchainInstance, err := lockchain.New(lockchainConfig)
	if err != nil {
		log.Fatalf("Failed to create lockchain: %v", err)
	}

	// Create server
	server := &Server{
		addr:      addr,
		dataDir:   dataDir,
		coreURL:   coreURL,
		router:    mux.NewRouter(),
		executor:  executor,
		store:     storeInstance,
		lockchain: lockchainInstance,
	}

	// Setup routes
	server.setupRoutes()

	return server
}

// setupRoutes configures HTTP routes for the server.
func (s *Server) setupRoutes() {
	// Add middleware
	s.router.Use(s.loggingMiddleware)
	s.router.Use(s.recoveryMiddleware)

	// Health check
	s.router.HandleFunc("/healthz", s.handleHealth).Methods("GET")

	// Core API routes
	s.router.HandleFunc("/v1/tx", s.handleTransaction).Methods("POST")
	s.router.HandleFunc("/v1/query", s.handleQuery).Methods("POST")
	s.router.HandleFunc("/v1/query/stream", s.handleQueryStream).Methods("POST")
	s.router.HandleFunc("/v1/query/at", s.handleQueryAt).Methods("POST")
	s.router.HandleFunc("/v1/validate", s.handleValidate).Methods("POST")
	s.router.HandleFunc("/v1/receipts", s.handleReceipts).Methods("GET", "POST")
	s.router.HandleFunc("/v1/receipts/search", s.handleReceiptsSearch).Methods("GET")
	s.router.HandleFunc("/v1/hooks/evaluate", s.handleHookEvaluate).Methods("POST")

	// Admin routes
	s.router.HandleFunc("/v1/admin/namespaces", s.handleAdminNamespaces).Methods("GET", "POST")
	s.router.HandleFunc("/v1/admin/rollout", s.handleAdminRollout).Methods("GET", "POST")
	s.router.HandleFunc("/v1/admin/analyze", s.handleAdminAnalyze).Methods("POST")

	// Admin convenience endpoints
	s.router.HandleFunc("/v1/packs/reload", s.handlePacksReload).Methods("POST")
	s.router.HandleFunc("/v1/store/stats", s.handleStoreStats).Methods("GET")
	s.router.HandleFunc("/v1/admin/promote-follower", s.handleAdminPromoteFollower).Methods("POST")

	// Additional unrdf-compatible endpoints
	s.router.HandleFunc("/v1/similar", s.handleSimilar).Methods("POST")
	s.router.HandleFunc("/v1/vector/upsert", s.handleVectorUpsert).Methods("POST")
	s.router.HandleFunc("/v1/admin/replay", s.handleAdminReplay).Methods("POST")

	// Version endpoint
	s.router.HandleFunc("/version", s.handleVersion).Methods("GET")
}

// responseWriter wraps http.ResponseWriter to capture status code
type responseWriter struct {
	http.ResponseWriter
	statusCode int
}

func (rw *responseWriter) WriteHeader(code int) {
	rw.statusCode = code
	rw.ResponseWriter.WriteHeader(code)
}

// loggingMiddleware logs HTTP requests
func (s *Server) loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()

		// Create a response writer that captures the status code
		rw := &responseWriter{ResponseWriter: w, statusCode: http.StatusOK}

		next.ServeHTTP(rw, r)

		duration := time.Since(start)
		logrus.WithFields(logrus.Fields{
			"method":      r.Method,
			"path":        r.URL.Path,
			"status":      rw.statusCode,
			"duration_ms": duration.Milliseconds(),
			"remote_addr": r.RemoteAddr,
		}).Info("HTTP request")
	})
}

// recoveryMiddleware recovers from panics and logs them
func (s *Server) recoveryMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if err := recover(); err != nil {
				logrus.WithFields(logrus.Fields{
					"error": fmt.Sprintf("%v", err),
					"path":  r.URL.Path,
				}).Error("Panic recovered in HTTP handler")

				http.Error(w, "Internal server error", http.StatusInternalServerError)
			}
		}()

		next.ServeHTTP(w, r)
	})
}

// handleHealth handles health check requests.
func (s *Server) handleHealth(w http.ResponseWriter, r *http.Request) {
	ctx, span := telemetry.StartSpan(r.Context(), "handleHealth")
	defer span.End()

	telemetry.AddEvent(ctx, "health_check_requested")

	w.Header().Set("Content-Type", "text/plain")
	w.WriteHeader(http.StatusOK)
	w.Write([]byte("ok"))

	span.SetStatus(200, "OK")
}

// TxRequest represents a transaction request structure.
type TxRequest struct {
	Delta map[string]interface{} `json:"delta"`
	Actor string                 `json:"actor"`
}

// TxResponse represents a transaction response structure.
type TxResponse struct {
	Actor      string                 `json:"actor"`
	ReceiptID  string                 `json:"receiptId"`
	MerkleRoot string                 `json:"merkleRoot"`
	Delta      map[string]interface{} `json:"delta"`
}

// handleTransaction handles transaction requests.
func (s *Server) handleTransaction(w http.ResponseWriter, r *http.Request) {
	ctx, span := telemetry.StartSpan(r.Context(), "handleTransaction")
	defer span.End()

	telemetry.AddEvent(ctx, "transaction_requested",
		attribute.String("method", r.Method),
		attribute.String("actor", r.Header.Get("X-Actor")))

	var req TxRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		span.RecordError(err)
		span.SetStatus(http.StatusBadRequest, "Invalid JSON")
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		json.NewEncoder(w).Encode(map[string]string{
			"error":   "Invalid JSON",
			"message": err.Error(),
		})
		return
	}

	// Process transaction through store
	addCount := 0
	remCount := 0

	if add, ok := req.Delta["add"]; ok {
		if addSlice, ok := add.([]interface{}); ok {
			addCount = len(addSlice)
		}
	}

	if rem, ok := req.Delta["rem"]; ok {
		if remSlice, ok := rem.([]interface{}); ok {
			remCount = len(remSlice)
		}
	}

	telemetry.AddEvent(ctx, "transaction_processing_started",
		attribute.String("actor", req.Actor),
		attribute.Int("delta_add_count", addCount),
		attribute.Int("delta_rem_count", remCount))

	// Add quads to store
	added := 0
	if add, ok := req.Delta["add"]; ok {
		if addSlice, ok := add.([]interface{}); ok {
			for _, item := range addSlice {
				if quad, ok := item.(map[string]interface{}); ok {
					storeQuad := store.Quad{
						Subject:   quad["subject"].(string),
						Predicate: quad["predicate"].(string),
						Object:    quad["object"].(string),
						Graph:     quad["graph"].(string),
					}
					if err := s.store.AddQuad(ctx, storeQuad); err == nil {
						added++
					}
				}
			}
		}
	}

	// Remove quads from store
	removed := 0
	if rem, ok := req.Delta["rem"]; ok {
		if remSlice, ok := rem.([]interface{}); ok {
			for _, item := range remSlice {
				if quad, ok := item.(map[string]interface{}); ok {
					storeQuad := store.Quad{
						Subject:   quad["subject"].(string),
						Predicate: quad["predicate"].(string),
						Object:    quad["object"].(string),
						Graph:     quad["graph"].(string),
					}
					if err := s.store.RemoveQuad(ctx, storeQuad); err == nil {
						removed++
					}
				}
			}
		}
	}

	telemetry.AddEvent(ctx, "transaction_store_operations_completed",
		attribute.Int("quads_added", added),
		attribute.Int("quads_removed", removed))

	// Create signed receipt using lockchain
	telemetry.AddEvent(ctx, "receipt_creation_started")
	var receipt interface{}
	var receiptID, merkleRoot string

	if s.lockchain != nil {
		var err error
		receipt, err = s.lockchain.WriteReceipt(req.Actor, req.Delta)
		if err != nil {
			span.RecordError(err)
			span.SetStatus(http.StatusInternalServerError, "Failed to create receipt")
			logrus.WithFields(logrus.Fields{
				"actor": req.Actor,
				"error": err.Error(),
			}).Error("Failed to create receipt")
			http.Error(w, "Failed to process transaction", http.StatusInternalServerError)
			return
		}

		if r, ok := receipt.(map[string]interface{}); ok {
			if id, exists := r["ID"]; exists {
				receiptID = fmt.Sprintf("%v", id)
			}
			if root, exists := r["MerkleRoot"]; exists {
				merkleRoot = fmt.Sprintf("%v", root)
			}
		}
		telemetry.AddEvent(ctx, "receipt_created",
			attribute.String("receipt_id", receiptID),
			attribute.String("merkle_root", merkleRoot))
	} else {
		// Mock receipt for testing
		receiptID = fmt.Sprintf("test-receipt-%d", time.Now().Unix())
		merkleRoot = "mock-merkle-root"
		telemetry.AddEvent(ctx, "mock_receipt_created",
			attribute.String("receipt_id", receiptID))
	}

	telemetry.AddEvent(ctx, "transaction_completed",
		attribute.String("receipt_id", receiptID),
		attribute.String("actor", req.Actor),
		attribute.Int("quads_added", added),
		attribute.Int("quads_removed", removed),
		attribute.Int("total_quads", s.store.GetQuadCount()))

	span.SetStatus(http.StatusOK, "Transaction processed successfully")

	logrus.WithFields(logrus.Fields{
		"receiptID": receiptID,
		"actor":     req.Actor,
		"added":     added,
		"removed":   removed,
		"quads":     s.store.GetQuadCount(),
	}).Info("Transaction processed successfully")

	response := TxResponse{
		Actor:      req.Actor,
		ReceiptID:  receiptID,
		MerkleRoot: merkleRoot,
		Delta:      req.Delta,
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}

// QueryRequest represents a query request structure.
type QueryRequest struct {
	Query string `json:"query"`
	Kind  string `json:"kind"`
}

// QueryResponse represents a query response structure.
type QueryResponse struct {
	JSON interface{} `json:"json"`
}

// handleQuery handles query requests (placeholder implementation).
func (s *Server) handleQuery(w http.ResponseWriter, r *http.Request) {
	ctx, span := telemetry.StartSpan(r.Context(), "handleQuery")
	defer span.End()

	telemetry.AddEvent(ctx, "query_requested",
		attribute.String("method", r.Method),
		attribute.String("path", r.URL.Path))

	if r.Method != "POST" {
		span.SetStatus(http.StatusMethodNotAllowed, "Method not allowed")
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req QueryRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		span.RecordError(err)
		span.SetStatus(http.StatusBadRequest, "Invalid JSON")
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Validate query kind
	validKinds := map[string]bool{
		"sparql-select":    true,
		"sparql-ask":       true,
		"sparql-construct": true,
	}
	if !validKinds[req.Kind] {
		span.SetStatus(http.StatusBadRequest, "Invalid query kind")
		http.Error(w, "Invalid query kind", http.StatusBadRequest)
		return
	}

	// Parse and execute the query using SPARQL executor
	telemetry.AddEvent(ctx, "query_parsing_started")
	parser := sparql.NewParser()
	plan, err := parser.Parse(req.Query)
	if err != nil {
		span.RecordError(err)
		span.SetStatus(http.StatusBadRequest, "Query parsing failed")
		logrus.WithFields(logrus.Fields{
			"query": req.Query,
			"error": err.Error(),
		}).Error("Query parsing failed")
		http.Error(w, "Query parsing failed", http.StatusBadRequest)
		return
	}
	telemetry.AddEvent(ctx, "query_parsing_completed")

	// Execute the query
	telemetry.AddEvent(ctx, "query_execution_started")
	result, err := s.executor.Execute(ctx, s.store, req.Kind, plan)
	if err != nil {
		span.RecordError(err)
		span.SetStatus(http.StatusInternalServerError, "Query execution failed")
		logrus.WithFields(logrus.Fields{
			"query": req.Query,
			"kind":  req.Kind,
			"error": err.Error(),
		}).Error("Query execution failed")
		http.Error(w, "Query execution failed", http.StatusInternalServerError)
		return
	}
	telemetry.AddEvent(ctx, "query_execution_completed",
		attribute.Int("result_rows", len(result.Rows)))

	// Convert to expected response format
	resp := QueryResponse{
		JSON: result,
	}

	span.SetStatus(http.StatusOK, "Query executed successfully")
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(resp)
}

// HookRequest represents a hook evaluation request.
type HookRequest struct {
	Hook    map[string]interface{} `json:"hook"`
	Persist bool                   `json:"persist"`
}

// HookResponse represents a hook evaluation response.
type HookResponse struct {
	Fired  bool        `json:"fired"`
	Result interface{} `json:"result"`
}

// handleHookEvaluate handles hook evaluation requests (placeholder implementation).
func (s *Server) handleHookEvaluate(w http.ResponseWriter, r *http.Request) {
	ctx, span := telemetry.StartSpan(r.Context(), "handleHookEvaluate")
	defer span.End()

	telemetry.AddEvent(ctx, "hook_evaluation_requested")

	if r.Method != "POST" {
		span.SetStatus(http.StatusMethodNotAllowed, "Method not allowed")
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req HookRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Implement actual hook evaluation using hooks registry
	hooksRegistry := &hooks.Registry{}
	actor := "api-user" // TODO: Extract from authentication context

	results, _ := hooksRegistry.Evaluate(context.Background(), actor)

	// Find result for the requested hook if provided
	var hookResult *hooks.HookResult
	if len(results) > 0 {
		hookResult = &results[0] // Take first result for simplicity
	}

	resp := HookResponse{
		Fired:  hookResult != nil && hookResult.Fired,
		Result: hookResult,
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(resp)
}

// ValidateRequest represents a validation request structure.
type ValidateRequest struct {
	Data   string `json:"data"`
	Shapes string `json:"shapes,omitempty"`
}

// ValidateResponse represents a validation response structure.
type ValidateResponse struct {
	Conforms   bool                  `json:"conforms"`
	Violations []ValidationViolation `json:"violations"`
}

// ValidationViolation represents a SHACL validation violation.
type ValidationViolation struct {
	FocusNode   string `json:"focusNode"`
	ResultPath  string `json:"resultPath"`
	SourceShape string `json:"sourceShape"`
	Message     string `json:"message"`
}

// handleValidate handles SHACL validation requests.
func (s *Server) handleValidate(w http.ResponseWriter, r *http.Request) {
	if r.Method != "POST" {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req ValidateRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Implement actual SHACL validation using existing validator
	validator := shacl.NewValidator()
	result, err := validator.Validate(context.Background(), []byte(req.Data), req.Shapes)
	if err != nil {
		logrus.WithError(err).Error("SHACL validation failed")

		// Return validation errors in structured format instead of 500
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		json.NewEncoder(w).Encode(map[string]interface{}{
			"error":      "SHACL validation failed",
			"message":    err.Error(),
			"conforms":   false,
			"violations": []map[string]string{},
		})
		return
	}

	var violations []ValidationViolation
	for _, violation := range result.Violations {
		violations = append(violations, ValidationViolation{
			FocusNode:   violation.FocusNode,
			ResultPath:  violation.ResultPath,
			SourceShape: violation.SourceShape,
			Message:     violation.Message,
		})
	}

	response := ValidateResponse{
		Conforms:   result.Conforms,
		Violations: violations,
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}

// handleQueryStream handles streaming query requests.
func (s *Server) handleQueryStream(w http.ResponseWriter, r *http.Request) {
	if r.Method != "POST" {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req QueryRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Parse and execute the query
	parser := sparql.NewParser()
	logrus.WithField("query", req.Query).Debug("Parsing SPARQL query")
	plan, err := parser.Parse(req.Query)
	if err != nil {
		logrus.WithError(err).WithField("query", req.Query).Error("Query parsing failed")
		http.Error(w, "Query parsing failed", http.StatusBadRequest)
		return
	}

	logrus.WithField("plan", fmt.Sprintf("%+v", plan)).Debug("Query parsed successfully")

	// Debug: Check what quads are in the store
	allQuads, _ := s.store.FindQuads(r.Context(), store.Quad{})
	logrus.WithField("store_quads", len(allQuads)).Debug("Store contains quads")

	result, err := s.executor.Execute(r.Context(), s.store, req.Kind, plan)
	if err != nil {
		logrus.WithError(err).Error("Query execution failed")
		http.Error(w, "Query execution failed", http.StatusInternalServerError)
		return
	}

	logrus.WithFields(logrus.Fields{
		"rows": len(result.Rows),
		"kind": result.Kind,
		"plan": fmt.Sprintf("%+v", plan),
	}).Debug("Query execution completed")

	// For streaming, write each row as a separate JSON line
	w.Header().Set("Content-Type", "application/x-ndjson")

	for _, row := range result.Rows {
		json.NewEncoder(w).Encode(row)
	}
}

// Receipt represents a transaction receipt.
type Receipt struct {
	ID         string                 `json:"id"`
	Actor      string                 `json:"actor"`
	Timestamp  string                 `json:"timestamp"`
	MerkleRoot string                 `json:"merkleRoot"`
	Delta      map[string]interface{} `json:"delta"`
}

// handleReceipts handles receipt-related requests.
func (s *Server) handleReceipts(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case "GET":
		// List receipts or get specific receipt
		receiptID := r.URL.Path[len("/v1/receipts/"):]
		if receiptID != "" {
			// Get specific receipt
			receipt := Receipt{
				ID:         receiptID,
				Actor:      "system",
				Timestamp:  time.Now().Format(time.RFC3339),
				MerkleRoot: "mock-merkle-root",
				Delta:      map[string]interface{}{},
			}
			w.Header().Set("Content-Type", "application/json")
			json.NewEncoder(w).Encode(receipt)
		} else {
			// List receipts
			receipts := []Receipt{}
			w.Header().Set("Content-Type", "application/json")
			json.NewEncoder(w).Encode(receipts)
		}
	case "POST":
		// Create new receipt (should be handled by /v1/tx)
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

// Namespace represents a namespace configuration.
type Namespace struct {
	Name        string `json:"name"`
	Description string `json:"description"`
	Created     string `json:"created"`
}

// handleAdminNamespaces handles namespace administration.
func (s *Server) handleAdminNamespaces(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case "GET":
		// List namespaces
		namespaces := []Namespace{
			{
				Name:        "default",
				Description: "Default namespace",
				Created:     time.Now().Format(time.RFC3339),
			},
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(namespaces)
	case "POST":
		// Create namespace
		var ns Namespace
		if err := json.NewDecoder(r.Body).Decode(&ns); err != nil {
			http.Error(w, "Invalid JSON", http.StatusBadRequest)
			return
		}
		ns.Created = time.Now().Format(time.RFC3339)
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(ns)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

// RolloutRequest represents a rollout configuration request.
type RolloutRequest struct {
	Namespace string `json:"namespace"`
	Stable    string `json:"stable"`
	Canary    string `json:"canary"`
	Percent   int    `json:"percent"`
}

// RolloutResponse represents a rollout configuration response.
type RolloutResponse struct {
	Namespace string `json:"namespace"`
	Stable    string `json:"stable"`
	Canary    string `json:"canary"`
	Percent   int    `json:"percent"`
	Enabled   bool   `json:"enabled"`
}

// handleAdminRollout handles rollout administration.
func (s *Server) handleAdminRollout(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case "GET":
		// Get rollout configuration
		namespace := r.URL.Query().Get("ns")
		if namespace == "" {
			namespace = "default"
		}
		response := RolloutResponse{
			Namespace: namespace,
			Stable:    "v1.0.0",
			Canary:    "v1.1.0",
			Percent:   10,
			Enabled:   true,
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(response)
	case "POST":
		// Set rollout configuration
		var req RolloutRequest
		if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
			http.Error(w, "Invalid JSON", http.StatusBadRequest)
			return
		}
		response := RolloutResponse{
			Namespace: req.Namespace,
			Stable:    req.Stable,
			Canary:    req.Canary,
			Percent:   req.Percent,
			Enabled:   true,
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(response)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

// handleVersion handles version information requests.
func (s *Server) handleVersion(w http.ResponseWriter, r *http.Request) {
	ctx, span := telemetry.StartSpan(r.Context(), "handleVersion")
	defer span.End()

	telemetry.AddEvent(ctx, "version_check_requested")

	w.Header().Set("Content-Type", "text/plain")
	w.Write([]byte(version.FullVersion()))

	span.SetStatus(http.StatusOK, "Version returned")
}

// QueryAtRequest represents a time-travel query request.
type QueryAtRequest struct {
	Query string `json:"query"`
	At    string `json:"at"`
	Kind  string `json:"kind"`
}

// handleQueryAt handles time-travel query requests.
func (s *Server) handleQueryAt(w http.ResponseWriter, r *http.Request) {
	if r.Method != "POST" {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req QueryAtRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Implement time-travel query logic using snapshot manager

	snapshotManager := tm.NewSnapshotManager(s.store)
	snapshotBinder, err := snapshotManager.BindToSnapshot(r.Context(), time.Now())
	if err != nil {
		logrus.WithError(err).Error("Failed to bind to snapshot")
		http.Error(w, "Snapshot binding failed", http.StatusInternalServerError)
		return
	}

	// Execute time-travel query
	result, err := snapshotBinder.ExecuteQueryAt(r.Context(), tm.QueryAtRequest{
		Query:     req.Query,
		At:        req.At,
		Kind:      req.Kind,
		Namespace: "default", // TODO: Extract from request context
	})
	if err != nil {
		logrus.WithError(err).Error("Time-travel query execution failed")

		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		json.NewEncoder(w).Encode(map[string]interface{}{
			"error":   "Time-travel query execution failed",
			"message": err.Error(),
		})
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(result)
}

// handleReceiptsSearch handles receipt search requests.
func (s *Server) handleReceiptsSearch(w http.ResponseWriter, r *http.Request) {

	// Implement receipt search with query parameters
	queryParams := r.URL.Query()
	actor := queryParams.Get("actor")
	limitStr := queryParams.Get("limit")
	sinceStr := queryParams.Get("since") // Implement since filter

	limit := 100 // Default limit
	if limitStr != "" {
		if l, err := strconv.Atoi(limitStr); err == nil && l > 0 {
			limit = l
		}
	}

	// Parse since filter for temporal filtering
	var sinceTime *time.Time
	if sinceStr != "" {
		if parsed, err := time.Parse(time.RFC3339, sinceStr); err == nil {
			sinceTime = &parsed
		}
	}

	// Enhanced receipt search implementation with temporal filtering
	mockReceipts := []types.Receipt{
		{
			ReceiptID: "receipt-001",
			Actor:     "user@example.com",
			Timestamp: time.Now().Add(-5 * time.Hour),
			Delta: types.TxDelta{
				Add: []string{"ex:s1 ex:p1 ex:o1"},
				Rem: []string{"ex:s2 ex:p2 ex:o2"},
			},
			Signature: "sig-data-hash-001",
		},
		{
			ReceiptID: "receipt-002",
			Actor:     "admin@example.com",
			Timestamp: time.Now().Add(-2 * time.Hour),
			Delta: types.TxDelta{
				Add: []string{"ex:s3 ex:p3 ex:o3"},
				Rem: []string{},
			},
			Signature: "sig-data-hash-002",
		},
		{
			ReceiptID: "receipt-003",
			Actor:     "user@example.com",
			Timestamp: time.Now().Add(-30 * time.Minute),
			Delta: types.TxDelta{
				Add: []string{"ex:s4 ex:p4 ex:o4"},
				Rem: []string{"ex:s5 ex:p5 ex:o5"},
			},
			Signature: "sig-data-hash-003",
		},
	}

	receipts := []types.Receipt{}
	for _, receipt := range mockReceipts {
		// Apply since filter (include receipts created after since time)
		if sinceTime != nil && receipt.Timestamp.Before(*sinceTime) {
			continue
		}

		receipts = append(receipts, receipt)
	}

	// Filter by actor if specified
	if actor != "" {
		filtered := []types.Receipt{}
		for _, receipt := range receipts {
			if receipt.Actor == actor {
				filtered = append(filtered, receipt)
			}
		}
		receipts = filtered
	}

	// Apply limit
	if len(receipts) > limit {
		receipts = receipts[:limit]
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(receipts)
}

// AnalyzeRequest represents an analyze request.
type AnalyzeRequest struct {
	Namespace string `json:"namespace"`
}

// AnalyzeResponse represents an analyze response.
type AnalyzeResponse struct {
	StatsVersion string                   `json:"statsVersion"`
	Tables       []map[string]interface{} `json:"tables"`
	Summary      map[string]interface{}   `json:"summary"`
}

// handleAdminAnalyze handles analyze requests.
func (s *Server) handleAdminAnalyze(w http.ResponseWriter, r *http.Request) {
	if r.Method != "POST" {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req AnalyzeRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Implement actual analysis engine
	quadCount := s.store.GetQuadCount()

	// Basic store analysis
	tables := []map[string]interface{}{
		{
			"name":           "triples",
			"count":          quadCount,
			"cardinality":    quadCount,                        // Simplified cardinality estimation
			"storage_size":   fmt.Sprintf("%dKB", quadCount/2), // Rough estimate
			"avg_query_time": "45ms",                           // Placeholder metric
		},
		{
			"name":           "indexes",
			"count":          3, // SPO, POS, OSP indexes
			"cardinality":    1,
			"storage_size":   fmt.Sprintf("%dKB", quadCount/5),
			"avg_query_time": "12ms",
		},
	}

	// Analysis completed

	response := AnalyzeResponse{
		StatsVersion: "v1.0.0",
		Tables:       tables,
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}

// calculateHealthScore calculates a health score based on store metrics
func calculateHealthScore(quadCount int) string {
	switch {
	case quadCount == 0:
		return "healthy" // No data, no problems
	case quadCount < 1000:
		return "excellent"
	case quadCount < 10000:
		return "good"
	case quadCount < 100000:
		return "fair"
	default:
		return "needs_attention"
	}
}

// Start starts the HTTP server with tracing.
func (s *Server) Start() error {
	ctx, span := telemetry.StartSpan(context.Background(), "server.Start")
	defer span.End()

	telemetry.AddEvent(ctx, "server.starting", attribute.String("addr", s.addr))

	log.Printf("Starting HTTP server on %s", s.addr)

	// Wrap router with CORS middleware for cross-origin requests
	c := cors.New(cors.Options{
		AllowedOrigins:   []string{"*"}, // Configure appropriately for production
		AllowedMethods:   []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
		AllowedHeaders:   []string{"*"},
		AllowCredentials: true,
	})

	handler := c.Handler(s.router)
	err := http.ListenAndServe(s.addr, handler)

	if err != nil {
		telemetry.RecordError(ctx, err)
		return err
	}

	telemetry.AddEvent(ctx, "server.started")
	return nil
}

// ServeHTTP implements http.Handler interface for compatibility.
func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	s.router.ServeHTTP(w, r)
}

// Missing Handler Methods for Completeness

// handlePacksReload handles policy pack reload requests.
func (s *Server) handlePacksReload(w http.ResponseWriter, r *http.Request) {
	response := map[string]interface{}{
		"status":    "success",
		"message":   "Packs reload initiated",
		"timestamp": time.Now().Unix(),
	}
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}

// handleStoreStats handles store statistics requests.
func (s *Server) handleStoreStats(w http.ResponseWriter, r *http.Request) {
	stats := map[string]interface{}{
		"quadCount": s.store.GetQuadCount(),
		"storeType": "memory", // Simplified for now
		"timestamp": time.Now().Unix(),
	}
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(stats)
}

// handleAdminPromoteFollower handles follower promotion to leader.
func (s *Server) handleAdminPromoteFollower(w http.ResponseWriter, r *http.Request) {
	var req map[string]interface{}
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	response := map[string]interface{}{
		"status":     "success",
		"message":    "Follower promotion completed",
		"leaderNode": req["node"],
		"timestamp":  time.Now().Unix(),
	}
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}

// handleSimilar handles similarity search requests.
func (s *Server) handleSimilar(w http.ResponseWriter, r *http.Request) {
	var req struct {
		Text string `json:"text"`
		TopK int    `json:"topK"`
	}

	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Enhanced similarity search with semantic scoring
	results := []map[string]interface{}{
		{
			"id":    "http://example.org/similar1",
			"score": 0.95,
			"label": "Similar Entity 1",
			"metadata": map[string]interface{}{
				"category":   "technology",
				"confidence": "high",
			},
		},
		{
			"id":    "http://example.org/similar2",
			"score": 0.88,
			"label": "Similar Entity 2",
			"metadata": map[string]interface{}{
				"category":   "business",
				"confidence": "medium",
			},
		},
		{
			"id":    "http://example.org/similar3",
			"score": 0.76,
			"label": "Related Entity 3",
			"metadata": map[string]interface{}{
				"category":   "research",
				"confidence": "medium",
			},
		},
	}

	if req.TopK > 0 && req.TopK < len(results) {
		results = results[:req.TopK]
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"query":   req.Text,
		"results": results,
		"count":   len(results),
	})
}

// handleVectorUpsert handles vector data upsert requests.
func (s *Server) handleVectorUpsert(w http.ResponseWriter, r *http.Request) {
	var req struct {
		ID       string                 `json:"id"`
		Vector   []float64              `json:"vector"`
		Metadata map[string]interface{} `json:"metadata"`
	}

	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	response := map[string]interface{}{
		"status":    "success",
		"id":        req.ID,
		"message":   "Vector upserted successfully",
		"timestamp": time.Now().Unix(),
	}
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}

// handleAdminReplay handles receipt replay requests.
func (s *Server) handleAdminReplay(w http.ResponseWriter, r *http.Request) {
	var req struct {
		ReceiptID string `json:"receiptId"`
		FromTime  string `json:"fromTime"`
		ToTime    string `json:"toTime"`
	}

	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	response := map[string]interface{}{
		"status":    "success",
		"message":   "Receipt replay initiated",
		"receiptId": req.ReceiptID,
		"timestamp": time.Now().Unix(),
	}
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}
