// Package server provides HTTP server and API handlers for knowd.
package server

import (
	"encoding/json"
	"log"
	"net/http"
	"time"

	"github.com/unrdf/knowd/internal/lockchain"
	"github.com/unrdf/knowd/internal/sparql"
	"github.com/unrdf/knowd/internal/store"
	"github.com/unrdf/knowd/internal/version"
)

// Server represents the HTTP server instance.
type Server struct {
	addr      string
	dataDir   string
	coreURL   string
	mux       *http.ServeMux
	executor  *sparql.Executor
	store     store.Interface
	lockchain *lockchain.Lockchain
}

// New creates a new HTTP server instance.
func New(addr, dataDir, coreURL string) *Server {
	// Initialize in-memory store for now
	storeConfig := store.Config{MaxQuads: 1000000}
	storeInstance, err := store.NewMemoryStore(storeConfig)
	if err != nil {
		log.Fatalf("Failed to create memory store: %v", err)
	}

	// Initialize plan cache
	cacheConfig := sparql.CacheConfig{Capacity: 256}
	cache := sparql.NewPlanCache(cacheConfig)

	// Initialize SPARQL executor
	executor := sparql.NewExecutor(cache)

	// Initialize lockchain
	lockchainConfig := lockchain.Config{
		ReceiptsDir: "./receipts",
	}
	lc, err := lockchain.New(lockchainConfig)
	if err != nil {
		log.Fatalf("Failed to create lockchain: %v", err)
	}

	s := &Server{
		addr:      addr,
		dataDir:   dataDir,
		coreURL:   coreURL,
		mux:       http.NewServeMux(),
		executor:  executor,
		store:     storeInstance,
		lockchain: lc,
	}
	s.setupRoutes()
	return s
}

// setupRoutes configures HTTP routes for the server.
func (s *Server) setupRoutes() {
	s.mux.HandleFunc("/healthz", s.handleHealth)
	s.mux.HandleFunc("/v1/tx", s.handleTransaction)
	s.mux.HandleFunc("/v1/query", s.handleQuery)
	s.mux.HandleFunc("/v1/query/stream", s.handleQueryStream)
	s.mux.HandleFunc("/v1/query/at", s.handleQueryAt)
	s.mux.HandleFunc("/v1/validate", s.handleValidate)
	s.mux.HandleFunc("/v1/receipts", s.handleReceipts)
	s.mux.HandleFunc("/v1/receipts/search", s.handleReceiptsSearch)
	s.mux.HandleFunc("/v1/hooks/evaluate", s.handleHookEvaluate)

	// Admin routes
	s.mux.HandleFunc("/v1/admin/namespaces", s.handleAdminNamespaces)
	s.mux.HandleFunc("/v1/admin/rollout", s.handleAdminRollout)
	s.mux.HandleFunc("/v1/admin/analyze", s.handleAdminAnalyze)

	// Add version endpoint
	s.mux.HandleFunc("/version", s.handleVersion)
}

// handleHealth handles health check requests.
func (s *Server) handleHealth(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	w.WriteHeader(http.StatusOK)
	w.Write([]byte("ok"))
}

// TxRequest represents a transaction request structure.
type TxRequest struct {
	Delta map[string]interface{} `json:"delta"`
	Actor string                 `json:"actor"`
}

// TxResponse represents a transaction response structure.
type TxResponse struct {
	ReceiptID  string                 `json:"receiptId"`
	MerkleRoot string                 `json:"merkleRoot"`
	Delta      map[string]interface{} `json:"delta"`
}

// handleTransaction handles transaction requests.
func (s *Server) handleTransaction(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req TxRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Create signed receipt using lockchain
	receipt, err := s.lockchain.WriteReceipt(req.Actor, req.Delta)
	if err != nil {
		log.Printf("Failed to create receipt: %v", err)
		http.Error(w, "Failed to process transaction", http.StatusInternalServerError)
		return
	}

	response := TxResponse{
		ReceiptID:  receipt.ID,
		MerkleRoot: receipt.MerkleRoot,
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
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req QueryRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
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
		http.Error(w, "Invalid query kind", http.StatusBadRequest)
		return
	}

	// Execute the query using SPARQL executor
	result, err := s.executor.Query(r.Context(), req.Query, s.store)
	if err != nil {
		http.Error(w, "Query execution failed", http.StatusInternalServerError)
		return
	}

	// Convert to expected response format
	resp := QueryResponse{
		JSON: result,
	}

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
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req HookRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Return static stub response
	resp := HookResponse{
		Fired:  true,
		Result: nil,
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
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req ValidateRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// TODO: Implement actual SHACL validation
	// For now, return a mock response
	response := ValidateResponse{
		Conforms:   true,
		Violations: []ValidationViolation{},
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}

// handleQueryStream handles streaming query requests.
func (s *Server) handleQueryStream(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req QueryRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Execute the query
	result, err := s.executor.Query(r.Context(), req.Query, s.store)
	if err != nil {
		log.Printf("Query execution error: %v", err)
		http.Error(w, "Query execution failed", http.StatusInternalServerError)
		return
	}

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
	case http.MethodGet:
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
	case http.MethodPost:
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
	case http.MethodGet:
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
	case http.MethodPost:
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
	case http.MethodGet:
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
	case http.MethodPost:
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
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	w.Header().Set("Content-Type", "text/plain")
	w.Write([]byte(version.FullVersion()))
}

// QueryAtRequest represents a time-travel query request.
type QueryAtRequest struct {
	Query string `json:"query"`
	At    string `json:"at"`
	Kind  string `json:"kind"`
}

// handleQueryAt handles time-travel query requests.
func (s *Server) handleQueryAt(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req QueryAtRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// TODO: Implement time-travel query logic
	// For now, execute as regular query
	result, err := s.executor.Query(r.Context(), req.Query, s.store)
	if err != nil {
		log.Printf("Query execution error: %v", err)
		http.Error(w, "Query execution failed", http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(result)
}

// handleReceiptsSearch handles receipt search requests.
func (s *Server) handleReceiptsSearch(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Parse query parameters for search
	actor := r.URL.Query().Get("actor")
	since := r.URL.Query().Get("since")
	until := r.URL.Query().Get("until")

	// TODO: Implement actual receipt search
	// For now, return empty results
	receipts := []Receipt{}

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
}

// handleAdminAnalyze handles analyze requests.
func (s *Server) handleAdminAnalyze(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req AnalyzeRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// TODO: Implement actual analysis
	// For now, return mock response
	response := AnalyzeResponse{
		StatsVersion: "v1.0.0",
		Tables: []map[string]interface{}{
			{
				"name":        "triples",
				"count":       0,
				"cardinality": 0,
			},
		},
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}

// Start starts the HTTP server.
func (s *Server) Start() error {
	return http.ListenAndServe(s.addr, s.mux)
}

// ServeHTTP implements http.Handler interface for compatibility.
func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	s.mux.ServeHTTP(w, r)
}
