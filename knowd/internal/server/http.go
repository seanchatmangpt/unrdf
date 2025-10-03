// Package server provides HTTP server and API handlers for knowd.
package server

import (
	"encoding/json"
	"log"
	"net/http"

	"github.com/unrdf/knowd/internal/version"
)

// Server represents the HTTP server instance.
type Server struct {
	addr    string
	dataDir string
	coreURL string
	mux     *http.ServeMux
}

// New creates a new HTTP server instance.
func New(addr, dataDir, coreURL string) *Server {
	s := &Server{
		addr:    addr,
		dataDir: dataDir,
		coreURL: coreURL,
		mux:     http.NewServeMux(),
	}
	s.setupRoutes()
	return s
}

// setupRoutes configures HTTP routes for the server.
func (s *Server) setupRoutes() {
	s.mux.HandleFunc("/healthz", s.handleHealth)
	s.mux.HandleFunc("/v1/tx", s.handleTransaction)
	s.mux.HandleFunc("/v1/query", s.handleQuery)
	s.mux.HandleFunc("/v1/hooks/evaluate", s.handleHookEvaluate)

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

// handleTransaction handles transaction requests (placeholder implementation).
func (s *Server) handleTransaction(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Read request body
	var body map[string]interface{}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Echo the request body (placeholder for future transaction pipeline)
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(body)
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

	// Return static stub response
	resp := QueryResponse{
		JSON: []interface{}{},
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

// handleVersion handles version information requests.
func (s *Server) handleVersion(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	w.Header().Set("Content-Type", "text/plain")
	w.Write([]byte(version.FullVersion()))
}

// Start starts the HTTP server.
func (s *Server) Start() error {
	log.Printf("Starting knowd server on %s", s.addr)
	log.Printf("Data directory: %s", s.dataDir)
	log.Printf("Core URL: %s", s.coreURL)

	return http.ListenAndServe(s.addr, s.mux)
}

// ServeHTTP implements http.Handler interface for compatibility.
func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	s.mux.ServeHTTP(w, r)
}
