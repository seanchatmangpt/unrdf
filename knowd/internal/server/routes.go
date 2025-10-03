package server

import "net/http"

// AddRoutes defines the v1 API routes.
func AddRoutes(mux *http.ServeMux) {
	// Add routes for server functionality
	mux.HandleFunc("/healthz", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("ok"))
	})

	mux.HandleFunc("/version", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(`{"version": "v1.0.0", "commit": "abc123"}`))
	})
}
