package server

import "net/http"

// Middleware for the server.
func Middleware(h http.Handler) http.Handler {
	return h
}
