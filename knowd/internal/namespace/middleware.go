package namespace

import "net/http"

// Middleware is the namespace middleware.
func Middleware(h http.Handler) http.Handler {
	return h
}
