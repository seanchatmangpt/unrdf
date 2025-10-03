package server

import (
	"net/http"

	"github.com/unrdf/knowd/internal/telemetry"
	"go.opentelemetry.io/contrib/instrumentation/net/http/otelhttp"
	"go.opentelemetry.io/otel/attribute"
)

// Middleware for the server with OpenTelemetry instrumentation.
func Middleware(h http.Handler) http.Handler {
	// Wrap with OpenTelemetry HTTP instrumentation
	otelHandler := otelhttp.NewHandler(h, "http.server",
		otelhttp.WithSpanNameFormatter(func(operation string, r *http.Request) string {
			return "http." + r.Method + "." + r.URL.Path
		}),
		otelhttp.WithMessageEvents(otelhttp.ReadEvents, otelhttp.WriteEvents),
	)

	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		ctx, span := telemetry.StartSpan(r.Context(), "http.request")
		defer span.End()

		// Add request attributes
		span.SetAttributes(
			attribute.String("http.method", r.Method),
			attribute.String("http.url", r.URL.String()),
			attribute.String("http.user_agent", r.UserAgent()),
			attribute.String("http.remote_addr", r.RemoteAddr),
		)

		// Add span to request context
		r = r.WithContext(ctx)

		// Call the wrapped handler
		otelHandler.ServeHTTP(w, r)
	})
}

// TracingMiddleware provides additional tracing for specific operations.
func TracingMiddleware(h http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Extract span context from headers if present
		ctx := r.Context()

		// Call the wrapped handler
		h.ServeHTTP(w, r.WithContext(ctx))
	})
}
