package telemetry

import (
	"context"
	"fmt"
	"log"
	"os"
	"strconv"
	"sync"

	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/attribute"
	"go.opentelemetry.io/otel/codes"
	"go.opentelemetry.io/otel/metric"
	"go.opentelemetry.io/otel/metric/noop"
	sdkmetric "go.opentelemetry.io/otel/sdk/metric"
	"go.opentelemetry.io/otel/sdk/resource"
	sdktrace "go.opentelemetry.io/otel/sdk/trace"
	semconv "go.opentelemetry.io/otel/semconv/v1.24.0"
	"go.opentelemetry.io/otel/trace"
)

// Weaver provides a global OpenTelemetry setup for tracing, metrics, and logging
type Weaver struct {
	tracerProvider *sdktrace.TracerProvider
	meterProvider  *sdkmetric.MeterProvider
	shutdownFuncs  []func(context.Context) error
	mu             sync.RWMutex
	isInitialized  bool
}

// Global weaver instance
var globalWeaver *Weaver
var globalWeaverOnce sync.Once

// Config holds configuration for the OpenTelemetry weaver
type Config struct {
	ServiceName    string
	ServiceVersion string
	Environment    string

	// Tracing configuration
	TraceEnabled bool

	// Metrics configuration
	MetricsEnabled bool

	// Sampling configuration
	SampleRatio float64 // 0.0 to 1.0

	// Resource attributes
	ResourceAttributes map[string]string
}

// DefaultConfig returns a default configuration
func DefaultConfig() *Config {
	hostname, _ := os.Hostname()

	return &Config{
		ServiceName:    "knowd",
		ServiceVersion: "1.0.0",
		Environment:    getEnvOrDefault("KNOWD_ENVIRONMENT", "development"),
		TraceEnabled:   getEnvOrDefault("KNOWD_OTEL_TRACING", "true") == "true",
		MetricsEnabled: getEnvOrDefault("KNOWD_OTEL_METRICS", "true") == "true",
		SampleRatio:    getEnvFloatOrDefault("KNOWD_OTEL_SAMPLE_RATIO", 1.0),
		ResourceAttributes: map[string]string{
			"service.name":    "knowd",
			"service.version": "1.0.0",
			"host.name":       hostname,
		},
	}
}

// GetGlobalWeaver returns the global OpenTelemetry weaver instance
func GetGlobalWeaver() *Weaver {
	globalWeaverOnce.Do(func() {
		globalWeaver = NewWeaver(DefaultConfig())
	})
	return globalWeaver
}

// NewWeaver creates a new OpenTelemetry weaver with the given configuration
func NewWeaver(config *Config) *Weaver {
	w := &Weaver{
		shutdownFuncs: make([]func(context.Context) error, 0),
	}

	// Set up resource
	res, err := resource.New(context.Background(),
		resource.WithAttributes(
			semconv.ServiceName(config.ServiceName),
			semconv.ServiceVersion(config.ServiceVersion),
			semconv.DeploymentEnvironment(config.Environment),
		),
		resource.WithHostID(),
		resource.WithOSType(),
	)
	if err != nil {
		log.Printf("Failed to create resource: %v", err)
		return w
	}

	// Set up tracing
	if config.TraceEnabled {
		if err := w.setupTracing(config, res); err != nil {
			log.Printf("Failed to setup tracing: %v", err)
		}
	}

	// Set up metrics
	if config.MetricsEnabled {
		if err := w.setupMetrics(config, res); err != nil {
			log.Printf("Failed to setup metrics: %v", err)
		}
	}

	w.isInitialized = true
	return w
}

// setupTracing configures OpenTelemetry tracing
func (w *Weaver) setupTracing(config *Config, res *resource.Resource) error {
	// Create tracer provider
	tracerProvider := sdktrace.NewTracerProvider(
		sdktrace.WithResource(res),
	)

	// Set as global tracer provider
	otel.SetTracerProvider(tracerProvider)

	w.tracerProvider = tracerProvider
	w.shutdownFuncs = append(w.shutdownFuncs, func(ctx context.Context) error {
		return tracerProvider.Shutdown(ctx)
	})

	return nil
}

// setupMetrics configures OpenTelemetry metrics
func (w *Weaver) setupMetrics(config *Config, res *resource.Resource) error {
	// Create meter provider
	meterProvider := sdkmetric.NewMeterProvider(
		sdkmetric.WithResource(res),
	)

	otel.SetMeterProvider(meterProvider)

	w.meterProvider = meterProvider
	w.shutdownFuncs = append(w.shutdownFuncs, func(ctx context.Context) error {
		return meterProvider.Shutdown(ctx)
	})

	return nil
}

// Tracer returns a tracer for the given name
func (w *Weaver) Tracer(name string) trace.Tracer {
	if !w.isInitialized {
		return trace.NewNoopTracerProvider().Tracer(name)
	}
	return otel.Tracer(name)
}

// Meter returns a meter for the given name
func (w *Weaver) Meter(name string) metric.Meter {
	if !w.isInitialized {
		return noop.NewMeterProvider().Meter(name)
	}
	return otel.Meter(name)
}

// StartSpan starts a new span with the given name and options
func (w *Weaver) StartSpan(ctx context.Context, name string, opts ...trace.SpanStartOption) (context.Context, trace.Span) {
	tracer := w.Tracer("knowd")
	return tracer.Start(ctx, name, opts...)
}

// RecordError records an error on the current span
func (w *Weaver) RecordError(ctx context.Context, err error, opts ...trace.EventOption) {
	span := trace.SpanFromContext(ctx)
	if span.IsRecording() {
		span.RecordError(err, opts...)
		span.SetStatus(codes.Error, err.Error())
	}
}

// AddEvent adds an event to the current span
func (w *Weaver) AddEvent(ctx context.Context, name string, attrs ...attribute.KeyValue) {
	span := trace.SpanFromContext(ctx)
	if span.IsRecording() {
		span.AddEvent(name, trace.WithAttributes(attrs...))
	}
}

// Shutdown gracefully shuts down the OpenTelemetry weaver
func (w *Weaver) Shutdown(ctx context.Context) error {
	w.mu.Lock()
	defer w.mu.Unlock()

	var errs []error
	for _, shutdown := range w.shutdownFuncs {
		if err := shutdown(ctx); err != nil {
			errs = append(errs, err)
		}
	}

	if len(errs) > 0 {
		return fmt.Errorf("shutdown errors: %v", errs)
	}

	w.isInitialized = false
	return nil
}

// Helper function to get environment variable or default
func getEnvOrDefault(key, defaultValue string) string {
	if value := os.Getenv(key); value != "" {
		return value
	}
	return defaultValue
}

// Helper function to get environment variable as float
func getEnvFloatOrDefault(key string, defaultValue float64) float64 {
	if value := os.Getenv(key); value != "" {
		if f, err := strconv.ParseFloat(value, 64); err == nil {
			return f
		}
	}
	return defaultValue
}

// Global convenience functions

// StartSpan starts a new span using the global weaver
func StartSpan(ctx context.Context, name string, opts ...trace.SpanStartOption) (context.Context, trace.Span) {
	return GetGlobalWeaver().StartSpan(ctx, name, opts...)
}

// RecordError records an error on the current span using the global weaver
func RecordError(ctx context.Context, err error, opts ...trace.EventOption) {
	GetGlobalWeaver().RecordError(ctx, err, opts...)
}

// AddEvent adds an event to the current span using the global weaver
func AddEvent(ctx context.Context, name string, attrs ...attribute.KeyValue) {
	GetGlobalWeaver().AddEvent(ctx, name, attrs...)
}

// Tracer returns a tracer using the global weaver
func Tracer(name string) trace.Tracer {
	return GetGlobalWeaver().Tracer(name)
}

// Meter returns a meter using the global weaver
func Meter(name string) metric.Meter {
	return GetGlobalWeaver().Meter(name)
}

// Shutdown gracefully shuts down the global OpenTelemetry weaver
func Shutdown(ctx context.Context) error {
	return GetGlobalWeaver().Shutdown(ctx)
}
