// Command knowd implements the main entrypoint for the knowd server.
package main

import (
	"context"
	"flag"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/unrdf/knowd/internal/server"
	"github.com/unrdf/knowd/internal/version"
)

// Configuration holds command-line flags and configuration for v1.0.0 GA.
type Configuration struct {
	addr    string
	dataDir string
	coreURL string
	store   string
}

func main() {
	// Parse command line flags
	var config Configuration

	flag.StringVar(&config.addr, "addr", ":8090", "HTTP server address")
	flag.StringVar(&config.dataDir, "data-dir", "./data", "Data directory path")
	flag.StringVar(&config.coreURL, "core-url", "native://", "Core URL")
	flag.StringVar(&config.store, "store", "mem", "Store type (mem|disk)")

	// JIRA command flags
	jiraValidate := flag.Bool("jira-validate", false, "Validate JIRA feature parity")
	jiraReport := flag.Bool("jira-report", false, "Generate JIRA feature parity report")
	jiraUpdate := flag.String("jira-update", "", "Update JIRA ticket status (format: TICKET_ID:STATUS:NOTES)")

	// Benchmark command flags
	benchmarkRun := flag.Bool("benchmark", false, "Run comprehensive performance benchmarks")
	benchmarkReport := flag.Bool("benchmark-report", false, "Generate benchmark report")
	benchmarkCompare := flag.String("benchmark-compare", "", "Compare with baseline file")

	flag.Parse()

	// Apply environment variable overrides
	if val := os.Getenv("KNOWD_ADDR"); val != "" {
		config.addr = val
	}
	if val := os.Getenv("KNOWD_DATA_DIR"); val != "" {
		config.dataDir = val
	}
	if val := os.Getenv("KNOWD_CORE_URL"); val != "" {
		config.coreURL = val
	}
	if val := os.Getenv("KNOWD_STORE"); val != "" {
		config.store = val
	}

	// Handle benchmark and JIRA commands if specified
	if *benchmarkRun || *benchmarkReport || *benchmarkCompare != "" {
		handleBenchmarkCommand(benchmarkRun, benchmarkReport, nil, *benchmarkCompare)
		return
	}

	// Handle JIRA commands if specified
	if *jiraValidate || *jiraReport || *jiraUpdate != "" {
		log.Printf("JIRA commands not yet implemented")
		return
	}

	// Log startup information
	log.Printf("Starting %s", version.BuildInfo())
	log.Printf("Configuration:")
	log.Printf("  Address: %s", config.addr)
	log.Printf("  Data Directory: %s", config.dataDir)
	log.Printf("  Core URL: %s", config.coreURL)
	log.Printf("  Store: %s", config.store)

	// Create HTTP server
	httpServer := server.New(config.addr, config.dataDir, config.coreURL, config.store)

	// Start server in a goroutine
	serverErr := make(chan error, 1)
	go func() {
		if err := httpServer.Start(); err != nil && err != http.ErrServerClosed {
			serverErr <- err
		}
	}()

	// Set up graceful shutdown
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	// Wait for shutdown signal or server error
	select {
	case sig := <-sigChan:
		log.Printf("Received signal %s, shutting down gracefully...", sig)

	case err := <-serverErr:
		log.Fatalf("Server error: %v", err)
	}

	// Graceful shutdown with timeout
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	// Create a custom server for graceful shutdown
	httpServerInstance := &http.Server{
		Addr:           config.addr,
		Handler:        httpServer,
		ReadTimeout:    10 * time.Second,
		WriteTimeout:   10 * time.Second,
		MaxHeaderBytes: 1 << 20,
	}

	if err := httpServerInstance.Shutdown(ctx); err != nil {
		log.Printf("Error during shutdown: %v", err)
	}

	log.Println("knowd server stopped")
}
