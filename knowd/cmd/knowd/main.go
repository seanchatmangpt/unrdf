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
}

func main() {
	// Parse command line flags
	var config Configuration

	flag.StringVar(&config.addr, "addr", ":8090", "HTTP server address")
	flag.StringVar(&config.dataDir, "data-dir", "./data", "Data directory path")
	flag.StringVar(&config.coreURL, "core-url", "native://", "Core URL")

	// Check for environment variable overrides if flags are not explicitly set
	if config.addr == ":8090" && os.Getenv("KNOWD_ADDR") != "" {
		config.addr = os.Getenv("KNOWD_ADDR")
	}
	if config.dataDir == "./data" && os.Getenv("KNOWD_DATA_DIR") != "" {
		config.dataDir = os.Getenv("KNOWD_DATA_DIR")
	}
	if config.coreURL == "native://" && os.Getenv("KNOWD_CORE_URL") != "" {
		config.coreURL = os.Getenv("KNOWD_CORE_URL")
	}

	flag.Parse()

	// Log startup information
	log.Printf("Starting %s", version.BuildInfo())
	log.Printf("Configuration:")
	log.Printf("  Address: %s", config.addr)
	log.Printf("  Data Directory: %s", config.dataDir)
	log.Printf("  Core URL: %s", config.coreURL)

	// Create HTTP server with GA configuration
	serverConfig := &server.Configuration{
		Addr:    config.addr,
		DataDir: config.dataDir,
	}
	httpServer := server.New(serverConfig)

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
