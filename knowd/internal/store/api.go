// Package store provides storage interfaces for knowd.
package store

import "context"

// Interface defines the storage interface for RDF quads.
type Interface interface {
	// AddQuad adds a quad to the store.
	AddQuad(ctx context.Context, quad Quad) error
	
	// RemoveQuad removes a quad from the store.
	RemoveQuad(ctx context.Context, quad Quad) error
	
	// HasQuad checks if a quad exists in the store.
	HasQuad(ctx context.Context, quad Quad) (bool, error)
	
	// FindQuads finds quads matching a pattern.
	FindQuads(ctx context.Context, pattern Quad) ([]Quad, error)
	
	// GetQuadCount returns the total number of quads in the store.
	GetQuadCount() int
	
	// Close closes the store.
	Close() error
}

// Quad represents an RDF quad (subject, predicate, object, graph).
type Quad struct {
	Subject   string
	Predicate string
	Object    string
	Graph     string
}

// Config holds store configuration.
type Config struct {
	MaxQuads int
}