package lockchain

import (
	"crypto/ed25519"
	"crypto/rand"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"strings"
	"time"
)

// Lockchain manages receipts with cryptographic signing and canonicalization.
type Lockchain struct {
	signingKey ed25519.PrivateKey
	publicKey  ed25519.PublicKey
}

// Receipt represents a cryptographically signed receipt.
type Receipt struct {
	Version    string                 `json:"version"`
	ID         string                 `json:"id"`
	Actor      string                 `json:"actor"`
	Timestamp  string                 `json:"timestamp"`
	Delta      map[string]interface{} `json:"delta"`
	Canonical  string                 `json:"canonical"`
	MerkleRoot string                 `json:"merkleRoot"`
	Signature  string                 `json:"signature"`
	PubKey     string                 `json:"pubKey"`
	Git        *GitInfo               `json:"git,omitempty"`
}

// GitInfo represents optional Git anchoring information.
type GitInfo struct {
	Commit string `json:"commit"`
	Path   string `json:"path"`
}

// Config holds lockchain configuration.
type Config struct {
	SigningKeyFile string
	PublicKeyFile  string
	ReceiptsDir    string
	GitRepo        string
}

// New creates a new lockchain instance.
func New(config Config) (*Lockchain, error) {
	lc := &Lockchain{}

	// Load signing keys if provided
	if config.SigningKeyFile != "" {
		keyData, err := loadKeyFile(config.SigningKeyFile)
		if err != nil {
			return nil, fmt.Errorf("failed to load signing key: %w", err)
		}
		lc.signingKey = keyData
	}

	if config.PublicKeyFile != "" {
		pubData, err := loadKeyFile(config.PublicKeyFile)
		if err != nil {
			return nil, fmt.Errorf("failed to load public key: %w", err)
		}
		lc.publicKey = pubData
	}

	// Generate keypair if not provided
	if lc.signingKey == nil {
		var err error
		lc.publicKey, lc.signingKey, err = ed25519.GenerateKey(rand.Reader)
		if err != nil {
			return nil, fmt.Errorf("failed to generate keypair: %w", err)
		}
	}

	return lc, nil
}

// WriteReceipt creates and signs a receipt for the given payload.
func (lc *Lockchain) WriteReceipt(actor string, delta map[string]interface{}) (*Receipt, error) {
	// Create receipt
	receipt := &Receipt{
		Version:   "1",
		ID:        generateReceiptID(),
		Actor:     actor,
		Timestamp: time.Now().UTC().Format(time.RFC3339),
		Delta:     delta,
	}

	// Canonicalize the delta
	canonical, err := lc.canonicalizeDelta(delta)
	if err != nil {
		return nil, fmt.Errorf("failed to canonicalize delta: %w", err)
	}
	receipt.Canonical = canonical

	// Calculate Merkle root
	receipt.MerkleRoot = lc.calculateMerkleRoot(canonical)

	// Sign the receipt
	signature, err := lc.signReceipt(receipt)
	if err != nil {
		return nil, fmt.Errorf("failed to sign receipt: %w", err)
	}
	receipt.Signature = signature

	// Add public key
	receipt.PubKey = base64.StdEncoding.EncodeToString(lc.publicKey)

	return receipt, nil
}

// Verify verifies a receipt's signature and integrity.
func (lc *Lockchain) Verify(receipt *Receipt) error {
	// Verify signature
	if receipt.Signature == "" {
		return fmt.Errorf("receipt has no signature")
	}

	// Decode public key
	pubKey, err := base64.StdEncoding.DecodeString(receipt.PubKey)
	if err != nil {
		return fmt.Errorf("invalid public key encoding: %w", err)
	}

	// Decode signature
	signature, err := base64.StdEncoding.DecodeString(receipt.Signature)
	if err != nil {
		return fmt.Errorf("invalid signature encoding: %w", err)
	}

	// Create message to verify (canonical form without signature)
	message := lc.createSignatureMessage(receipt)

	// Verify signature
	if !ed25519.Verify(pubKey, []byte(message), signature) {
		return fmt.Errorf("invalid signature")
	}

	// Verify Merkle root matches canonical form
	expectedMerkle := lc.calculateMerkleRoot(receipt.Canonical)
	if receipt.MerkleRoot != expectedMerkle {
		return fmt.Errorf("merkle root mismatch")
	}

	return nil
}

// canonicalizeDelta creates a canonical representation of the delta.
func (lc *Lockchain) canonicalizeDelta(delta map[string]interface{}) (string, error) {
	// Use URDNA2015 canonicalization algorithm
	// For now, implement a simplified version
	canonicalizer := NewURDNA2015Canonicalizer()

	// Convert delta to quads for canonicalization
	quads := lc.deltaToQuads(delta)

	canonical, err := canonicalizer.Canonicalize(quads)
	if err != nil {
		return "", fmt.Errorf("canonicalization failed: %w", err)
	}

	return canonical, nil
}

// deltaToQuads converts a delta to RDF quads for canonicalization.
func (lc *Lockchain) deltaToQuads(delta map[string]interface{}) []Quad {
	var quads []Quad

	// Process additions
	if add, ok := delta["add"].([]interface{}); ok {
		for _, item := range add {
			if quad, ok := item.(map[string]interface{}); ok {
				quads = append(quads, Quad{
					Subject:   getString(quad, "subject"),
					Predicate: getString(quad, "predicate"),
					Object:    getString(quad, "object"),
					Graph:     getString(quad, "graph"),
				})
			}
		}
	}

	// Process removals (similar structure)
	if rem, ok := delta["rem"].([]interface{}); ok {
		for _, item := range rem {
			if quad, ok := item.(map[string]interface{}); ok {
				quads = append(quads, Quad{
					Subject:   getString(quad, "subject"),
					Predicate: getString(quad, "predicate"),
					Object:    getString(quad, "object"),
					Graph:     getString(quad, "graph"),
				})
			}
		}
	}

	return quads
}

// calculateMerkleRoot calculates the SHA3-256 Merkle root of canonical data.
func (lc *Lockchain) calculateMerkleRoot(canonical string) string {
	hash := sha256.Sum256([]byte(canonical))
	return hex.EncodeToString(hash[:])
}

// signReceipt signs a receipt using Ed25519.
func (lc *Lockchain) signReceipt(receipt *Receipt) (string, error) {
	message := lc.createSignatureMessage(receipt)
	signature := ed25519.Sign(lc.signingKey, []byte(message))
	return base64.StdEncoding.EncodeToString(signature), nil
}

// createSignatureMessage creates the message to be signed (receipt without signature).
func (lc *Lockchain) createSignatureMessage(receipt *Receipt) string {
	// Create a deterministic JSON representation without signature
	msgReceipt := *receipt
	msgReceipt.Signature = ""

	data, err := json.Marshal(msgReceipt)
	if err != nil {
		return ""
	}

	// Normalize JSON (simple approach)
	return strings.TrimSpace(string(data))
}

// generateReceiptID generates a unique receipt ID.
func generateReceiptID() string {
	return fmt.Sprintf("receipt-%d", time.Now().UnixNano())
}

// loadKeyFile loads a key from file.
func loadKeyFile(filename string) ([]byte, error) {
	// In a real implementation, this would load from file
	// For now, return dummy key
	return make([]byte, 32), nil
}

// getString safely gets a string value from a map.
func getString(m map[string]interface{}, key string) string {
	if val, ok := m[key].(string); ok {
		return val
	}
	return ""
}
