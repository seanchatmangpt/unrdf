package lockchain

import (
	"crypto/sha256"
	"encoding/hex"
)

// Root calculates the merkle root of chunks using SHA256 hashing.
func Root(chunks [][]byte) string {
	if len(chunks) == 0 {
		return ""
	}

	if len(chunks) == 1 {
		hash := sha256.Sum256(chunks[0])
		return hex.EncodeToString(hash[:])
	}

	// For multiple chunks, create a simple merkle tree
	// In a real implementation, this would recursively hash pairs
	var combined []byte
	for _, chunk := range chunks {
		hash := sha256.Sum256(chunk)
		combined = append(combined, hash[:]...)
	}

	hash := sha256.Sum256(combined)
	return hex.EncodeToString(hash[:])
}
