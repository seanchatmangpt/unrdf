package types

import "time"

// Receipt is the JSON type for a receipt.
type Receipt struct {
	Version    string    `json:"version"`
	Actor      string    `json:"actor"`
	Timestamp  time.Time `json:"timestamp"` // RFC3339
	Delta      TxDelta   `json:"delta"`
	Canonical  string    `json:"canonical"` // base64
	MerkleRoot string    `json:"merkleRoot"`
	Signature  string    `json:"signature"` // base64
	PubKey     string    `json:"pubKey"`    // base64
	Git        *GitInfo  `json:"git,omitempty"`
	ReceiptID  string    `json:"receiptId"` // For internal use
}

// TxDelta represents the transaction delta.
type TxDelta struct {
	Add []string `json:"add"`
	Rem []string `json:"rem"`
}

// GitInfo represents Git commit information.
type GitInfo struct {
	Commit string `json:"commit"`
	Path   string `json:"path"`
}
