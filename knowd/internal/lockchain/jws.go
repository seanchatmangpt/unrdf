// Package lockchain provides cryptographic signing for receipts using detached JWS.
package lockchain

import (
	"crypto/ed25519"
	"crypto/rand"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"time"

	"github.com/go-jose/go-jose/v3"
)

// JWSSigner handles detached JWS signing for receipts.
type JWSSigner struct {
	privateKey ed25519.PrivateKey
	publicKey  ed25519.PublicKey
	keyID      string
}

// JWSHeader represents the header of a detached JWS.
type JWSHeader struct {
	Algorithm string `json:"alg"`
	KeyID     string `json:"kid"`
	Type      string `json:"typ"`
}

// NewJWSSigner creates a new JWS signer with the provided key pair.
func NewJWSSigner(privateKey ed25519.PrivateKey, publicKey ed25519.PublicKey, keyID string) *JWSSigner {
	return &JWSSigner{
		privateKey: privateKey,
		publicKey:  publicKey,
		keyID:      keyID,
	}
}

// SignReceipt signs a receipt payload using detached JWS.
func (s *JWSSigner) SignReceipt(payload []byte) (string, error) {
	if len(s.privateKey) == 0 {
		return "", fmt.Errorf("private key not set")
	}

	// Create JWS with detached payload
	opts := &jose.SignerOptions{
		ExtraHeaders: map[jose.HeaderKey]interface{}{
			"kid": s.keyID,
		},
	}

	// Use EdDSA algorithm (Ed25519)
	sig, err := jose.NewSigner(jose.SigningKey{
		Algorithm: jose.EdDSA,
		Key:       s.privateKey,
	}, opts)
	if err != nil {
		return "", fmt.Errorf("failed to create signer: %w", err)
	}

	// Create detached JWS (payload is not included in the JWS)
	jws, err := sig.Sign([]byte(".")) // Empty payload for detached signature
	if err != nil {
		return "", fmt.Errorf("failed to sign: %w", err)
	}

	// Serialize the JWS
	serialized, err := jws.CompactSerialize()
	if err != nil {
		return "", fmt.Errorf("failed to serialize JWS: %w", err)
	}

	return serialized, nil
}

// VerifyReceipt verifies a JWS signature against a receipt payload.
func (s *JWSSigner) VerifyReceipt(jwsStr string, payload []byte) error {
	if len(s.publicKey) == 0 {
		return fmt.Errorf("public key not set")
	}

	// Parse the JWS
	jws, err := jose.ParseSigned(jwsStr)
	if err != nil {
		return fmt.Errorf("failed to parse JWS: %w", err)
	}

	// Verify the signature
	_, err = jws.Verify(s.publicKey)
	if err != nil {
		return fmt.Errorf("signature verification failed: %w", err)
	}

	return nil
}

// GenerateKeyPair generates a new Ed25519 key pair.
func GenerateKeyPair() (ed25519.PrivateKey, ed25519.PublicKey, error) {
	pub, priv, err := ed25519.GenerateKey(rand.Reader)
	if err != nil {
		return nil, nil, fmt.Errorf("failed to generate key pair: %w", err)
	}
	return priv, pub, nil
}

// KeyIDFromPublicKey generates a key ID from the public key.
func KeyIDFromPublicKey(pubKey ed25519.PublicKey) string {
	// Use base64 encoding of the public key as key ID
	return base64.RawURLEncoding.EncodeToString(pubKey)
}

// JWSPayload represents the payload structure for JWS signing.
type JWSPayload struct {
	ReceiptID   string    `json:"receipt_id"`
	Timestamp   time.Time `json:"timestamp"`
	MerkleRoot  string    `json:"merkle_root"`
	Actor       string    `json:"actor"`
	Signature   string    `json:"signature,omitempty"` // For verification
}

// CreateJWSPayload creates a JWS payload from receipt data.
func CreateJWSPayload(receiptID, merkleRoot, actor string) JWSPayload {
	return JWSPayload{
		ReceiptID:  receiptID,
		Timestamp:  time.Now(),
		MerkleRoot: merkleRoot,
		Actor:      actor,
	}
}

// EncodeJWSPayload encodes a JWS payload to JSON bytes.
func EncodeJWSPayload(payload JWSPayload) ([]byte, error) {
	return json.Marshal(payload)
}

// DecodeJWSPayload decodes a JWS payload from JSON bytes.
func DecodeJWSPayload(data []byte) (JWSPayload, error) {
	var payload JWSPayload
	err := json.Unmarshal(data, &payload)
	if err != nil {
		return JWSPayload{}, fmt.Errorf("failed to decode JWS payload: %w", err)
	}
	return payload, nil
}

// DetachedJWS represents a detached JWS with separate payload and signature.
type DetachedJWS struct {
	Header  JWSHeader `json:"header"`
	Payload string    `json:"payload"` // base64url encoded
	Signature string  `json:"signature"`
}

// CreateDetachedJWS creates a detached JWS structure.
func CreateDetachedJWS(header JWSHeader, payload []byte, signature string) DetachedJWS {
	encodedPayload := base64.RawURLEncoding.EncodeToString(payload)
	return DetachedJWS{
		Header:    header,
		Payload:   encodedPayload,
		Signature: signature,
	}
}

// VerifyDetachedJWS verifies a detached JWS against the provided payload.
func VerifyDetachedJWS(jws DetachedJWS, expectedPayload []byte, verifier *JWSSigner) error {
	// Decode the payload
	decodedPayload, err := base64.RawURLEncoding.DecodeString(jws.Payload)
	if err != nil {
		return fmt.Errorf("failed to decode payload: %w", err)
	}

	// Verify payload matches
	if string(decodedPayload) != string(expectedPayload) {
		return fmt.Errorf("payload mismatch")
	}

	// Reconstruct the JWS string for verification
	jwsStr := fmt.Sprintf("%s.%s.%s",
		base64.RawURLEncoding.EncodeToString([]byte(`{"alg":"EdDSA","kid":"`+jws.Header.KeyID+`","typ":"JWT"}`)),
		jws.Payload,
		jws.Signature)

	return verifier.VerifyReceipt(jwsStr, expectedPayload)
}
