package lockchain

import (
	"crypto/ed25519"
	"crypto/rand"
	"encoding/base64"
	"fmt"
	"os"
)

// Signer signs receipts using Ed25519.
type Signer struct {
	privateKey ed25519.PrivateKey
	publicKey  ed25519.PublicKey
}

// NewSigner creates a new Ed25519 signer from key files.
func NewSigner(privateKeyPath, publicKeyPath string) (*Signer, error) {
	var privateKey ed25519.PrivateKey
	var publicKey ed25519.PublicKey
	var err error

	if privateKeyPath != "" {
		// Load private key from file
		privateKeyBytes, err := os.ReadFile(privateKeyPath)
		if err != nil {
			return nil, fmt.Errorf("failed to read private key: %w", err)
		}

		// Decode from PEM or raw bytes
		privateKey = ed25519.PrivateKey(privateKeyBytes[32:]) // Assuming DER format

		// Generate public key from private key
		publicKey = ed25519.PublicKey(privateKey.Public().(ed25519.PublicKey))
	} else if publicKeyPath != "" {
		// Load only public key
		publicKeyBytes, err := os.ReadFile(publicKeyPath)
		if err != nil {
			return nil, fmt.Errorf("failed to read public key: %w", err)
		}
		publicKey = ed25519.PublicKey(publicKeyBytes)
	} else {
		// Generate new keypair if no files provided
		publicKey, privateKey, err = ed25519.GenerateKey(rand.Reader)
		if err != nil {
			return nil, fmt.Errorf("failed to generate keypair: %w", err)
		}
	}

	return &Signer{
		privateKey: privateKey,
		publicKey:  publicKey,
	}, nil
}

// Sign signs data and returns the signature.
func (s *Signer) Sign(data []byte) (signature []byte, err error) {
	if s.privateKey == nil {
		return nil, fmt.Errorf("private key not available")
	}

	signature = ed25519.Sign(s.privateKey, data)
	return signature, nil
}

// Verify verifies a signature against data.
func (s *Signer) Verify(data, signature []byte) bool {
	if s.publicKey == nil {
		return false
	}

	return ed25519.Verify(s.publicKey, data, signature)
}

// SignBase64 signs data and returns base64-encoded signature.
func (s *Signer) SignBase64(data []byte) (signature string, err error) {
	sig, err := s.Sign(data)
	if err != nil {
		return "", err
	}

	return base64.StdEncoding.EncodeToString(sig), nil
}

// VerifyBase64 verifies a base64-encoded signature.
func (s *Signer) VerifyBase64(data []byte, signatureBase64 string) bool {
	signature, err := base64.StdEncoding.DecodeString(signatureBase64)
	if err != nil {
		return false
	}

	return s.Verify(data, signature)
}

// GetPublicKeyBase64 returns the public key as a base64-encoded string.
func (s *Signer) GetPublicKeyBase64() string {
	if s.publicKey == nil {
		return ""
	}
	return base64.StdEncoding.EncodeToString(s.publicKey)
}

// GetPublicKey returns the raw public key.
func (s *Signer) GetPublicKey() []byte {
	return s.publicKey
}

// GetPrivateKey returns the raw private key (use with caution).
func (s *Signer) GetPrivateKey() []byte {
	return s.privateKey
}

// JWS implements JSON Web Signature for detached signatures.
type JWS struct {
	Header    map[string]interface{}
	Payload   []byte
	Signature string
}

// CreateDetachedJWS creates a detached JWS for receipt signing.
func (s *Signer) CreateDetachedJWS(payload []byte) (*JWS, error) {
	signature, err := s.SignBase64(payload)
	if err != nil {
		return nil, err
	}

	header := map[string]interface{}{
		"alg": "EdDSA",
		"crv": "Ed25519",
		"kid": s.GetPublicKeyBase64()[:16], // First 16 chars as key ID
	}

	return &JWS{
		Header:    header,
		Payload:   payload,
		Signature: signature,
	}, nil
}

// VerifyDetachedJWS verifies a detached JWS.
func (s *Signer) VerifyDetachedJWS(jws *JWS) bool {
	if jws.Header["alg"] != "EdDSA" {
		return false
	}

	return s.VerifyBase64(jws.Payload, jws.Signature)
}

// SaveKeyPair saves the keypair to files.
func (s *Signer) SaveKeyPair(privateKeyPath, publicKeyPath string) error {
	if s.privateKey != nil {
		if err := os.WriteFile(privateKeyPath, s.privateKey, 0600); err != nil {
			return fmt.Errorf("failed to write private key: %w", err)
		}
	}

	if s.publicKey != nil {
		if err := os.WriteFile(publicKeyPath, s.publicKey, 0644); err != nil {
			return fmt.Errorf("failed to write public key: %w", err)
		}
	}

	return nil
}


// JSONWebSignature represents a complete JWS structure.
type JSONWebSignature struct {
	Header    map[string]interface{} `json:"header"`
	Payload   string                 `json:"payload"`   // base64url encoded
	Signature string                 `json:"signature"` // base64url encoded
}

// CreateJWS creates a complete JWS structure.
func (s *Signer) CreateJWS(payload []byte) (*JSONWebSignature, error) {
	signature, err := s.SignBase64(payload)
	if err != nil {
		return nil, err
	}

	header := map[string]interface{}{
		"alg": "EdDSA",
		"crv": "Ed25519",
		"typ": "JWT",
	}

	return &JSONWebSignature{
		Header:    header,
		Payload:   base64.RawURLEncoding.EncodeToString(payload),
		Signature: signature,
	}, nil
}

// VerifyJWS verifies a complete JWS structure.
func (s *Signer) VerifyJWS(jws *JSONWebSignature) bool {
	if jws.Header["alg"] != "EdDSA" {
		return false
	}

	// Decode payload
	payload, err := base64.RawURLEncoding.DecodeString(jws.Payload)
	if err != nil {
		return false
	}

	return s.VerifyBase64(payload, jws.Signature)
}
