package auth

import (
	"crypto/hmac"
	"crypto/sha256"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"time"
)

// Tokens handles token authentication.
type Tokens struct {
	secret []byte
}

// Token represents an authentication token.
type Token struct {
	User      string    `json:"user"`
	Namespace string    `json:"namespace"`
	ExpiresAt time.Time `json:"expires_at"`
	Nonce     string    `json:"nonce"`
}

// TokenConfig holds token configuration.
type TokenConfig struct {
	Secret string
}

// NewTokens creates a new token handler.
func NewTokens(config TokenConfig) *Tokens {
	return &Tokens{
		secret: []byte(config.Secret),
	}
}

// GenerateToken generates a new authentication token.
func (t *Tokens) GenerateToken(user, namespace string, duration time.Duration) (string, error) {
	token := Token{
		User:      user,
		Namespace: namespace,
		ExpiresAt: time.Now().Add(duration),
		Nonce:     fmt.Sprintf("%d", time.Now().UnixNano()),
	}

	data, err := json.Marshal(token)
	if err != nil {
		return "", fmt.Errorf("failed to marshal token: %w", err)
	}

	mac := hmac.New(sha256.New, t.secret)
	mac.Write(data)
	signature := mac.Sum(nil)

	tokenData := map[string]string{
		"token":     base64.StdEncoding.EncodeToString(data),
		"signature": base64.StdEncoding.EncodeToString(signature),
	}

	encoded, err := json.Marshal(tokenData)
	if err != nil {
		return "", fmt.Errorf("failed to encode token: %w", err)
	}

	return base64.StdEncoding.EncodeToString(encoded), nil
}

// ValidateToken validates an authentication token.
func (t *Tokens) ValidateToken(tokenString string) (*Token, error) {
	decoded, err := base64.StdEncoding.DecodeString(tokenString)
	if err != nil {
		return nil, fmt.Errorf("invalid token encoding: %w", err)
	}

	var tokenData map[string]string
	if err := json.Unmarshal(decoded, &tokenData); err != nil {
		return nil, fmt.Errorf("invalid token format: %w", err)
	}

	tokenB64, ok := tokenData["token"]
	if !ok {
		return nil, fmt.Errorf("missing token data")
	}

	signatureB64, ok := tokenData["signature"]
	if !ok {
		return nil, fmt.Errorf("missing token signature")
	}

	tokenDataBytes, err := base64.StdEncoding.DecodeString(tokenB64)
	if err != nil {
		return nil, fmt.Errorf("invalid token data encoding: %w", err)
	}

	signature, err := base64.StdEncoding.DecodeString(signatureB64)
	if err != nil {
		return nil, fmt.Errorf("invalid signature encoding: %w", err)
	}

	mac := hmac.New(sha256.New, t.secret)
	mac.Write(tokenDataBytes)
	expectedSignature := mac.Sum(nil)

	if !hmac.Equal(signature, expectedSignature) {
		return nil, fmt.Errorf("invalid token signature")
	}

	var token Token
	if err := json.Unmarshal(tokenDataBytes, &token); err != nil {
		return nil, fmt.Errorf("failed to unmarshal token: %w", err)
	}

	if time.Now().After(token.ExpiresAt) {
		return nil, fmt.Errorf("token expired")
	}

	return &token, nil
}
