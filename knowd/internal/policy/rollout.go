package policy

import (
	"encoding/json"
	"fmt"
	"sync"
	"time"
)

// Rollout manages pack rollouts.
type Rollout struct {
	mu       sync.RWMutex
	policies map[string]RolloutPolicy
}

// RolloutPolicy represents a rollout configuration.
 type RolloutPolicy struct {
	Namespace string  `json:"namespace"`
	Stable   string  `json:"stable"`
	Canary   string  `json:"canary"`
	Percent  int     `json:"percent"`
	Updated  time.Time `json:"updated"`
}

// RolloutConfig holds rollout configuration.
type RolloutConfig struct{}

// NewRollout creates a new rollout manager.
func NewRollout(config RolloutConfig) *Rollout {
	return &Rollout{
		policies: make(map[string]RolloutPolicy),
	}
}

// SetRollout sets a rollout policy for a namespace.
func (r *Rollout) SetRollout(namespace, stable, canary string, percent int) error {
	if percent < 0 || percent > 100 {
		return fmt.Errorf("rollout percentage must be between 0 and 100")
	}
	
	r.mu.Lock()
	defer r.mu.Unlock()
	
	r.policies[namespace] = RolloutPolicy{
		Namespace: namespace,
		Stable:   stable,
		Canary:   canary,
		Percent:  percent,
		Updated:  time.Now(),
	}
	
	return nil
}

// GetRollout returns the rollout policy for a namespace.
func (r *Rollout) GetRollout(namespace string) (RolloutPolicy, bool) {
	r.mu.RLock()
	defer r.mu.RUnlock()
	
	policy, exists := r.policies[namespace]
	return policy, exists
}

// GetAllRollouts returns all rollout policies.
func (r *Rollout) GetAllRollouts() map[string]RolloutPolicy {
	r.mu.RLock()
	defer r.mu.RUnlock()
	
	// Return a copy to avoid race conditions
	result := make(map[string]RolloutPolicy)
	for k, v := range r.policies {
		result[k] = v
	}
	return result
}

// ClearRollout clears the rollout policy for a namespace.
func (r *Rollout) ClearRollout(namespace string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	
	delete(r.policies, namespace)
}

// ShouldUseCanary determines if a request should use the canary version.
func (r *Rollout) ShouldUseCanary(namespace, requestHash string) bool {
	r.mu.RLock()
	defer r.mu.RUnlock()
	
	policy, exists := r.policies[namespace]
	if !exists {
		return false
	}
	
	if policy.Percent == 0 {
		return false
	}
	
	if policy.Percent == 100 {
		return true
	}
	
	// Simple hash-based routing
	hash := 0
	for _, char := range requestHash {
		hash = (hash*31 + int(char)) % 100
	}
	
	return hash < policy.Percent
}

// Serialize returns serialized rollout policies.
func (r *Rollout) Serialize() ([]byte, error) {
	r.mu.RLock()
	defer r.mu.RUnlock()
	
	policies := make([]RolloutPolicy, 0, len(r.policies))
	for _, policy := range r.policies {
		policies = append(policies, policy)
	}
	
	return json.Marshal(policies)
}

// Deserialize loads rollout policies from serialized data.
func (r *Rollout) Deserialize(data []byte) error {
	var policies []RolloutPolicy
	if err := json.Unmarshal(data, &policies); err != nil {
		return fmt.Errorf("failed to unmarshal rollouts: %w", err)
	}
	
	r.mu.Lock()
	defer r.mu.Unlock()
	
	r.policies = make(map[string]RolloutPolicy)
	for _, policy := range policies {
		r.policies[policy.Namespace] = policy
	}
	
	return nil
}
