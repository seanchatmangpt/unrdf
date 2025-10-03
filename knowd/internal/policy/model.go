package policy

import "time"

// Pack represents a complete policy pack containing rules, hooks, and configurations.
type Pack struct {
	Version     string            `json:"version"`
	Name        string            `json:"name"`
	Description string            `json:"description"`
	Rules       []Rule            `json:"rules"`
	Hooks       []Hook            `json:"hooks"`
	Config      map[string]string `json:"config"`
	CreatedAt   time.Time         `json:"createdAt"`
	UpdatedAt   time.Time         `json:"updatedAt"`
}

// Rule represents a policy rule.
type Rule struct {
	ID          string   `json:"id"`
	Name        string   `json:"name"`
	Description string   `json:"description"`
	Condition   string   `json:"condition"`
	Action      string   `json:"action"`
	Priority    int      `json:"priority"`
	Enabled     bool     `json:"enabled"`
	Tags        []string `json:"tags"`
}

// Hook represents a hook definition.
type Hook struct {
	ID       string                 `json:"id"`
	Name     string                 `json:"name"`
	Type     string                 `json:"type"`
	Query    string                 `json:"query"`
	Schedule string                 `json:"schedule"`
	Config   map[string]interface{} `json:"config"`
	Enabled  bool                   `json:"enabled"`
}
