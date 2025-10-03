package policy

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"path/filepath"
	"time"

	"github.com/xeipuuv/gojsonschema"
)

// Loader handles loading policy packs from files.
type Loader struct {
	loadedPacks map[string]*Pack
}

// NewLoader creates a new policy loader.
func NewLoader() *Loader {
	return &Loader{
		loadedPacks: make(map[string]*Pack),
	}
}

// LoadPack loads a policy pack from a file path.
func (l *Loader) LoadPack(path string) (*Pack, error) {
	// Check if already loaded
	if pack, exists := l.loadedPacks[path]; exists {
		return pack, nil
	}

	// Read file
	data, err := ioutil.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("failed to read pack file %s: %w", path, err)
	}

	// Parse JSON
	var pack Pack
	if err := json.Unmarshal(data, &pack); err != nil {
		return nil, fmt.Errorf("failed to parse pack file %s: %w", path, err)
	}

	// Set timestamps if not present
	if pack.CreatedAt.IsZero() {
		pack.CreatedAt = time.Now()
	}
	if pack.UpdatedAt.IsZero() {
		pack.UpdatedAt = time.Now()
	}

	// Validate pack
	if err := l.validatePack(&pack); err != nil {
		return nil, fmt.Errorf("invalid pack %s: %w", path, err)
	}

	// Cache the pack
	l.loadedPacks[path] = &pack

	return &pack, nil
}

// LoadPacksFromDirectory loads all pack files from a directory.
func (l *Loader) LoadPacksFromDirectory(dirPath string) (map[string]*Pack, error) {
	packs := make(map[string]*Pack)

	// Read directory
	files, err := ioutil.ReadDir(dirPath)
	if err != nil {
		return nil, fmt.Errorf("failed to read directory %s: %w", dirPath, err)
	}

	// Load each JSON file
	for _, file := range files {
		if filepath.Ext(file.Name()) == ".json" {
			path := filepath.Join(dirPath, file.Name())
			pack, err := l.LoadPack(path)
			if err != nil {
				return nil, fmt.Errorf("failed to load pack %s: %w", path, err)
			}
			packs[pack.Name] = pack
		}
	}

	return packs, nil
}

// GetLoadedPacks returns all currently loaded packs.
func (l *Loader) GetLoadedPacks() map[string]*Pack {
	return l.loadedPacks
}

// ReloadPack reloads a pack from disk.
func (l *Loader) ReloadPack(path string) (*Pack, error) {
	// Remove from cache
	delete(l.loadedPacks, path)

	// Reload
	return l.LoadPack(path)
}

// validatePack validates a policy pack structure.
func (l *Loader) validatePack(pack *Pack) error {
	if pack.Name == "" {
		return fmt.Errorf("pack name is required")
	}
	if pack.Version == "" {
		return fmt.Errorf("pack version is required")
	}

	// Validate rules
	for i, rule := range pack.Rules {
		if rule.ID == "" {
			return fmt.Errorf("rule %d has empty ID", i)
		}
		if rule.Name == "" {
			return fmt.Errorf("rule %d has empty name", i)
		}
	}

	// Validate hooks
	for i, hook := range pack.Hooks {
		if hook.ID == "" {
			return fmt.Errorf("hook %d has empty ID", i)
		}
		if hook.Name == "" {
			return fmt.Errorf("hook %d has empty name", i)
		}
		if hook.Type == "" {
			return fmt.Errorf("hook %d has empty type", i)

		}
	}

	return nil
}

// LoadPackFromBytes loads a policy pack from raw JSON bytes.
func (l *Loader) LoadPackFromBytes(data []byte) (*Pack, error) {
	var pack Pack
	if err := json.Unmarshal(data, &pack); err != nil {
		return nil, fmt.Errorf("failed to parse pack JSON: %w", err)
	}

	// Set timestamps if not present
	if pack.CreatedAt.IsZero() {
		pack.CreatedAt = time.Now()
	}
	if pack.UpdatedAt.IsZero() {
		pack.UpdatedAt = time.Now()
	}

	// Validate pack with enhanced schema validation
	if err := l.validatePackWithSchema(&pack); err != nil {
		// Schema validation failed, falling back to basic validation
		if err := l.validatePack(&pack); err != nil {
			return nil, fmt.Errorf("invalid pack: %w", err)
		}
	}

	return &pack, nil
}

// validatePackWithSchema validates a policy pack using JSON schema.
func (l *Loader) validatePackWithSchema(pack *Pack) error {
	// Convert pack to JSON for schema validation
	data, err := json.Marshal(pack)
	if err != nil {
		return fmt.Errorf("failed to marshal pack to JSON: %w", err)
	}

	// Load policy pack schema
	schemaLoader := gojsonschema.NewStringLoader(l.getPolicyPackSchema())
	documentLoader := gojsonschema.NewBytesLoader(data)

	result, err := gojsonschema.Validate(schemaLoader, documentLoader)
	if err != nil {
		return fmt.Errorf("schema validation error: %w", err)
	}

	if !result.Valid() {
		var errs []string
		for _, desc := range result.Errors() {
			errs = append(errs, fmt.Sprintf("%s: %s", desc.Field(), desc.Description()))
		}
		errStr := ""
		for i, err := range errs {
			if i > 0 {
				errStr += "; "
			}
			errStr += err
		}
		return fmt.Errorf("pack validation failed: %s", errStr)
	}

	return nil
}

// getPolicyPackSchema returns the JSON schema for policy packs.
func (l *Loader) getPolicyPackSchema() string {
	return `{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "required": ["version", "name"],
  "properties": {
    "version": {
      "type": "string",
      "pattern": "^v[0-9]+\\.[0-9]+\\.[0-9]+$"
    },
    "name": {
      "type": "string",
      "minLength": 1,
      "maxLength": 100
    },
    "description": {
      "type": "string",
      "maxLength": 500
    },
    "rules": {
      "type": "array",
      "items": {
        "$ref": "#/definitions/rule"
      }
    },
    "hooks": {
      "type": "array",
      "items": {
        "$ref": "#/definitions/hook"
      }
    },
    "config": {
      "type": "object"
    },
    "createdAt": {
      "type": "string",
      "format": "date-time"
    },
    "updatedAt": {
      "type": "string",
      "format": "date-time"
    }
  },
  "definitions": {
    "rule": {
      "type": "object",
      "required": ["id", "name"],
      "properties": {
        "id": {"type": "string"},
        "name": {"type": "string"},
        "description": {"type": "string"},
        "condition": {"type": "string"},
        "action": {"type": "string"},
        "priority": {"type": "number"},
        "enabled": {"type": "boolean"},
        "tags": {"type": "array", "items": {"type": "string"}}
      }
    },
    "hook": {
      "type": "object",
      "required": ["id", "name", "type"],
      "properties": {
        "id": {"type": "string"},
        "name": {"type": "string"},
        "type": {"type": "string"},
        "query": {"type": "string"},
        "schedule": {"type": "string"},
        "config": {"type": "object"},
        "enabled": {"type": "boolean"}
      }
    }
  }
}`
}
