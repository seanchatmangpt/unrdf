package policy

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"path/filepath"
	"time"
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

	// Validate pack
	if err := l.validatePack(&pack); err != nil {
		return nil, fmt.Errorf("invalid pack: %w", err)
	}

	return &pack, nil
}
