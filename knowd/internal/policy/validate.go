// Package policy provides validation for policy packs.
package policy

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strings"

	"github.com/xeipuuv/gojsonschema"
)

// PackValidator validates policy packs against the schema.
type PackValidator struct {
	schemaPath string
	schema     *gojsonschema.Schema
}

// ValidationError represents a validation error.
type ValidationError struct {
	Field   string `json:"field"`
	Message string `json:"message"`
	Value   interface{} `json:"value,omitempty"`
}

// ValidationResult represents the result of pack validation.
type ValidationResult struct {
	Valid  bool              `json:"valid"`
	Errors []ValidationError `json:"errors,omitempty"`
}

// NewPackValidator creates a new pack validator with the given schema path.
func NewPackValidator(schemaPath string) (*PackValidator, error) {
	if schemaPath == "" {
		schemaPath = "api/pack/schema-v1.json"
	}

	schemaBytes, err := os.ReadFile(schemaPath)
	if err != nil {
		return nil, fmt.Errorf("failed to read schema file %s: %w", schemaPath, err)
	}

	schemaLoader := gojsonschema.NewBytesLoader(schemaBytes)
	schema, err := gojsonschema.NewSchema(schemaLoader)
	if err != nil {
		return nil, fmt.Errorf("failed to parse schema: %w", err)
	}

	return &PackValidator{
		schemaPath: schemaPath,
		schema:     schema,
	}, nil
}

// ValidatePack validates a policy pack file or URL.
func (v *PackValidator) ValidatePack(packPath string) (*ValidationResult, error) {
	var packData []byte
	var err error

	if v.isURL(packPath) {
		packData, err = v.fetchURL(packPath)
		if err != nil {
			return nil, fmt.Errorf("failed to fetch pack from URL: %w", err)
		}
	} else {
		packData, err = os.ReadFile(packPath)
		if err != nil {
			return nil, fmt.Errorf("failed to read pack file: %w", err)
		}
	}

	return v.ValidatePackData(packData)
}

// ValidatePackData validates policy pack data from bytes.
func (v *PackValidator) ValidatePackData(packData []byte) (*ValidationResult, error) {
	// Try to parse as JSON first
	var packJSON interface{}
	if err := json.Unmarshal(packData, &packJSON); err != nil {
		return &ValidationResult{
			Valid: false,
			Errors: []ValidationError{{
				Field:   "root",
				Message: "Invalid JSON format",
				Value:   string(packData),
			}},
		}, nil
	}

	// Validate against schema
	documentLoader := gojsonschema.NewBytesLoader(packData)
	result, err := v.schema.Validate(documentLoader)
	if err != nil {
		return nil, fmt.Errorf("validation failed: %w", err)
	}

	if result.Valid() {
		return &ValidationResult{Valid: true}, nil
	}

	// Convert validation errors
	var errors []ValidationError
	for _, err := range result.Errors() {
		errors = append(errors, ValidationError{
			Field:   err.Field(),
			Message: err.Description(),
			Value:   err.Value(),
		})
	}

	return &ValidationResult{
		Valid:  false,
		Errors: errors,
	}, nil
}

// ValidatePackString validates a policy pack from a JSON string.
func (v *PackValidator) ValidatePackString(packJSON string) (*ValidationResult, error) {
	return v.ValidatePackData([]byte(packJSON))
}

// CheckPack performs a comprehensive check of a pack including validation and dependency checks.
func (v *PackValidator) CheckPack(packPath string) (*PackCheckResult, error) {
	// Basic validation
	validationResult, err := v.ValidatePack(packPath)
	if err != nil {
		return nil, err
	}

	result := &PackCheckResult{
		Path:      packPath,
		Valid:     validationResult.Valid,
		Errors:    validationResult.Errors,
		Warnings:  []ValidationError{},
		Info:      []ValidationError{},
	}

	// Additional checks for pack structure
	if v.isURL(packPath) {
		result.Info = append(result.Info, ValidationError{
			Field:   "source",
			Message: "Pack loaded from URL",
			Value:   packPath,
		})
	} else {
		// Check file exists and is readable
		if _, err := os.Stat(packPath); os.IsNotExist(err) {
			result.Errors = append(result.Errors, ValidationError{
				Field:   "file",
				Message: "Pack file does not exist",
				Value:   packPath,
			})
		}
	}

	// Check for recommended fields
	if v.isURL(packPath) {
		packData, err := v.fetchURL(packPath)
		if err == nil {
			var pack map[string]interface{}
			if json.Unmarshal(packData, &pack) == nil {
				// Check for version field
				if _, hasVersion := pack["version"]; !hasVersion {
					result.Warnings = append(result.Warnings, ValidationError{
						Field:   "version",
						Message: "Pack missing version field",
					})
				}
			}
		}
	}

	return result, nil
}

// PackCheckResult represents the result of a comprehensive pack check.
type PackCheckResult struct {
	Path     string            `json:"path"`
	Valid    bool              `json:"valid"`
	Errors   []ValidationError `json:"errors"`
	Warnings []ValidationError `json:"warnings"`
	Info     []ValidationError `json:"info"`
}

// isURL checks if a path is a URL.
func (v *PackValidator) isURL(path string) bool {
	u, err := url.Parse(path)
	return err == nil && u.Scheme != "" && u.Host != ""
}

// fetchURL fetches data from a URL.
func (v *PackValidator) fetchURL(url string) ([]byte, error) {
	resp, err := http.Get(url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("HTTP %d from %s", resp.StatusCode, url)
	}

	return io.ReadAll(resp.Body)
}

// ValidateDirectory validates all pack files in a directory.
func (v *PackValidator) ValidateDirectory(dirPath string) ([]*PackCheckResult, error) {
	var results []*PackCheckResult

	err := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Only check JSON files
		if !strings.HasSuffix(path, ".json") {
			return nil
		}

		result, err := v.CheckPack(path)
		if err != nil {
			return err
		}

		results = append(results, result)
		return nil
	})

	return results, err
}

// FormatValidationErrors formats validation errors for display.
func FormatValidationErrors(errors []ValidationError) string {
	if len(errors) == 0 {
		return "No validation errors"
	}

	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Found %d validation error(s):\n", len(errors)))

	for i, err := range errors {
		sb.WriteString(fmt.Sprintf("  %d. %s: %s", i+1, err.Field, err.Message))
		if err.Value != nil {
			sb.WriteString(fmt.Sprintf(" (value: %v)", err.Value))
		}
		sb.WriteString("\n")
	}

	return sb.String()
}

// HasValidationErrors checks if there are any validation errors.
func HasValidationErrors(result *ValidationResult) bool {
	return !result.Valid
}

// GetValidationErrors returns the validation errors.
func GetValidationErrors(result *ValidationResult) []ValidationError {
	return result.Errors
}
