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
	Field   string      `json:"field"`
	Message string      `json:"message"`
	Value   interface{} `json:"value,omitempty"`
}

// ValidationResult represents the result of pack validation.
type ValidationResult struct {
	Valid    bool              `json:"valid"`
	Errors   []ValidationError `json:"errors,omitempty"`
	Warnings []ValidationError `json:"warnings,omitempty"`
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

	validationResult := &ValidationResult{
		Valid:    result.Valid(),
		Errors:   []ValidationError{},
		Warnings: []ValidationError{},
	}

	if result.Valid() {
		// Perform additional semantic validation
		v.performSemanticValidation(packData, validationResult)
		return validationResult, nil
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

	validationResult.Errors = errors
	return validationResult, nil
}

// performSemanticValidation performs additional semantic validation beyond JSON schema.
func (v *PackValidator) performSemanticValidation(packData []byte, result *ValidationResult) {
	var pack map[string]interface{}
	if err := json.Unmarshal(packData, &pack); err != nil {
		return
	}

	// Validate version format
	if version, exists := pack["version"]; exists {
		versionStr := fmt.Sprintf("%v", version)
		if !strings.HasPrefix(versionStr, "v") || !strings.Contains(versionStr[1:], ".") {
			result.Errors = append(result.Errors, ValidationError{
				Field:   "version",
				Message: "Version must follow pattern vX.Y.Z",
				Value:   version,
			})
		}
	}

	// Validate queries
	if queries, exists := pack["queries"]; exists {
		queriesMap, ok := queries.(map[string]interface{})
		if ok {
			v.validateQueries(queriesMap, result)
		}
	}

	// Validate hooks
	if hooks, exists := pack["hooks"]; exists {
		hooksMap, ok := hooks.(map[string]interface{})
		if ok {
			v.validateHooks(hooksMap, result)
		}
	}

	// Validate shapes
	if shapes, exists := pack["shapes"]; exists {
		shapesMap, ok := shapes.(map[string]interface{})
		if ok {
			v.validateShapes(shapesMap, result)
		}
	}
}

// validateQueries validates query definitions.
func (v *PackValidator) validateQueries(queries map[string]interface{}, result *ValidationResult) {
	for queryName, queryData := range queries {
		queryMap, ok := queryData.(map[string]interface{})
		if !ok {
			continue
		}

		// Check required fields
		if _, hasQuery := queryMap["query"]; !hasQuery {
			result.Errors = append(result.Errors, ValidationError{
				Field:   fmt.Sprintf("queries.%s", queryName),
				Message: "Query field is required",
				Value:   queryData,
			})
		}

		// Validate query kind
		if kind, hasKind := queryMap["kind"]; hasKind {
			kindStr := fmt.Sprintf("%v", kind)
			validKinds := []string{"sparql-select", "sparql-ask", "sparql-construct"}
			valid := false
			for _, validKind := range validKinds {
				if kindStr == validKind {
					valid = true
					break
				}
			}
			if !valid {
				result.Errors = append(result.Errors, ValidationError{
					Field:   fmt.Sprintf("queries.%s.kind", queryName),
					Message: fmt.Sprintf("Invalid query kind '%s'", kindStr),
					Value:   kind,
				})
			}
		}
	}
}

// validateHooks validates hook definitions.
func (v *PackValidator) validateHooks(hooks map[string]interface{}, result *ValidationResult) {
	for hookName, hookData := range hooks {
		hookMap, ok := hookData.(map[string]interface{})
		if !ok {
			continue
		}

		// Check required fields
		if _, hasKind := hookMap["kind"]; !hasKind {
			result.Errors = append(result.Errors, ValidationError{
				Field:   fmt.Sprintf("hooks.%s", hookName),
				Message: "Kind field is required",
				Value:   hookData,
			})
		}

		// Validate hook kind
		if kind, hasKind := hookMap["kind"]; hasKind {
			kindStr := fmt.Sprintf("%v", kind)
			validKinds := []string{"ask", "shacl", "delta", "threshold", "count", "window"}
			valid := false
			for _, validKind := range validKinds {
				if kindStr == validKind {
					valid = true
					break
				}
			}
			if !valid {
				result.Errors = append(result.Errors, ValidationError{
					Field:   fmt.Sprintf("hooks.%s.kind", hookName),
					Message: fmt.Sprintf("Invalid hook kind '%s'", kindStr),
					Value:   kind,
				})
			}
		}

		// Validate WASM effects
		if effect, hasEffect := hookMap["effect"]; hasEffect {
			effectMap, ok := effect.(map[string]interface{})
			if ok {
				if wasmPath, hasWASM := effectMap["wasm"]; hasWASM {
					wasmStr := fmt.Sprintf("%v", wasmPath)
					if !strings.HasSuffix(wasmStr, ".wasm") {
						result.Errors = append(result.Errors, ValidationError{
							Field:   fmt.Sprintf("hooks.%s.effect.wasm", hookName),
							Message: "WASM file must have .wasm extension",
							Value:   wasmPath,
						})
					}
				}
			}
		}
	}
}

// validateShapes validates SHACL shapes.
func (v *PackValidator) validateShapes(shapes map[string]interface{}, result *ValidationResult) {
	for shapeName, shapeData := range shapes {
		shapeStr := fmt.Sprintf("%v", shapeData)

		// Basic validation - check for SHACL vocabulary
		if !strings.Contains(shapeStr, "sh:") {
			result.Warnings = append(result.Warnings, ValidationError{
				Field:   fmt.Sprintf("shapes.%s", shapeName),
				Message: "Shape doesn't appear to contain SHACL vocabulary",
				Value:   shapeData,
			})
		}

		// Check for prefix declarations
		if !strings.Contains(shapeStr, "@prefix") && !strings.Contains(shapeStr, "PREFIX") {
			result.Warnings = append(result.Warnings, ValidationError{
				Field:   fmt.Sprintf("shapes.%s", shapeName),
				Message: "Shape doesn't contain prefix declarations",
				Value:   shapeData,
			})
		}
	}
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
		Path:     packPath,
		Valid:    validationResult.Valid,
		Errors:   validationResult.Errors,
		Warnings: []ValidationError{},
		Info:     []ValidationError{},
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
