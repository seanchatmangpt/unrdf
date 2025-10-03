package shacl

import (
	"context"
	"testing"
)

func TestNewValidator(t *testing.T) {
	validator := NewValidator()
	if validator == nil {
		t.Fatal("NewValidator() returned nil")
	}
	if validator.shapes == nil {
		t.Fatal("Validator shapes map not initialized")
	}
}

func TestAddShape(t *testing.T) {
	validator := NewValidator()
	shape := &Shape{
		ID:   "testShape",
		Type: "sh:NodeShape",
	}
	
	validator.AddShape("testShape", shape)
	
	if _, exists := validator.shapes["testShape"]; !exists {
		t.Fatal("Shape not added to validator")
	}
}

func TestValidateBasicShape(t *testing.T) {
	validator := NewValidator()
	
	// Create a simple shape with min/max length constraints
	shape := &Shape{
		ID:   "testShape",
		Type: "sh:NodeShape",
		Properties: []*PropertyShape{
			{
				ID:        "nameProp",
				Path:      "foaf:name",
				MinLength: 2,
				MaxLength: 50,
			},
		},
	}
	
	validator.AddShape("testShape", shape)
	
	// Test data that should conform
	testData := []byte(`
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

_:person1 foaf:name "John Doe" .
`)
	
	result, err := validator.Validate(context.Background(), testData, "ttl")
	if err != nil {
		t.Fatalf("Validation failed: %v", err)
	}
	
	if !result.Conforms {
		t.Fatalf("Should conform to shape, violations: %v", result.Violations)
	}
}

func TestValidateLengthConstraints(t *testing.T) {
	validator := NewValidator()
	
	shape := &Shape{
		ID:   "testShape",
		Type: "sh:NodeShape",
		Properties: []*PropertyShape{
			{
				ID:        "nameProp",
				Path:      "foaf:name",
				MinLength: 5,
				MaxLength: 20,
			},
		},
	}
	
	validator.AddShape("testShape", shape)
	
	tests := []struct {
		name      string
		data      string
		shouldMatch bool
	}{
		{
			name:      "Valid length",
			data:      `_:person1 foaf:name "John Doe Smith" .`,
			shouldMatch: true,
		},
		{
			name:      "Too short",
			data:      `_:person1 foaf:name "Bob" .`,
			shouldMatch: false,
		},
		{
			name:      "Too long",
			data:      `_:person1 foaf:name "This is a very long name that exceeds maximum length" .`,
			shouldMatch: false,
		},
	}
	
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			testData := []byte(`
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

` + tt.data)
		
			result, err := validator.Validate(context.Background(), testData, "ttl")
			if err != nil {
				t.Fatalf("Validation failed: %v", err)
			}
			
			if result.Conforms != tt.shouldMatch {
				if tt.shouldMatch {
					t.Fatalf("Expected data to conform but it didn't, violations: %v", result.Violations)
				} else {
					t.Fatalf("Expected data to not conform but it did")
				}
			}
		})
	}
}

func TestValidateNodeKindConstraints(t *testing.T) {
	validator := NewValidator()
	
	shape := &Shape{
		ID:   "personShape",
		Type: "sh:NodeShape",
		Properties: []*PropertyShape{
			{
				ID:       "homepageProp",
				Path:     "foaf:homepage",
				NodeKind: "sh:IRI",
			},
		},
	}
	
	validator.AddShape("personShape", shape)
	
	tests := []struct {
		name      string
		data      string
		shouldCon bool
	}{
		{
			name:   "Valid IRI",
			data:   `_:person1 foaf:homepage <https://example.com/profile> .`,
			shouldCon: true,
		},
		{
			name:   "Invalid literal",
			data:   `_:person1 foaf:homepage "http://example.com" .`,
			shouldCon: false,
		},
		{
			name:   "Blank node instead of IRI",
			data:   `_:person1 foaf:homepage _:homepage1 .`,
			shouldCon: false,
		},
	}
	
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			testData := []byte(`
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

` + tt.data)
		
			result, err := validator.Validate(context.Background(), testData, "ttl")
			if err != nil {
				t.Fatalf("Validation failed: %v", err)
			}
			
			if result.Conforms != tt.shouldCon {
				if tt.shouldCon {
					t.Fatalf("Expected data to conform but it didn't, violations: %v", result.Violations)
				} else {
					t.Fatalf("Expected data to not conform but it did")
				}
			}
		})
	}
}

func TestValidateCountConstraints(t *testing.T) {
	validator := NewValidator()
	
	shape := &Shape{
		ID:   "articleShape",
		Type: "sh:NodeShape",
		Properties: []*PropertyShape{
			{
				ID:       "titleProp",
				Path:     "dc:title",
				MinCount: 1,
				MaxCount: 3,
			},
		},
	}
	
	validator.AddShape("articleShape", shape)
	
	tests := []struct {
		name      string
		data      string
		shouldCon bool
	}{
		{
			name:   "Valid count",
			data:   `_:article1 dc:title "Main Title" ; dc:title "Subtitle" .`,
			shouldCon: true,
		},
		{
			name:   "Too few titles",
			data:   `_:article1 dc:abstract "Description" .`,
			shouldCon: false,
		},
		{
			name:   "Too many titles",
			data:   `_:article1 dc:title "Title 1" ; dc:title "Title 2" ; dc:title "Title 3" ; dc:title "Title 4" .`,
			shouldCon: false,
		},
	}
	
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			testData := []byte(`
@prefix dc: <http://purl.org/dc/elements/1.1/> .

` + tt.data)
		
			result, err := validator.Validate(context.Background(), testData, "ttl")
			if err != nil {
				t.Fatalf("Validation failed: %v", err)
			}
			
			if result.Conforms != tt.shouldCon {
				if tt.shouldCon {
					t.Fatalf("Expected data to conform but it didn't, violations: %v", result.Violations)
				} else {
					t.Fatalf("Expected data to not conform but it did")
				}
			}
		})
	}
}

func TestValidateWithPatternConstraints(t *testing.T) {
	validator := NewValidator()
	
	shape := &Shape{
		ID:   "contactShape",
		Type: "sh:NodeShape",
		Properties: []*PropertyShape{
			{
				ID:      "emailProp",
				Path:    "foaf:mbox",
				Pattern: `^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$`,
			},
		},
	}
	
	validator.AddShape("contactShape", shape)
	
	tests := []struct {
		name      string
		data      string
		shouldCon bool
	}{
		{
			name:   "Valid email pattern",
			data:   `_:contact1 foaf:mbox "user@example.com" .`,
			shouldCon: true,
		},
		{
			name:   "Invalid email pattern",
			data:   `_:contact1 foaf:mbox "not-an-email" .`,
			shouldCon: false,
		},
		{
			name:   "Another valid email",
			data:   `_:contact1 foaf:mbox "admin@company.org" .`,
			shouldCon: true,
		},
	}
	
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			testData := []byte(`
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

` + tt.data)
		
			result, err := validator.Validate(context.Background(), testData, "ttl")
			if err != nil {
				t.Fatalf("Validation failed: %v", err)
			}
			
			if result.Conforms != tt.shouldCon {
				if tt.shouldCon {
					t.Fatalf("Expected data to conform but it didn't, violations: %v", result.Violations)
				} else {
					t.Fatalf("Expected data to not conform but it did")
				}
			}
		})
	}
}

func TestValidateStrict(t *testing.T) {
	validator := NewValidator()
	
	shape := &Shape{
		ID:   "strictShape",
		Type: "sh:NodeShape",
		Closed: true,
		Properties: []*PropertyShape{
			{
				ID:  "allowedProp",
				Path: "schema:name",
			},
		},
		IgnoredProperties: []string{"rdf:type"},
	}
	
	validator.AddShape("strictShape", shape)
	
	// Test closed shape validation
	testData := []byte(`
@prefix schema: <http://schema.org/> .

_:resource1 schema:name "Example" ;
           schema:description "Extra property" .
`)
	
	result, err := validator.ValidateStrict(context.Background(), testData, "strictShape")
	if err != nil {
		t.Fatalf("Strict validation failed: %v", err)
	}
	
	// Should not conform due to extra property in closedShape
	if result.Conforms {
		t.Fatal("Expected strict validation to reject extra properties")
	}
}

func TestComplexValidation(t *testing.T) {
	validator := NewValidator()
	
	// Create a complex shape with multiple constraints
	shape := &Shape{
		ID:   "complexShape",
		Type: "sh:NodeShape",
		Properties: []*PropertyShape{
			{
				ID:       "nameProp",
				Path:     "foaf:name",
				MinLength: 2,
				MaxLength: 100,
			},
			{
				ID:        "emailProp",
				Path:      "foaf:mbox",
				MinCount:  1,
				Pattern:   `^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$`,
			},
			{
				ID:       "homepageProp",
				Path:     "foaf:homepage",
				NodeKind:  "sh:IRI",
				MaxCount:  1,
			},
		},
	}
	
	validator.AddShape("complexShape", shape)
	
	// Valid complex data
	validData := []byte(`
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

_:person1 a foaf:Person ;
          foaf:name "John Doe" ;
          foaf:mbox "john@example.com" ;
          foaf:homepage <https://example.com/john> .
`)
	
	result, err := validator.Validate(context.Background(), validData, "ttl")
	if err != nil {
		t.Fatalf("Complex validation failed: %v", err)
	}
	
	if !result.Conforms {
		t.Fatalf("Complex validation should conform, violations: %v", result.Violations)
	}
	
	// Test with insufficient data
	invalidData := []byte(`
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

_:person1 foaf:name "John" ;
          foaf:mbox "invalid-email" .
`)
	
	result, err = validator.Validate(context.Background(), invalidData, "ttl")
	if err != nil {
		t.Fatalf("Complex validation failed: %v", err)
	}
	
	// Should not conform due to invalid email pattern
	if result.Conforms {
		t.Fatal("Complex validation should not conform with invalid email")
	}
}

// Benchmark validation performance
func BenchmarkValidation(b *testing.B) {
	validator := NewValidator()
	
	shape := &Shape{
		ID:   "testShape",
		Type: "sh:NodeShape",
		Properties: []*PropertyShape{
			{
				ID:   "nameProp",
				Path: "foaf:name",
				MinLength: 2,
				MaxLength: 50,
			},
		},
	}
	
	validator.AddShape("testShape", shape)
	
	testData := []byte(`
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

_:person1 foaf:name "John Doe" .
`)
	
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := validator.Validate(context.Background(), testData, "ttl")
		if err != nil {
			b.Fatal(err)
		}
	}
}
