package main

import (
	"fmt"

	"github.com/unrdf/knowd/internal/sparql"
)

func main() {
	parser := sparql.NewParser()

	// Test a simple query first
	query := "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"
	plan, err := parser.Parse(query)
	if err != nil {
		fmt.Printf("Error parsing query: %v\n", err)
		return
	}

	fmt.Printf("✅ Basic query parsed successfully\n")
	fmt.Printf("   Type: %s, Patterns: %d\n", plan.Type, len(plan.Patterns))

	// Test advanced features
	unionQuery := `SELECT ?s WHERE {
		{ ?s <http://example.org/p> ?o }
		UNION
		{ ?s <http://example.org/q> ?o }
	}`

	unionPlan, err := parser.Parse(unionQuery)
	if err != nil {
		fmt.Printf("❌ Error parsing UNION query: %v\n", err)
		return
	}

	fmt.Printf("✅ UNION query parsed successfully\n")
	fmt.Printf("   Unions: %d\n", len(unionPlan.Unions))

	// Test BIND
	bindQuery := `SELECT ?s WHERE {
		?s <http://example.org/p> ?o .
		BIND(?o AS ?bound)
	}`

	bindPlan, err := parser.Parse(bindQuery)
	if err != nil {
		fmt.Printf("❌ Error parsing BIND query: %v\n", err)
		return
	}

	fmt.Printf("✅ BIND query parsed successfully\n")
	fmt.Printf("   Binds: %d\n", len(bindPlan.Bind))

	// Test OPTIONAL
	optionalQuery := `SELECT ?s ?o WHERE {
		?s <http://example.org/p> ?o .
		OPTIONAL { ?s <http://example.org/q> ?o2 }
	}`

	optionalPlan, err := parser.Parse(optionalQuery)
	if err != nil {
		fmt.Printf("❌ Error parsing OPTIONAL query: %v\n", err)
		return
	}

	fmt.Printf("✅ OPTIONAL query parsed successfully\n")
	fmt.Printf("   Optionals: %d\n", len(optionalPlan.Optional))

	fmt.Println("\n🎉 All advanced SPARQL features parsed successfully!")
}
