// Package sparql provides SPARQL query parsing and execution for knowd.
package sparql

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/pkg/errors"
)

// Parser handles SPARQL query parsing.
type Parser struct {
	// Basic regex patterns for minimal SPARQL parsing
	selectPattern    *regexp.Regexp
	askPattern       *regexp.Regexp
	constructPattern *regexp.Regexp
	// Namespace prefix mappings
	namespaces map[string]string
	prefixes   map[string]string
}

// Plan represents a compiled SPARQL query plan.
type Plan struct {
	Type     string   // SELECT, ASK, CONSTRUCT
	Columns  []string // For SELECT queries
	Patterns []BasicGraphPattern
	Filters  []Filter
	Limit    int
	Offset   int
	Unions   []UnionBlock
	Optional []OptionalBlock
	Minus    []BasicGraphPattern
	Bind     []BindBlock
	Values   []ValuesBlock
	OrderBy  []OrderByItem
	GroupBy  []string
	Having   []Filter
}

// BasicGraphPattern represents a SPARQL basic graph pattern.
type BasicGraphPattern struct {
	Triples []Triple
}

// Triple represents a SPARQL triple pattern.
type Triple struct {
	Subject   string // Can be variable (?s) or literal
	Predicate string // Can be variable (?p) or literal
	Object    string // Can be variable (?o) or literal
}

// Filter represents a SPARQL FILTER expression.
type Filter struct {
	Expression string
}

// UnionBlock represents a UNION block.
type UnionBlock struct {
	Left  BasicGraphPattern
	Right BasicGraphPattern
}

// OptionalBlock represents an OPTIONAL block.
type OptionalBlock struct {
	Pattern BasicGraphPattern
}

// BindBlock represents a BIND clause.
type BindBlock struct {
	Variable   string
	Expression string
}

// ValuesBlock represents a VALUES clause.
type ValuesBlock struct {
	Variables []string
	Values    [][]string
}

// OrderByItem represents an ORDER BY item.
type OrderByItem struct {
	Variable   string
	Descending bool
}

// BindClause represents a BIND clause.
type BindClause struct {
	Variable   string
	Expression string
}

// NewParser creates a new SPARQL parser.
func NewParser() *Parser {
	return &Parser{
		selectPattern:    regexp.MustCompile(`(?i)(?:PREFIX\s+\w+:\s*<[^>]+>\s*)*SELECT\s+(.+?)\s+WHERE\s*\{`),
		askPattern:       regexp.MustCompile(`(?i)(?:PREFIX\s+\w+:\s*<[^>]+>\s*)*ASK\s+WHERE\s*\{`),
		constructPattern: regexp.MustCompile(`(?i)(?:PREFIX\s+\w+:\s*<[^>]+>\s*)*CONSTRUCT\s*\{`),
		namespaces:       make(map[string]string),
	}
}

// parsePrefixes extracts PREFIX declarations from the query.
func (p *Parser) parsePrefixes(query string) {
	// Simple regex to find PREFIX declarations
	prefixRegex := regexp.MustCompile(`(?i)PREFIX\s+(\w+):\s*<([^>]+)>`)
	matches := prefixRegex.FindAllStringSubmatch(query, -1)

	for _, match := range matches {
		if len(match) >= 3 {
			prefix := match[1]
			uri := match[2]
			p.namespaces[prefix] = uri
		}
	}
}

// expandPrefixes expands prefixed names in the query using the namespace map.
func (p *Parser) expandPrefixes(term string) string {
	// Handle prefixed names like foaf:name
	if strings.Contains(term, ":") {
		parts := strings.SplitN(term, ":", 2)
		if len(parts) == 2 {
			prefix := parts[0]
			localName := parts[1]

			if namespace, exists := p.namespaces[prefix]; exists {
				// Return full URI with angle brackets
				return fmt.Sprintf("<%s%s>", namespace, localName)
			}
		}
	}

	// Handle full URIs already in angle brackets
	if strings.HasPrefix(term, "<") && strings.HasSuffix(term, ">") {
		return term
	}

	// Handle variables and literals
	return term
}

// Parse parses a SPARQL query string and returns a plan.
func (p *Parser) Parse(query string) (*Plan, error) {
	query = strings.TrimSpace(query)
	if query == "" {
		return nil, errors.New("empty query")
	}

	// Extract PREFIX declarations
	p.parsePrefixes(query)

	// Try SELECT query
	if p.selectPattern.MatchString(query) {
		return p.parseSelect(query)
	}

	// Try ASK query
	if p.askPattern.MatchString(query) {
		return p.parseAsk(query)
	}

	// Try CONSTRUCT query
	if p.constructPattern.MatchString(query) {
		return p.parseConstruct(query)
	}

	return nil, errors.Errorf("unsupported query type: %s", query[:min(50, len(query))])
}

// parseSelect parses a SELECT query.
func (p *Parser) parseSelect(query string) (*Plan, error) {
	matches := p.selectPattern.FindStringSubmatch(query)
	if len(matches) < 2 {
		return nil, errors.New("invalid SELECT query format")
	}

	columns := p.parseColumns(matches[1])

	// Extract WHERE clause content
	whereStart := strings.Index(query, "{")
	if whereStart == -1 {
		return nil, fmt.Errorf("missing WHERE clause")
	}

	whereEnd := strings.LastIndex(query, "}")
	if whereEnd <= whereStart {
		return nil, fmt.Errorf("malformed WHERE clause")
	}

	whereClause := query[whereStart+1 : whereEnd]

	// Parse advanced constructs from WHERE clause
	patterns, unions, optionals, minuses, binds, values := p.parseAdvancedConstructs(whereClause)

	// Parse filters from WHERE clause
	filters := p.parseFilters(whereClause)

	// Parse LIMIT and OFFSET
	limit := p.parseLimit(query[whereEnd+1:])
	offset := p.parseOffset(query[whereEnd+1:])

	return &Plan{
		Type:     "SELECT",
		Columns:  columns,
		Patterns: patterns,
		Unions:   unions,
		Optional: optionals,
		Minus:    minuses,
		Bind:     binds,
		Values:   values,
		Filters:  filters,
		Limit:    limit,
		Offset:   offset,
	}, nil
}

// parseAsk parses an ASK query.
func (p *Parser) parseAsk(query string) (*Plan, error) {
	// Extract WHERE clause content
	whereStart := strings.Index(query, "{")
	if whereStart == -1 {
		return nil, fmt.Errorf("missing WHERE clause")
	}

	whereEnd := strings.LastIndex(query, "}")
	if whereEnd <= whereStart {
		return nil, fmt.Errorf("malformed WHERE clause")
	}

	whereClause := query[whereStart+1 : whereEnd]

	// Parse advanced constructs from WHERE clause
	patterns, unions, optionals, minuses, binds, values := p.parseAdvancedConstructs(whereClause)

	return &Plan{
		Type:     "ASK",
		Patterns: patterns,
		Unions:   unions,
		Optional: optionals,
		Minus:    minuses,
		Bind:     binds,
		Values:   values,
	}, nil
}

// parseConstruct parses a CONSTRUCT query.
func (p *Parser) parseConstruct(query string) (*Plan, error) {
	// For now, simplified CONSTRUCT parsing
	constructStart := strings.Index(query, "{")
	if constructStart == -1 {
		return nil, fmt.Errorf("missing CONSTRUCT template")
	}

	// Find WHERE clause
	whereIdx := strings.Index(query[constructStart:], "WHERE")
	if whereIdx == -1 {
		return nil, fmt.Errorf("missing WHERE clause in CONSTRUCT")
	}

	whereStart := constructStart + whereIdx + 5
	whereEnd := strings.LastIndex(query, "}")
	if whereEnd <= whereStart {
		return nil, fmt.Errorf("malformed WHERE clause")
	}

	whereClause := query[whereStart+1 : whereEnd]

	// Parse advanced constructs from WHERE clause
	patterns, unions, optionals, minuses, binds, values := p.parseAdvancedConstructs(whereClause)

	return &Plan{
		Type:     "CONSTRUCT",
		Patterns: patterns,
		Unions:   unions,
		Optional: optionals,
		Minus:    minuses,
		Bind:     binds,
		Values:   values,
	}, nil
}

// parseColumns parses SELECT columns.
func (p *Parser) parseColumns(columnsStr string) []string {
	if strings.Contains(columnsStr, "*") {
		return []string{"*"}
	}

	// Simple variable parsing - split on whitespace
	columns := []string{}
	parts := strings.Fields(columnsStr)
	for _, part := range parts {
		column := strings.TrimSpace(part)
		if strings.HasPrefix(column, "?") {
			columns = append(columns, column)
		}
	}

	return columns
}

// parseBasicGraphPattern parses a basic graph pattern from SPARQL query text.
// It handles simple triple patterns but delegates advanced constructs to parseAdvancedConstructs.
func (p *Parser) parseBasicGraphPattern(clause string) BasicGraphPattern {
	var triples []Triple

	// Split the clause into individual statements (separated by dots)
	statements := p.splitOnDots(clause)

	for _, statement := range statements {
		statement = strings.TrimSpace(statement)
		if statement == "" || strings.HasPrefix(statement, "#") {
			continue
		}

		linesUpper := strings.ToUpper(statement)

		// Skip advanced constructs - they are handled by parseAdvancedConstructs
		if p.isAdvancedConstruct(linesUpper) {
			continue
		}

		// Parse simple triple pattern
		if triple := p.parseTriplePattern(statement); triple != nil {
			triples = append(triples, *triple)
		}
	}

	return BasicGraphPattern{Triples: triples}
}

// splitOnDots splits a clause into individual statements separated by dots.
func (p *Parser) splitOnDots(clause string) []string {
	var statements []string
	var current strings.Builder
	inBraces := 0

	for _, char := range clause {
		switch char {
		case '{':
			inBraces++
			current.WriteRune(char)
		case '}':
			inBraces--
			current.WriteRune(char)
		case '.':
			if inBraces == 0 {
				// Check if this dot separates statements or is part of a triple
				statement := strings.TrimSpace(current.String())
				if statement != "" {
					statements = append(statements, statement)
				}
				current.Reset()
			} else {
				current.WriteRune(char)
			}
		default:
			current.WriteRune(char)
		}
	}

	// Add the last statement if any
	lastStatement := strings.TrimSpace(current.String())
	if lastStatement != "" {
		statements = append(statements, lastStatement)
	}

	return statements
}

// isAdvancedConstruct checks if a line contains advanced SPARQL constructs.
func (p *Parser) isAdvancedConstruct(line string) bool {
	advancedKeywords := []string{"UNION", "OPTIONAL", "MINUS", "BIND", "VALUES", "FILTER"}
	for _, keyword := range advancedKeywords {
		if strings.Contains(line, keyword) {
			return true
		}
	}
	return false
}

// parseFilters extracts FILTER expressions from the WHERE clause.
func (p *Parser) parseFilters(clause string) []Filter {
	var filters []Filter

	// Split the clause into individual statements
	statements := p.splitOnDots(clause)

	for _, statement := range statements {
		statement = strings.TrimSpace(statement)
		if statement == "" || strings.HasPrefix(statement, "#") {
			continue
		}

		linesUpper := strings.ToUpper(statement)
		if strings.Contains(linesUpper, "FILTER") {
			// Extract the filter expression
			start := strings.Index(statement, "FILTER(") + 7
			end := strings.LastIndex(statement, ")")
			if start > 0 && end > start {
				expression := strings.TrimSpace(statement[start:end])
				filters = append(filters, Filter{Expression: expression})
			}
		}
	}

	return filters
}

// parseTriplePattern parses a single triple pattern from a line of SPARQL.
func (p *Parser) parseTriplePattern(line string) *Triple {
	// Check if line contains advanced constructs first
	linesUpper := strings.ToUpper(line)
	if p.isAdvancedConstruct(linesUpper) {
		return nil // This line contains advanced constructs, not a simple triple
	}

	// Remove trailing period if present
	line = strings.TrimSuffix(line, ".")

	// Simple triple pattern parsing - split by whitespace
	parts := strings.Fields(line)
	if len(parts) < 3 {
		return nil // Not a valid triple pattern
	}

	return &Triple{
		Subject:   p.expandPrefixes(parts[0]),
		Predicate: p.expandPrefixes(parts[1]),
		Object:    p.expandPrefixes(strings.Join(parts[2:], " ")),
	}
}

// parseLimit parses LIMIT clause from query text.
// Returns 0 if no valid LIMIT is found or if parsing fails.
func (p *Parser) parseLimit(queryPart string) int {
	limitRe := regexp.MustCompile(`(?i)\s+LIMIT\s+(\d+)`)
	if matches := limitRe.FindStringSubmatch(queryPart); len(matches) > 1 {
		var limit int
		if n, err := fmt.Sscanf(matches[1], "%d", &limit); n == 1 && err == nil && limit >= 0 {
			return limit
		}
	}
	return 0
}

// parseOffset parses OFFSET clause from query text.
// Returns 0 if no valid OFFSET is found or if parsing fails.
func (p *Parser) parseOffset(queryPart string) int {
	offsetRe := regexp.MustCompile(`(?i)\s+OFFSET\s+(\d+)`)
	if matches := offsetRe.FindStringSubmatch(queryPart); len(matches) > 1 {
		var offset int
		if n, err := fmt.Sscanf(matches[1], "%d", &offset); n == 1 && err == nil && offset >= 0 {
			return offset
		}
	}
	return 0
}

// parseAdvancedConstructs parses advanced SPARQL constructs from WHERE clause.
// Returns patterns, unions, optionals, minuses, binds, and values in that order.
func (p *Parser) parseAdvancedConstructs(clause string) ([]BasicGraphPattern, []UnionBlock, []OptionalBlock, []BasicGraphPattern, []BindBlock, []ValuesBlock) {
	var patterns []BasicGraphPattern
	var unions []UnionBlock
	var optionals []OptionalBlock
	var minuses []BasicGraphPattern
	var binds []BindBlock
	var values []ValuesBlock

	lines := strings.Split(clause, "\n")
	i := 0

	for i < len(lines) {
		line := strings.TrimSpace(lines[i])
		if line == "" || strings.HasPrefix(line, "#") {
			i++
			continue
		}

		linesUpper := strings.ToUpper(line)

		switch {
		case strings.Contains(linesUpper, "UNION"):
			leftPattern, rightPattern, newI := p.parseUnionBlock(lines, i)
			unions = append(unions, UnionBlock{Left: leftPattern, Right: rightPattern})
			i = newI

		case strings.Contains(linesUpper, "OPTIONAL"):
			optionalPattern, newI := p.parseOptionalBlock(lines, i)
			optionals = append(optionals, OptionalBlock{Pattern: optionalPattern})
			i = newI

		case strings.Contains(linesUpper, "MINUS"):
			minusPattern, newI := p.parseMinusBlock(lines, i)
			minuses = append(minuses, minusPattern)
			i = newI

		case strings.Contains(linesUpper, "BIND"):
			bind, newI := p.parseBindBlock(lines, i)
			binds = append(binds, bind)
			i = newI

		case strings.Contains(linesUpper, "VALUES"):
			valuesBlock, newI := p.parseValuesBlock(lines, i)
			values = append(values, valuesBlock)
			i = newI

		default:
			// Parse as basic graph pattern
			bgp, newI := p.parseBasicGraphPatternFromLines(lines, i)
			patterns = append(patterns, bgp)
			i = newI
		}
	}

	return patterns, unions, optionals, minuses, binds, values
}

// parseUnionBlock parses a UNION block.
func (p *Parser) parseUnionBlock(lines []string, start int) (BasicGraphPattern, BasicGraphPattern, int) {
	// Skip the UNION keyword
	i := start + 1
	leftPattern, i := p.parseBasicGraphPatternFromLines(lines, i)
	rightPattern, i := p.parseBasicGraphPatternFromLines(lines, i)
	return leftPattern, rightPattern, i
}

// parseOptionalBlock parses an OPTIONAL block.
func (p *Parser) parseOptionalBlock(lines []string, start int) (BasicGraphPattern, int) {
	// Skip the OPTIONAL keyword and opening brace
	i := start + 1
	if i < len(lines) && strings.Contains(lines[i], "{") {
		i++
	}
	pattern, end := p.parseBasicGraphPatternFromLines(lines, i)
	return pattern, end
}

// parseMinusBlock parses a MINUS block.
func (p *Parser) parseMinusBlock(lines []string, start int) (BasicGraphPattern, int) {
	// Skip the MINUS keyword and opening brace
	i := start + 1
	if i < len(lines) && strings.Contains(lines[i], "{") {
		i++
	}
	pattern, end := p.parseBasicGraphPatternFromLines(lines, i)
	return pattern, end
}

// parseBindBlock parses a BIND clause.
// Expected format: BIND(?variable AS ?expression)
func (p *Parser) parseBindBlock(lines []string, start int) (BindBlock, int) {
	line := strings.TrimSpace(lines[start])

	var variable, expression string

	// Look for BIND(?var AS ?expr) pattern
	if strings.HasPrefix(line, "BIND(") && strings.Contains(line, " AS ") {
		// Extract content inside parentheses
		content := line[5 : len(line)-1] // Remove BIND( and )

		// Split by AS
		parts := strings.Split(content, " AS ")
		if len(parts) == 2 {
			variable = strings.TrimSpace(parts[0])
			expression = strings.TrimSpace(parts[1])
		}
	}

	return BindBlock{
		Variable:   variable,
		Expression: expression,
	}, start + 1
}

// parseValuesBlock parses a VALUES clause.
// Expected format: VALUES ?var1 ?var2 { ("val1" "val2") ("val3" "val4") }
func (p *Parser) parseValuesBlock(lines []string, start int) (ValuesBlock, int) {
	line := strings.TrimSpace(lines[start])

	var variables []string
	var values [][]string

	// Look for VALUES ?vars { values } pattern
	if strings.HasPrefix(line, "VALUES") && strings.Contains(line, "{") {
		// Extract variables (between VALUES and {)
		valuesStart := strings.Index(line, "VALUES") + 6
		braceStart := strings.Index(line, "{")
		if braceStart > valuesStart {
			varStr := strings.TrimSpace(line[valuesStart:braceStart])
			variables = strings.Fields(varStr)
		}

		// Extract values (inside braces)
		braceEnd := strings.LastIndex(line, "}")
		if braceEnd > braceStart {
			valueStr := line[braceStart+1 : braceEnd]
			values = p.parseValuesList(valueStr)
		}
	}

	return ValuesBlock{
		Variables: variables,
		Values:    values,
	}, start + 1
}

// parseValuesList parses the list of values from a VALUES clause.
func (p *Parser) parseValuesList(valueStr string) [][]string {
	// This is a simplified parser for VALUES content
	// In a real implementation, this would handle complex quoting and escaping
	var result [][]string

	// Split by spaces, but this is very basic
	fields := strings.Fields(valueStr)
	result = append(result, fields)

	return result
}

// parseBasicGraphPatternFromLines parses a basic graph pattern starting from line index.
func (p *Parser) parseBasicGraphPatternFromLines(lines []string, start int) (BasicGraphPattern, int) {
	var triples []Triple
	i := start

	for i < len(lines) {
		line := strings.TrimSpace(lines[i])
		if line == "" || strings.HasPrefix(line, "#") {
			i++
			continue
		}

		linesUpper := strings.ToUpper(line)

		// Check if this is the start of an advanced construct
		if strings.Contains(linesUpper, "UNION") ||
			strings.Contains(linesUpper, "OPTIONAL") ||
			strings.Contains(linesUpper, "MINUS") ||
			strings.Contains(linesUpper, "BIND") ||
			strings.Contains(linesUpper, "VALUES") ||
			line == "}" {
			break
		}

		// Parse simple triple patterns
		if parts := strings.Fields(line); len(parts) >= 3 {
			triple := Triple{
				Subject:   parts[0],
				Predicate: parts[1],
				Object:    strings.Join(parts[2:], " "),
			}
			triples = append(triples, triple)
		}
		i++
	}

	return BasicGraphPattern{Triples: triples}, i
}

// min returns the minimum of two integers.
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}
