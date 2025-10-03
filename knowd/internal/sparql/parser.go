// Package sparql provides SPARQL query parsing and execution for knowd.
package sparql

import (
	"fmt"
	"regexp"
	"strings"
)

// Parser handles SPARQL query parsing.
type Parser struct {
	// Basic regex patterns for minimal SPARQL parsing
	selectPattern    *regexp.Regexp
	askPattern       *regexp.Regexp
	constructPattern *regexp.Regexp
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
		selectPattern:    regexp.MustCompile(`(?i)^\s*SELECT\s+(.+?)\s+WHERE\s*\{`),
		askPattern:       regexp.MustCompile(`(?i)^\s*ASK\s+WHERE\s*\{`),
		constructPattern: regexp.MustCompile(`(?i)^\s*CONSTRUCT\s*\{`),
	}
}

// Parse parses a SPARQL query string and returns a plan.
func (p *Parser) Parse(query string) (*Plan, error) {
	query = strings.TrimSpace(query)

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

	return nil, fmt.Errorf("unsupported query type: %s", query[:min(50, len(query))])
}

// parseSelect parses a SELECT query.
func (p *Parser) parseSelect(query string) (*Plan, error) {
	matches := p.selectPattern.FindStringSubmatch(query)
	if len(matches) < 2 {
		return nil, fmt.Errorf("invalid SELECT query")
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
	pattern := p.parseBasicGraphPattern(whereClause)

	// Parse LIMIT and OFFSET
	limit := p.parseLimit(query[whereEnd+1:])
	offset := p.parseOffset(query[whereEnd+1:])

	return &Plan{
		Type:     "SELECT",
		Columns:  columns,
		Patterns: []BasicGraphPattern{pattern},
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
	pattern := p.parseBasicGraphPattern(whereClause)

	return &Plan{
		Type:     "ASK",
		Patterns: []BasicGraphPattern{pattern},
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
	pattern := p.parseBasicGraphPattern(whereClause)

	return &Plan{
		Type:     "CONSTRUCT",
		Patterns: []BasicGraphPattern{pattern},
	}, nil
}

// parseColumns parses SELECT columns.
func (p *Parser) parseColumns(columnsStr string) []string {
	if strings.Contains(columnsStr, "*") {
		return []string{"*"}
	}

	// Simple variable parsing
	columns := []string{}
	parts := strings.Split(columnsStr, ",")
	for _, part := range parts {
		column := strings.TrimSpace(part)
		if strings.HasPrefix(column, "?") {
			columns = append(columns, column)
		}
	}

	return columns
}

// parseBasicGraphPattern parses the inner clause of WHERE.
func (p *Parser) parseBasicGraphPattern(clause string) BasicGraphPattern {
	var triples []Triple

	// Simple triple pattern parsing with enhanced construct support
	lines := strings.Split(clause, "\n")
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}

		linesUpper := strings.ToUpper(line)

		// Handle advanced constructs
		if strings.Contains(linesUpper, "UNION") {
			// UNION parsing would be handled at a higher level
			continue
		} else if strings.Contains(linesUpper, "OPTIONAL") {
			// OPTIONAL parsing would be handled at a higher level
			continue
		} else if strings.Contains(linesUpper, "MINUS") {
			// MINUS parsing would be handled at a higher level
			continue
		} else if strings.Contains(linesUpper, "BIND") {
			// BIND parsing would be handled at a higher level
			continue
		} else if strings.Contains(linesUpper, "VALUES") {
			// VALUES parsing would be handled at a higher level
			continue
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
	}

	return BasicGraphPattern{Triples: triples}
}

// parseLimit parses LIMIT clause from query text.
func (p *Parser) parseLimit(queryPart string) int {
	limitRe := regexp.MustCompile(`(?i)\s+LIMIT\s+(\d+)`)
	if matches := limitRe.FindStringSubmatch(queryPart); len(matches) > 1 {
		var limit int
		if _, err := fmt.Sscanf(matches[1], "%d", &limit); err == nil {
			return limit
		}
	}
	return 0
}

// parseOffset parses OFFSET clause from query text.
func (p *Parser) parseOffset(queryPart string) int {
	offsetRe := regexp.MustCompile(`(?i)\s+OFFSET\s+(\d+)`)
	if matches := offsetRe.FindStringSubmatch(queryPart); len(matches) > 1 {
		var offset int
		if _, err := fmt.Sscanf(matches[1], "%d", &offset); err == nil {
			return offset
		}
	}
	return 0
}

// parseAdvancedConstructs parses advanced SPARQL constructs from WHERE clause.
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

		if strings.Contains(linesUpper, "UNION") {
			// Parse UNION construct
			leftPattern, newI := p.parseUnionBlock(lines, i)
			rightPattern, newI := p.parseUnionBlock(lines, newI)
			unions = append(unions, UnionBlock{Left: leftPattern, Right: rightPattern})
			i = newI
		} else if strings.Contains(linesUpper, "OPTIONAL") {
			// Parse OPTIONAL construct
			optionalPattern, newI := p.parseOptionalBlock(lines, i)
			optionals = append(optionals, OptionalBlock{Pattern: optionalPattern})
			i = newI
		} else if strings.Contains(linesUpper, "MINUS") {
			// Parse MINUS construct
			minusPattern, newI := p.parseMinusBlock(lines, i)
			minuses = append(minuses, minusPattern)
			i = newI
		} else if strings.Contains(linesUpper, "BIND") {
			// Parse BIND construct
			bind, newI := p.parseBindBlock(lines, i)
			binds = append(binds, bind)
			i = newI
		} else if strings.Contains(linesUpper, "VALUES") {
			// Parse VALUES construct
			valuesBlock, newI := p.parseValuesBlock(lines, i)
			values = append(values, valuesBlock)
			i = newI
		} else {
			// Parse as basic graph pattern
			bgp, newI := p.parseBasicGraphPatternFromLines(lines, i)
			patterns = append(patterns, bgp)
			i = newI
		}
	}

	return patterns, unions, optionals, minuses, binds, values
}

// parseUnionBlock parses a UNION block.
func (p *Parser) parseUnionBlock(lines []string, start int) (BasicGraphPattern, int) {
	// Skip the UNION keyword
	i := start + 1
	return p.parseBasicGraphPatternFromLines(lines, i)
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
func (p *Parser) parseBindBlock(lines []string, start int) (BindBlock, int) {
	// Simple BIND parsing: BIND(?var AS ?expression)
	_ = strings.TrimSpace(lines[start])
	// For now, return a simple stub
	return BindBlock{
		Variable:   "?bound",
		Expression: "?value",
	}, start + 1
}

// parseValuesBlock parses a VALUES clause.
func (p *Parser) parseValuesBlock(lines []string, start int) (ValuesBlock, int) {
	// Simple VALUES parsing: VALUES ?var { "val1" "val2" }
	_ = strings.TrimSpace(lines[start])
	// For now, return a simple stub
	return ValuesBlock{
		Variables: []string{"?var"},
		Values:    [][]string{{"val1"}, {"val2"}},
	}, start + 1
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
