// Package rdf provides RDF serialization and parsing utilities.
package rdf

import (
	"sort"
	"strings"

	"github.com/knakk/rdf"
)

// Serializer handles RDF serialization to various formats.
type Serializer struct {
	namespaces map[string]string
	prefixes   map[string]string
}

// NewSerializer creates a new RDF serializer.
func NewSerializer() *Serializer {
	return &Serializer{
		namespaces: make(map[string]string),
		prefixes:   make(map[string]string),
	}
}

// AddNamespace adds a namespace prefix for serialization.
func (s *Serializer) AddNamespace(prefix, namespace string) {
	s.namespaces[namespace] = prefix
	s.prefixes[prefix] = namespace
}

// ToTurtle serializes RDF triples to Turtle format.
func (s *Serializer) ToTurtle(triples []rdf.Triple) string {
	var result strings.Builder
	
	// Output namespace declarations
	if len(s.namespaces) > 0 {
		for _, prefix := range s.sortedPrefixes() {
			namespace := s.prefixes[prefix]
			result.WriteString("@prefix " + prefix + ": <" + namespace + "> .\n")
		}
		result.WriteString("\n")
	}
	
	// Group triples by subject for compact Turtle output
	subjectGroups := s.groupBySubject(triples)
	
	for _, subject := range s.sortedSubjects(subjectGroups) {
		triplets := subjectGroups[subject]
		if len(triplets) == 0 {
			continue
		}
		
		result.WriteString(subject + " ")
		
		// Output predicates and objects
		for i, triple := range triplets {
			predicate := s.shortenPredicate(triple.Pred.String())
			object := s.shortenObject(triple.Obj.String())
			
			result.WriteString(predicate + " " + object)
			
			if i < len(triplets)-1 {
				result.WriteString(" ;")
			}
			result.WriteString("\n")
		}
		
		result.WriteString(" .\n\n")
	}
	
	return result.String()
}

// ToNTriples serializes RDF triples to N-Triples format.
func (s *Serializer) ToNTriples(triples []rdf.Triple) string {
	var result strings.Builder
	
	for _, triple := range triples {
		result.WriteString(triple.Subj.String() + " ")
		result.WriteString(triple.Pred.String() + " ")
		result.WriteString(triple.Obj.String() + " .\n")
	}
	
	return result.String()
}

// ToNQuads serializes RDF triples to N-Quads format.
func (s *Serializer) ToNQuads(triples []rdf.Triple) string {
	var result strings.Builder
	
	for _, triple := range triples {
		result.WriteString(triple.Subj.String() + " ")
		result.WriteString(triple.Pred.String() + " ")
		result.WriteString(triple.Obj.String() + " <" + s.getGraph(triple) + "> .\n")
	}
	
	return result.String()
}

// ToJsonLd serializes RDF triples to JSON-LD format.
func (s *Serializer) ToJsonLd(triples []rdf.Triple) string {
	// Create a simple JSON-LD representation
	var result strings.Builder
	
	result.WriteString("[\n")
	result.WriteString("  {\n")
	result.WriteString("    \"@context\": {\n")
	
	// Add context mappings
	for _, prefix := range s.sortedPrefixes() {
		namespace := s.prefixes[prefix]
		result.WriteString("      \"" + prefix + "\": \"" + namespace + "\"")
		if prefix != s.lastPrefix() {
			result.WriteString(",")
		}
		result.WriteString("\n")
	}
	
	result.WriteString("    },\n")
	result.WriteString("    \"@graph\": [\n")
	
	// Group by subject for clean JSON-LD output
	subjectGroups := s.groupBySubject(triples)
	tripleCount := 0
	
	for _, subject := range s.sortedSubjects(subjectGroups) {
		triplets := subjectGroups[subject]
		if len(triplets) == 0 {
			continue
		}
		
		result.WriteString("      {\n")
		result.WriteString("        \"@id\": \"" + s.shortenSubject(subject) + "\",\n")
		
		// Group predicates and objects
		predicateGroups := s.groupByPredicate(triplets)
		propCount := 0
		
		for _, predicate := range s.sortedPredicates(predicateGroups) {
			objects := predicateGroups[predicate]
			predShort := s.shortenPredicateForJson(predicate)
			
			result.WriteString("        \"" + predShort + "\": ")
			
			if len(objects) == 1 {
				result.WriteString("\"" + s.shortenObjectForJson(objects[0].Obj.String()) + "\"")
			} else {
				result.WriteString("[\n")
				for i, obj := range objects {
					result.WriteString("          \"" + s.shortenObjectForJson(obj.Obj.String()) + "\"")
					if i < len(objects)-1 {
						result.WriteString(",")
					}
					result.WriteString("\n")
				}
				result.WriteString("        ]")
			}
			
			propCount++
			if propCount < len(predicateGroups) {
				result.WriteString(",")
			}
			result.WriteString("\n")
		}
		
		result.WriteString("      }")
		tripleCount++
		if tripleCount < len(subjectGroups) {
			result.WriteString(",")
		}
		result.WriteString("\n")
	}
	
	result.WriteString("    ]\n")
	result.WriteString("  }\n")
	result.WriteString("]\n")
	
	return result.String()
}

// Helper methods for serialization

func (s *Serializer) groupBySubject(triples []rdf.Triple) map[string][]rdf.Triple {
	groups := make(map[string][]rdf.Triple)
	for _, triple := range triples {
		subject := triple.Subj.String()
		groups[subject] = append(groups[subject], triple)
	}
	return groups
}

func (s *Serializer) groupByPredicate(triples []rdf.Triple) map[string][]rdf.Triple {
	groups := make(map[string][]rdf.Triple)
	for _, triple := range triples {
		predicate := triple.Pred.String()
		groups[predicate] = append(groups[predicate], triple)
	}
	return groups
}

func (s *Serializer) sortedSubjects(groups map[string][]rdf.Triple) []string {
	var subjects []string
	for subject := range groups {
		subjects = append(subjects, subject)
	}
	sort.Strings(subjects)
	return subjects
}

func (s *Serializer) sortedPredicates(groups map[string][]rdf.Triple) []string {
	var predicates []string
	for predicate := range groups {
		predicates = append(predicates, predicate)
	}
	sort.Strings(predicates)
	return predicates
}

func (s *Serializer) sortedPrefixes() []string {
	var prefixes []string
	for prefix := range s.prefixes {
		prefixes = append(prefixes, prefix)
	}
	sort.Strings(prefixes)
	return prefixes
}

func (s *Serializer) lastPrefix() string {
	prefixes := s.sortedPrefixes()
	if len(prefixes) == 0 {
		return ""
	}
	return prefixes[len(prefixes)-1]
}

func (s *Serializer) shortenSubject(subject string) string {
	return s.shortenIRI(subject)
}

func (s *Serializer) shortenPredicate(predicate string) string {
	return s.shortenIRI(predicate)
}

func (s *Serializer) shortenObject(object string) string {
	if strings.HasPrefix(object, "\"") {
		return object // Already a literal
	}
	return s.shortenIRI(object)
}

func (s *Serializer) shortenIRI(iri string) string {
	// Remove angle brackets
	if strings.HasPrefix(iri, "<") && strings.HasSuffix(iri, ">") {
		iri = iri[1 : len(iri)-1]
	}
	
	// Try to match against known namespaces
	for namespace, prefix := range s.namespaces {
		if strings.HasPrefix(iri, namespace) {
			localName := strings.TrimPrefix(iri, namespace)
			return prefix + ":" + localName
		}
	}
	
	return "<" + iri + ">"
}

func (s *Serializer) shortenPredicateForJson(predicate string) string {
	predicate = s.shortenPredicate(predicate)
	if strings.HasPrefix(predicate, "<") && strings.HasSuffix(predicate, ">") {
		return strings.Trim(predicate, "<>")
	}
	return predicate
}

func (s *Serializer) shortenObjectForJson(object string) string {
	if strings.HasPrefix(object, "\"") {
		// Extract literal value from quotes
		if strings.HasSuffix(object, "\"") {
			return strings.Trim(object, "\"")
		}
		return object
	}
	return s.shortenIRI(object)
}

func (s *Serializer) getGraph(triple rdf.Triple) string {
	// Default graph URI for N-Quads
	return "http://default.graph"
}
