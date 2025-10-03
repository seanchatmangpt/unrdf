package main

import (
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// JiraTicket represents a single JIRA ticket with validation status
type JiraTicket struct {
	ID                  string
	Title               string
	Priority            string
	Epic                string
	Estimate            int
	AcceptanceCriteria  []string
	TestCases           []string
	Status              string // "implemented", "partial", "not_implemented", "unknown"
	ImplementationNotes string
}

// JiraSystem manages JIRA ticket validation and reporting
type JiraSystem struct {
	Tickets []JiraTicket
}

// NewJiraSystem creates a new JIRA validation system
func NewJiraSystem() *JiraSystem {
	return &JiraSystem{
		Tickets: make([]JiraTicket, 0),
	}
}

// LoadTicketsFromMarkdown parses JIRA tickets from the markdown documentation
func (js *JiraSystem) LoadTicketsFromMarkdown() error {
	docsPath := "docs/jira/feature-parity-tickets.md"

	content, err := os.ReadFile(docsPath)
	if err != nil {
		return fmt.Errorf("failed to read JIRA tickets file: %w", err)
	}

	// Parse tickets using regex patterns
	ticketPattern := regexp.MustCompile(`(?s)### (KNOWD-\d+): (.+?)\n\*\*Priority:\*\* (.+?)\n\*\*Epic:\*\* (.+?)\n\*\*Estimate:\*\* (\d+) points`)

	// Find all ticket matches
	allMatches := ticketPattern.FindAllStringSubmatch(string(content), -1)

	for _, matches := range allMatches {
		if len(matches) >= 6 {
			ticket := JiraTicket{
				ID:       matches[1],
				Title:    matches[2],
				Priority: matches[3],
				Epic:     matches[4],
				Estimate: parseInt(matches[5]),
				Status:   "unknown",
			}

			js.Tickets = append(js.Tickets, ticket)
		}
	}

	return nil
}

// ValidateFeatureParity checks current implementation against JIRA requirements
func (js *JiraSystem) ValidateFeatureParity() error {
	// Check each ticket for implementation status
	for i := range js.Tickets {
		ticket := &js.Tickets[i]

		switch ticket.ID {
		case "KNOWD-101":
			ticket.Status = js.validateCoreEngine()
		case "KNOWD-102":
			ticket.Status = js.validateKnowledgeHooks()
		case "KNOWD-103":
			ticket.Status = js.validateSHACL()
		case "KNOWD-104":
			ticket.Status = js.validateLockchain()
		case "KNOWD-105":
			ticket.Status = js.validateRESTAPI()
		case "KNOWD-106":
			ticket.Status = js.validateObservability()
		case "KNOWD-107":
			ticket.Status = js.validateNamespaces()
		case "KNOWD-108":
			ticket.Status = js.validateCluster()
		case "KNOWD-109":
			ticket.Status = js.validateVectorSearch()
		case "KNOWD-110":
			ticket.Status = js.validateWASM()
		case "KNOWD-111":
			ticket.Status = js.validateRemoteStorage()
		case "KNOWD-112":
			ticket.Status = js.validatePerformance()
		case "KNOWD-113":
			ticket.Status = js.validateSecurity()
		case "KNOWD-114":
			ticket.Status = js.validateMTLS()
		case "KNOWD-115":
			ticket.Status = js.validateIntegrationTests()
		case "KNOWD-116":
			ticket.Status = js.validateMigration()
		case "KNOWD-117":
			ticket.Status = js.validateDocumentation()
		case "KNOWD-118":
			ticket.Status = js.validateBenchmarks()
		default:
			ticket.Status = "unknown"
		}
	}

	return nil
}

// Helper function to parse integers safely
func parseInt(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		return 0
	}
	return i
}

// Validation methods for each ticket
func (js *JiraSystem) validateCoreEngine() string {
	// Check if RDF parsing and SPARQL execution are implemented
	if fileExists("internal/sparql/parser.go") && fileExists("internal/sparql/exec.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateKnowledgeHooks() string {
	// Check if hooks system is implemented
	if fileExists("internal/hooks/hooks.go") && fileExists("internal/hooks/batch.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateSHACL() string {
	// Check if SHACL validation is implemented
	if fileExists("internal/shacl/validator.go") && fileExists("internal/shacl/shapes.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateLockchain() string {
	// Check if cryptographic provenance is implemented
	if fileExists("internal/lockchain/lockchain.go") && fileExists("internal/lockchain/sign.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateRESTAPI() string {
	// Check if HTTP REST API is implemented
	if fileExists("internal/server/http.go") && fileExists("internal/server/routes.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateObservability() string {
	// Check if telemetry/observability is implemented
	if fileExists("internal/telemetry/otel.go") && fileExists("internal/telemetry/metrics.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateNamespaces() string {
	// Check if namespace isolation is implemented
	if fileExists("internal/namespace/ns.go") && fileExists("internal/namespace/middleware.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateCluster() string {
	// Check if cluster functionality is implemented
	if fileExists("internal/cluster/leader.go") && fileExists("internal/cluster/follower.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateVectorSearch() string {
	// Check if vector search is implemented
	if fileExists("internal/vec/hnsw.go") && fileExists("internal/vec/index.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateWASM() string {
	// Check if WASM execution is implemented
	if fileExists("internal/wasm/runtime.go") && fileExists("internal/wasm/loader.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateRemoteStorage() string {
	// Check if remote storage is implemented
	if fileExists("internal/store/disk/objstore.go") && fileExists("internal/store/disk/encrypt.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validatePerformance() string {
	// Check if performance optimizations are implemented
	if fileExists("internal/sparql/plan_cache.go") && fileExists("internal/hooks/batch.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateSecurity() string {
	// Check if security features are implemented
	if fileExists("internal/auth/mtls.go") && fileExists("internal/auth/tokens.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateMTLS() string {
	// Check if mTLS authentication is implemented
	if fileExists("internal/auth/mtls.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateIntegrationTests() string {
	// Check if integration tests exist
	if fileExists("integration_test.go") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateMigration() string {
	// Check if migration documentation exists
	if fileExists("docs/migration.md") || strings.Contains(getReadmeContent(), "migration") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateDocumentation() string {
	// Check if comprehensive documentation exists
	if fileExists("README.md") && fileExists("docs/README.md") {
		return "implemented"
	}
	return "not_implemented"
}

func (js *JiraSystem) validateBenchmarks() string {
	// Check if benchmark tests exist
	benchmarkFiles := []string{
		"internal/sparql/parser_bench_test.go",
		"internal/hooks/hooks_bench_test.go",
		"internal/store/memory_bench_test.go",
	}

	for _, file := range benchmarkFiles {
		if fileExists(file) {
			return "implemented"
		}
	}
	return "not_implemented"
}

// Helper function to check if file exists
func fileExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

// Helper function to get README content
func getReadmeContent() string {
	content, err := os.ReadFile("README.md")
	if err != nil {
		return ""
	}
	return string(content)
}

// GenerateReport creates a feature parity validation report
func (js *JiraSystem) GenerateReport() string {
	var report strings.Builder

	report.WriteString("# JIRA Feature Parity Validation Report\n\n")
	report.WriteString("Generated by knowd JIRA WIP tracking system\n\n")

	// Summary statistics
	total := len(js.Tickets)
	implemented := 0
	partial := 0
	notImplemented := 0

	for _, ticket := range js.Tickets {
		switch ticket.Status {
		case "implemented":
			implemented++
		case "partial":
			partial++
		case "not_implemented":
			notImplemented++
		}
	}

	report.WriteString("## Summary\n\n")
	report.WriteString(fmt.Sprintf("| Status | Count | Percentage |\n"))
	report.WriteString(fmt.Sprintf("|--------|-------|------------|\n"))
	report.WriteString(fmt.Sprintf("| ✅ Implemented | %d | %.1f%% |\n", implemented, float64(implemented)/float64(total)*100))
	report.WriteString(fmt.Sprintf("| 🔄 Partial | %d | %.1f%% |\n", partial, float64(partial)/float64(total)*100))
	report.WriteString(fmt.Sprintf("| ❌ Not Implemented | %d | %.1f%% |\n", notImplemented, float64(notImplemented)/float64(total)*100))
	report.WriteString(fmt.Sprintf("| **Total** | %d | **100%%** |\n\n", total))

	// Detailed ticket status
	report.WriteString("## Detailed Status\n\n")
	for _, ticket := range js.Tickets {
		statusIcon := "❓"
		switch ticket.Status {
		case "implemented":
			statusIcon = "✅"
		case "partial":
			statusIcon = "🔄"
		case "not_implemented":
			statusIcon = "❌"
		}

		report.WriteString(fmt.Sprintf("### %s %s (%s)\n", statusIcon, ticket.ID, ticket.Title))
		report.WriteString(fmt.Sprintf("- **Priority:** %s\n", ticket.Priority))
		report.WriteString(fmt.Sprintf("- **Epic:** %s\n", ticket.Epic))
		report.WriteString(fmt.Sprintf("- **Estimate:** %d points\n", ticket.Estimate))
		report.WriteString(fmt.Sprintf("- **Status:** %s\n", ticket.Status))
		if ticket.ImplementationNotes != "" {
			report.WriteString(fmt.Sprintf("- **Notes:** %s\n", ticket.ImplementationNotes))
		}
		report.WriteString("\n")
	}

	return report.String()
}

// UpdateTicketStatus allows manual status updates
func (js *JiraSystem) UpdateTicketStatus(ticketID, status, notes string) error {
	for i := range js.Tickets {
		if js.Tickets[i].ID == ticketID {
			js.Tickets[i].Status = status
			js.Tickets[i].ImplementationNotes = notes
			return nil
		}
	}
	return fmt.Errorf("ticket %s not found", ticketID)
}

// ExportTicketsToMarkdown exports current ticket status to markdown
func (js *JiraSystem) ExportTicketsToMarkdown() error {
	content := js.GenerateReport()

	// Write to a status file
	return os.WriteFile("docs/jira/feature-parity-status.md", []byte(content), 0644)
}
