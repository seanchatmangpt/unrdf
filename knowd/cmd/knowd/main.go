// Command knowd implements the main entrypoint for the knowd server with noun-verb CLI structure.
package main

import (
	"fmt"
	"log"
	"os"

	"github.com/unrdf/knowd/internal/version"
)

func main() {
	if len(os.Args) < 2 {
		printUsage()
		os.Exit(1)
	}

	noun := os.Args[1]
	verbs := os.Args[2:]

	// Route to appropriate command handler
	switch noun {
	case "server":
		handleServerCommands(verbs)
	case "data":
		handleDataCommands(verbs)
	case "policy":
		handlePolicyCommands(verbs)
	case "security":
		handleSecurityCommands(verbs)
	case "cluster":
		handleClusterCommands(verbs)
	case "ai":
		handleAICommands(verbs)
	case "monitoring":
		handleMonitoringCommands(verbs)
	case "backup":
		handleBackupCommands(verbs)
	case "help", "--help", "-h":
		printUsage()
	case "version", "--version", "-v":
		fmt.Printf("Knowd %s\n", version.BuildInfo())
	default:
		fmt.Fprintf(os.Stderr, "Error: Unknown noun '%s'\n\n", noun)
		printUsage()
		os.Exit(1)
	}
}

func printUsage() {
	fmt.Printf("Knowd - Knowledge Graph Database Server v%s\n\n", version.BuildInfo())
	fmt.Println("Usage: knowd <noun> <verb> [options...]")
	fmt.Println()
	fmt.Println("Nouns (categories of operations):")
	fmt.Println("  server     - Server lifecycle and management")
	fmt.Println("  data       - Data operations (query, validate, transact)")
	fmt.Println("  policy     - Policy and hook management")
	fmt.Println("  security   - Security and authentication")
	fmt.Println("  cluster    - Clustering and scaling")
	fmt.Println("  ai         - AI agent management")
	fmt.Println("  monitoring - Metrics and observability")
	fmt.Println("  backup     - Backup and recovery")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  knowd server start")
	fmt.Println("  knowd data query \"SELECT * WHERE { ?s ?p ?o }\"")
	fmt.Println("  knowd policy list")
	fmt.Println("  knowd cluster status")
	fmt.Println()
	fmt.Println("Use 'knowd <noun> help' for detailed help on each noun")
	fmt.Println("Use 'knowd help' or 'knowd --help' for this help")
}

// Command handler functions for each noun
func handleServerCommands(verbs []string) {
	if len(verbs) == 0 {
		fmt.Println("Error: server command requires a verb")
		fmt.Println("Usage: knowd server <verb>")
		fmt.Println("Verbs: start, stop, status, restart")
		os.Exit(1)
	}

	verb := verbs[0]
	switch verb {
	case "start":
		startServer()
	case "stop":
		stopServer()
	case "status":
		serverStatus()
	case "restart":
		restartServer()
	case "help":
		printServerHelp()
	default:
		fmt.Fprintf(os.Stderr, "Error: Unknown server verb '%s'\n\n", verb)
		printServerHelp()
		os.Exit(1)
	}
}

func handleDataCommands(verbs []string) {
	if len(verbs) == 0 {
		fmt.Println("Error: data command requires a verb")
		fmt.Println("Usage: knowd data <verb>")
		fmt.Println("Verbs: query, transact, validate, import, export")
		os.Exit(1)
	}

	verb := verbs[0]
	switch verb {
	case "query":
		queryData(verbs[1:])
	case "transact":
		transactData(verbs[1:])
	case "validate":
		validateData(verbs[1:])
	case "import":
		importData(verbs[1:])
	case "export":
		exportData(verbs[1:])
	case "help":
		printDataHelp()
	default:
		fmt.Fprintf(os.Stderr, "Error: Unknown data verb '%s'\n\n", verb)
		printDataHelp()
		os.Exit(1)
	}
}

func handlePolicyCommands(verbs []string) {
	if len(verbs) == 0 {
		fmt.Println("Error: policy command requires a verb")
		fmt.Println("Usage: knowd policy <verb>")
		fmt.Println("Verbs: list, create, update, delete, enable, disable")
		os.Exit(1)
	}

	verb := verbs[0]
	switch verb {
	case "list":
		listPolicies()
	case "create":
		createPolicy(verbs[1:])
	case "update":
		updatePolicy(verbs[1:])
	case "delete":
		deletePolicy(verbs[1:])
	case "enable":
		enablePolicy(verbs[1:])
	case "disable":
		disablePolicy(verbs[1:])
	case "help":
		printPolicyHelp()
	default:
		fmt.Fprintf(os.Stderr, "Error: Unknown policy verb '%s'\n\n", verb)
		printPolicyHelp()
		os.Exit(1)
	}
}

func handleSecurityCommands(verbs []string) {
	if len(verbs) == 0 {
		fmt.Println("Error: security command requires a verb")
		fmt.Println("Usage: knowd security <verb>")
		fmt.Println("Verbs: auth, cert, token, audit")
		os.Exit(1)
	}

	verb := verbs[0]
	switch verb {
	case "auth":
		handleAuthCommands(verbs[1:])
	case "cert":
		handleCertCommands(verbs[1:])
	case "token":
		handleTokenCommands(verbs[1:])
	case "audit":
		handleAuditCommands(verbs[1:])
	case "help":
		printSecurityHelp()
	default:
		fmt.Fprintf(os.Stderr, "Error: Unknown security verb '%s'\n\n", verb)
		printSecurityHelp()
		os.Exit(1)
	}
}

func handleClusterCommands(verbs []string) {
	if len(verbs) == 0 {
		fmt.Println("Error: cluster command requires a verb")
		fmt.Println("Usage: knowd cluster <verb>")
		fmt.Println("Verbs: status, join, leave, promote, scale")
		os.Exit(1)
	}

	verb := verbs[0]
	switch verb {
	case "status":
		clusterStatus()
	case "join":
		joinCluster(verbs[1:])
	case "leave":
		leaveCluster()
	case "promote":
		promoteNode(verbs[1:])
	case "scale":
		scaleCluster(verbs[1:])
	case "help":
		printClusterHelp()
	default:
		fmt.Fprintf(os.Stderr, "Error: Unknown cluster verb '%s'\n\n", verb)
		printClusterHelp()
		os.Exit(1)
	}
}

func handleAICommands(verbs []string) {
	if len(verbs) == 0 {
		fmt.Println("Error: ai command requires a verb")
		fmt.Println("Usage: knowd ai <verb>")
		fmt.Println("Verbs: list, create, update, delete, execute")
		os.Exit(1)
	}

	verb := verbs[0]
	switch verb {
	case "list":
		listAIAgents()
	case "create":
		createAIAgent(verbs[1:])
	case "update":
		updateAIAgent(verbs[1:])
	case "delete":
		deleteAIAgent(verbs[1:])
	case "execute":
		executeAIAgent(verbs[1:])
	case "help":
		printAIHelp()
	default:
		fmt.Fprintf(os.Stderr, "Error: Unknown ai verb '%s'\n\n", verb)
		printAIHelp()
		os.Exit(1)
	}
}

func handleMonitoringCommands(verbs []string) {
	if len(verbs) == 0 {
		fmt.Println("Error: monitoring command requires a verb")
		fmt.Println("Usage: knowd monitoring <verb>")
		fmt.Println("Verbs: metrics, logs, health, alerts")
		os.Exit(1)
	}

	verb := verbs[0]
	switch verb {
	case "metrics":
		showMetrics(verbs[1:])
	case "logs":
		showLogs(verbs[1:])
	case "health":
		checkHealth()
	case "alerts":
		manageAlerts(verbs[1:])
	case "help":
		printMonitoringHelp()
	default:
		fmt.Fprintf(os.Stderr, "Error: Unknown monitoring verb '%s'\n\n", verb)
		printMonitoringHelp()
		os.Exit(1)
	}
}

func handleBackupCommands(verbs []string) {
	if len(verbs) == 0 {
		fmt.Println("Error: backup command requires a verb")
		fmt.Println("Usage: knowd backup <verb>")
		fmt.Println("Verbs: create, list, restore, schedule")
		os.Exit(1)
	}

	verb := verbs[0]
	switch verb {
	case "create":
		createBackup(verbs[1:])
	case "list":
		listBackups()
	case "restore":
		restoreBackup(verbs[1:])
	case "schedule":
		scheduleBackup(verbs[1:])
	case "help":
		printBackupHelp()
	default:
		fmt.Fprintf(os.Stderr, "Error: Unknown backup verb '%s'\n\n", verb)
		printBackupHelp()
		os.Exit(1)
	}
}

// Help functions for each noun
func printServerHelp() {
	fmt.Println("Server Commands:")
	fmt.Println("  knowd server start     - Start the Knowd server")
	fmt.Println("  knowd server stop      - Stop the Knowd server")
	fmt.Println("  knowd server status    - Check server status")
	fmt.Println("  knowd server restart   - Restart the server")
	fmt.Println()
	fmt.Println("Options:")
	fmt.Println("  --addr <addr>          - Server address (default: :8090)")
	fmt.Println("  --data-dir <dir>       - Data directory (default: ./data)")
	fmt.Println("  --store <type>         - Storage type (mem|disk)")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  knowd server start --addr :8080")
	fmt.Println("  knowd server start --data-dir /var/lib/knowd")
}

func printDataHelp() {
	fmt.Println("Data Commands:")
	fmt.Println("  knowd data query <sparql>    - Execute SPARQL query")
	fmt.Println("  knowd data transact <file>   - Submit transaction from file")
	fmt.Println("  knowd data validate <data> <shapes> - Validate data with SHACL")
	fmt.Println("  knowd data import <file>     - Import RDF data")
	fmt.Println("  knowd data export <format>   - Export data")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  knowd data query \"SELECT * WHERE { ?s ?p ?o }\"")
	fmt.Println("  knowd data transact transaction.json")
	fmt.Println("  knowd data validate data.ttl shapes.ttl")
}

func printPolicyHelp() {
	fmt.Println("Policy Commands:")
	fmt.Println("  knowd policy list           - List all policies")
	fmt.Println("  knowd policy create <file>  - Create policy from file")
	fmt.Println("  knowd policy update <id> <file> - Update policy")
	fmt.Println("  knowd policy delete <id>    - Delete policy")
	fmt.Println("  knowd policy enable <id>    - Enable policy")
	fmt.Println("  knowd policy disable <id>   - Disable policy")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  knowd policy list")
	fmt.Println("  knowd policy create policy.yaml")
	fmt.Println("  knowd policy enable security-policy")
}

func printSecurityHelp() {
	fmt.Println("Security Commands:")
	fmt.Println("  knowd security auth status     - Check authentication status")
	fmt.Println("  knowd security auth configure  - Configure authentication")
	fmt.Println("  knowd security cert generate   - Generate certificates")
	fmt.Println("  knowd security cert renew      - Renew certificates")
	fmt.Println("  knowd security token create    - Create API token")
	fmt.Println("  knowd security token list      - List API tokens")
	fmt.Println("  knowd security audit logs      - View audit logs")
	fmt.Println("  knowd security audit receipts  - View receipt audit")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  knowd security cert generate --ca")
	fmt.Println("  knowd security token create --name admin")
}

func printClusterHelp() {
	fmt.Println("Cluster Commands:")
	fmt.Println("  knowd cluster status          - Show cluster status")
	fmt.Println("  knowd cluster join <leader>   - Join cluster as follower")
	fmt.Println("  knowd cluster leave           - Leave cluster")
	fmt.Println("  knowd cluster promote <node>  - Promote follower to leader")
	fmt.Println("  knowd cluster scale <count>   - Scale cluster size")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  knowd cluster status")
	fmt.Println("  knowd cluster join leader:8090")
	fmt.Println("  knowd cluster scale 5")
}

func printAIHelp() {
	fmt.Println("AI Commands:")
	fmt.Println("  knowd ai list               - List AI agents")
	fmt.Println("  knowd ai create <config>    - Create AI agent")
	fmt.Println("  knowd ai update <id> <config> - Update AI agent")
	fmt.Println("  knowd ai delete <id>        - Delete AI agent")
	fmt.Println("  knowd ai execute <id> <input> - Execute AI agent")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  knowd ai list")
	fmt.Println("  knowd ai create agent-config.yaml")
	fmt.Println("  knowd ai execute my-agent \"analyze this data\"")
}

func printMonitoringHelp() {
	fmt.Println("Monitoring Commands:")
	fmt.Println("  knowd monitoring metrics          - Show system metrics")
	fmt.Println("  knowd monitoring logs <level>     - Show logs")
	fmt.Println("  knowd monitoring health           - Health check")
	fmt.Println("  knowd monitoring alerts list      - List alerts")
	fmt.Println("  knowd monitoring alerts create    - Create alert rule")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  knowd monitoring metrics")
	fmt.Println("  knowd monitoring logs info")
	fmt.Println("  knowd monitoring health")
}

func printBackupHelp() {
	fmt.Println("Backup Commands:")
	fmt.Println("  knowd backup create <name>    - Create backup")
	fmt.Println("  knowd backup list             - List backups")
	fmt.Println("  knowd backup restore <name>   - Restore from backup")
	fmt.Println("  knowd backup schedule <cron>  - Schedule automatic backups")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  knowd backup create daily-$(date +%Y%m%d)")
	fmt.Println("  knowd backup restore daily-20250101")
	fmt.Println("  knowd backup schedule \"0 2 * * *\"")
}
