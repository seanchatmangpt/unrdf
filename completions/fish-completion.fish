# UNRDF Fish Completion Script
# Installation: Copy to ~/.config/fish/completions/unrdf.fish
#   cp fish-completion.fish ~/.config/fish/completions/unrdf.fish

# Remove default file completion
complete -c unrdf -e

# Global flags
complete -c unrdf -l help -d "Show help information"
complete -c unrdf -l version -d "Show version information"
complete -c unrdf -l config -d "Configuration file path" -r
complete -c unrdf -l format -d "Output format" -a "json yaml table"
complete -c unrdf -l output -d "Output file" -r
complete -c unrdf -l verbose -d "Verbose output"
complete -c unrdf -l quiet -d "Quiet mode"
complete -c unrdf -l json -d "JSON output"
complete -c unrdf -l yaml -d "YAML output"
complete -c unrdf -l table -d "Table output"

# Resource completions (first argument)
complete -c unrdf -n "__fish_use_subcommand" -a "graph" -d "Manage RDF graphs"
complete -c unrdf -n "__fish_use_subcommand" -a "hook" -d "Manage knowledge hooks"
complete -c unrdf -n "__fish_use_subcommand" -a "policy" -d "Manage policy packs"
complete -c unrdf -n "__fish_use_subcommand" -a "sidecar" -d "Manage KGC sidecar"
complete -c unrdf -n "__fish_use_subcommand" -a "store" -d "Manage RDF store"
complete -c unrdf -n "__fish_use_subcommand" -a "context" -d "Manage CLI contexts"
complete -c unrdf -n "__fish_use_subcommand" -a "plugin" -d "Manage CLI plugins"
complete -c unrdf -n "__fish_use_subcommand" -a "completion" -d "Generate shell completions"
complete -c unrdf -n "__fish_use_subcommand" -a "repl" -d "Start interactive SPARQL REPL"

# Graph verbs
complete -c unrdf -n "__fish_seen_subcommand_from graph" -a "list" -d "List graphs"
complete -c unrdf -n "__fish_seen_subcommand_from graph" -a "get" -d "Get graph details"
complete -c unrdf -n "__fish_seen_subcommand_from graph" -a "create" -d "Create new graph"
complete -c unrdf -n "__fish_seen_subcommand_from graph" -a "update" -d "Update graph"
complete -c unrdf -n "__fish_seen_subcommand_from graph" -a "delete" -d "Delete graph"
complete -c unrdf -n "__fish_seen_subcommand_from graph" -a "validate" -d "Validate graph"
complete -c unrdf -n "__fish_seen_subcommand_from graph" -a "export" -d "Export graph"
complete -c unrdf -n "__fish_seen_subcommand_from graph" -a "describe" -d "Describe graph"

# Hook verbs
complete -c unrdf -n "__fish_seen_subcommand_from hook" -a "list" -d "List hooks"
complete -c unrdf -n "__fish_seen_subcommand_from hook" -a "get" -d "Get hook details"
complete -c unrdf -n "__fish_seen_subcommand_from hook" -a "create" -d "Create new hook"
complete -c unrdf -n "__fish_seen_subcommand_from hook" -a "update" -d "Update hook"
complete -c unrdf -n "__fish_seen_subcommand_from hook" -a "delete" -d "Delete hook"
complete -c unrdf -n "__fish_seen_subcommand_from hook" -a "eval" -d "Evaluate hook"
complete -c unrdf -n "__fish_seen_subcommand_from hook" -a "history" -d "Hook execution history"
complete -c unrdf -n "__fish_seen_subcommand_from hook" -a "describe" -d "Describe hook"

# Policy verbs
complete -c unrdf -n "__fish_seen_subcommand_from policy" -a "list" -d "List policies"
complete -c unrdf -n "__fish_seen_subcommand_from policy" -a "get" -d "Get policy details"
complete -c unrdf -n "__fish_seen_subcommand_from policy" -a "apply" -d "Apply policy pack"
complete -c unrdf -n "__fish_seen_subcommand_from policy" -a "validate" -d "Validate policy"
complete -c unrdf -n "__fish_seen_subcommand_from policy" -a "test" -d "Test policy"
complete -c unrdf -n "__fish_seen_subcommand_from policy" -a "describe" -d "Describe policy"

# Sidecar verbs
complete -c unrdf -n "__fish_seen_subcommand_from sidecar" -a "status" -d "Show sidecar status"
complete -c unrdf -n "__fish_seen_subcommand_from sidecar" -a "logs" -d "Show sidecar logs"
complete -c unrdf -n "__fish_seen_subcommand_from sidecar" -a "config" -d "Manage sidecar config"
complete -c unrdf -n "__fish_seen_subcommand_from sidecar" -a "restart" -d "Restart sidecar"
complete -c unrdf -n "__fish_seen_subcommand_from sidecar" -a "health" -d "Check sidecar health"

# Store verbs
complete -c unrdf -n "__fish_seen_subcommand_from store" -a "import" -d "Import data"
complete -c unrdf -n "__fish_seen_subcommand_from store" -a "export" -d "Export data"
complete -c unrdf -n "__fish_seen_subcommand_from store" -a "query" -d "Query store"
complete -c unrdf -n "__fish_seen_subcommand_from store" -a "stats" -d "Show statistics"
complete -c unrdf -n "__fish_seen_subcommand_from store" -a "backup" -d "Backup store"
complete -c unrdf -n "__fish_seen_subcommand_from store" -a "restore" -d "Restore from backup"

# Context verbs
complete -c unrdf -n "__fish_seen_subcommand_from context" -a "list" -d "List contexts"
complete -c unrdf -n "__fish_seen_subcommand_from context" -a "get" -d "Get context details"
complete -c unrdf -n "__fish_seen_subcommand_from context" -a "use" -d "Switch context"
complete -c unrdf -n "__fish_seen_subcommand_from context" -a "create" -d "Create context"
complete -c unrdf -n "__fish_seen_subcommand_from context" -a "delete" -d "Delete context"
complete -c unrdf -n "__fish_seen_subcommand_from context" -a "current" -d "Show current context"

# Plugin verbs
complete -c unrdf -n "__fish_seen_subcommand_from plugin" -a "list" -d "List plugins"
complete -c unrdf -n "__fish_seen_subcommand_from plugin" -a "install" -d "Install plugin"

# Completion shells
complete -c unrdf -n "__fish_seen_subcommand_from completion" -a "bash" -d "Bash completion"
complete -c unrdf -n "__fish_seen_subcommand_from completion" -a "zsh" -d "Zsh completion"
complete -c unrdf -n "__fish_seen_subcommand_from completion" -a "fish" -d "Fish completion"
