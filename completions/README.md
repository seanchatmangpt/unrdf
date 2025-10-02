# UNRDF Shell Completions

Tab completions for the UNRDF CLI across bash, zsh, and fish shells.

## Quick Install

```bash
# Auto-detect shell and install
./install.sh

# Or specify shell explicitly
./install.sh bash
./install.sh zsh
./install.sh fish
```

## Manual Installation

### Bash

```bash
# Option 1: Source directly
source bash-completion.sh

# Option 2: Add to ~/.bashrc
echo "source /path/to/unrdf/completions/bash-completion.sh" >> ~/.bashrc
source ~/.bashrc

# Option 3: System-wide (requires sudo)
sudo cp bash-completion.sh /etc/bash_completion.d/unrdf
```

### Zsh

```bash
# Option 1: Copy to completions directory
mkdir -p ~/.zsh/completions
cp zsh-completion.zsh ~/.zsh/completions/_unrdf

# Add to ~/.zshrc:
fpath=(~/.zsh/completions $fpath)
autoload -Uz compinit && compinit

# Reload
source ~/.zshrc
```

### Fish

```bash
# Copy to fish completions directory
cp fish-completion.fish ~/.config/fish/completions/unrdf.fish

# Reload completions
fish_update_completions
```

## Usage

After installation, use Tab key to complete commands:

```bash
# Complete resources
unrdf <TAB>
# → graph  hook  policy  sidecar  store  context  plugin  completion

# Complete verbs
unrdf graph <TAB>
# → list  get  create  update  delete  validate  export  describe

# Complete flags
unrdf graph list --<TAB>
# → --help  --version  --config  --format  --output  --json  --yaml  --table
```

## Supported Commands

### Resources (Nouns)
- `graph` - Manage RDF graphs
- `hook` - Manage knowledge hooks
- `policy` - Manage policy packs
- `sidecar` - Manage KGC sidecar
- `store` - Manage RDF store
- `context` - Manage CLI contexts
- `plugin` - Manage CLI plugins
- `completion` - Generate shell completions

### Verbs
- `list` - List resources
- `get` - Get resource details
- `create` - Create new resource
- `update` - Update existing resource
- `delete` - Delete resource
- `apply` - Apply configuration
- `validate` - Validate resource
- `test` - Test resource
- `describe` - Show detailed information
- `export` - Export data
- `import` - Import data
- `eval` - Evaluate (hooks)
- `history` - Show history
- `status` - Show status
- `logs` - Show logs
- `config` - Manage configuration
- `restart` - Restart service
- `health` - Health check
- `query` - Query data
- `stats` - Show statistics
- `backup` - Backup data
- `restore` - Restore from backup
- `use` - Switch context
- `current` - Show current context
- `install` - Install plugin

### Common Flags
- `--help` - Show help information
- `--version` - Show version information
- `--config` - Configuration file path
- `--format` - Output format (json, yaml, table)
- `--output` - Output file path
- `--verbose` - Verbose output
- `--quiet` - Quiet mode
- `--json` - JSON output
- `--yaml` - YAML output
- `--table` - Table output

## Testing Completions

```bash
# Test in bash
bash -c "source bash-completion.sh && complete -p unrdf"

# Test in zsh
zsh -c "source zsh-completion.zsh && compdef -d unrdf"

# Test in fish
fish -c "source fish-completion.fish && complete -C'unrdf '"
```

## Updating Completions

When new commands are added to the CLI:

1. Edit the appropriate completion file(s)
2. Test the changes locally
3. Reinstall: `./install.sh`
4. Verify: `unrdf <TAB>`

## Troubleshooting

### Completions not working in Bash
```bash
# Check if bash-completion is installed
dpkg -l | grep bash-completion  # Debian/Ubuntu
brew list | grep bash-completion  # macOS

# Install if missing
sudo apt install bash-completion  # Debian/Ubuntu
brew install bash-completion@2    # macOS
```

### Completions not working in Zsh
```bash
# Rebuild completion cache
rm -f ~/.zcompdump
autoload -Uz compinit && compinit

# Check fpath
echo $fpath
```

### Completions not working in Fish
```bash
# Rebuild completions database
fish_update_completions

# Check completions directory
ls -la ~/.config/fish/completions/
```

## Contributing

To add new completions:

1. Update all three shell completion files
2. Test on each shell
3. Update this README
4. Submit PR

## License

MIT License - See LICENSE file for details
