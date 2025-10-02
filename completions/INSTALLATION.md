# Shell Completion Installation Guide

Quick reference for installing UNRDF shell completions.

## One-Line Install

```bash
cd /path/to/unrdf/completions && ./install.sh
```

## Manual Installation

### Bash

```bash
# Add to ~/.bashrc
echo "source /path/to/unrdf/completions/bash-completion.sh" >> ~/.bashrc
source ~/.bashrc

# Test
unrdf <TAB>
```

### Zsh

```bash
# Create completions directory
mkdir -p ~/.zsh/completions

# Copy completion file
cp /path/to/unrdf/completions/zsh-completion.zsh ~/.zsh/completions/_unrdf

# Add to ~/.zshrc
cat >> ~/.zshrc << 'ZSHRC'
fpath=(~/.zsh/completions $fpath)
autoload -Uz compinit && compinit
ZSHRC

# Reload
source ~/.zshrc

# Test
unrdf <TAB>
```

### Fish

```bash
# Copy to fish completions directory
cp /path/to/unrdf/completions/fish-completion.fish ~/.config/fish/completions/unrdf.fish

# Update completions
fish_update_completions

# Test (in new fish shell)
unrdf <TAB>
```

## Verify Installation

All shells should now complete:

```bash
unrdf <TAB>
# → graph  hook  policy  sidecar  store  context  plugin  completion  repl

unrdf graph <TAB>
# → list  get  create  update  delete  validate  export  describe

unrdf hook <TAB>
# → list  get  create  update  delete  eval  history  describe
```
