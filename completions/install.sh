#!/usr/bin/env bash
# UNRDF Shell Completion Installer
# Detects shell and installs appropriate completion script

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Detect shell
detect_shell() {
  if [ -n "$BASH_VERSION" ]; then
    echo "bash"
  elif [ -n "$ZSH_VERSION" ]; then
    echo "zsh"
  elif [ -n "$FISH_VERSION" ]; then
    echo "fish"
  else
    # Fallback to $SHELL
    basename "$SHELL"
  fi
}

# Install bash completion
install_bash() {
  echo -e "${YELLOW}Installing bash completion...${NC}"

  local completion_dir
  if [ -d "/etc/bash_completion.d" ]; then
    completion_dir="/etc/bash_completion.d"
  elif [ -d "/usr/local/etc/bash_completion.d" ]; then
    completion_dir="/usr/local/etc/bash_completion.d"
  elif [ -d "$HOME/.bash_completion.d" ]; then
    completion_dir="$HOME/.bash_completion.d"
  else
    mkdir -p "$HOME/.bash_completion.d"
    completion_dir="$HOME/.bash_completion.d"
  fi

  cp bash-completion.sh "$completion_dir/unrdf"

  # Add to .bashrc if not already present
  if ! grep -q "source.*bash-completion.d/unrdf" "$HOME/.bashrc" 2>/dev/null; then
    echo "" >> "$HOME/.bashrc"
    echo "# UNRDF completion" >> "$HOME/.bashrc"
    echo "source $completion_dir/unrdf" >> "$HOME/.bashrc"
  fi

  echo -e "${GREEN}✓ Bash completion installed to $completion_dir/unrdf${NC}"
  echo -e "${YELLOW}Run: source ~/.bashrc${NC}"
}

# Install zsh completion
install_zsh() {
  echo -e "${YELLOW}Installing zsh completion...${NC}"

  local completion_dir="$HOME/.zsh/completions"
  mkdir -p "$completion_dir"

  cp zsh-completion.zsh "$completion_dir/_unrdf"

  # Add to .zshrc if not already present
  if ! grep -q "fpath=.*\.zsh/completions" "$HOME/.zshrc" 2>/dev/null; then
    echo "" >> "$HOME/.zshrc"
    echo "# UNRDF completion" >> "$HOME/.zshrc"
    echo "fpath=($completion_dir \$fpath)" >> "$HOME/.zshrc"
    echo "autoload -Uz compinit && compinit" >> "$HOME/.zshrc"
  fi

  echo -e "${GREEN}✓ Zsh completion installed to $completion_dir/_unrdf${NC}"
  echo -e "${YELLOW}Run: source ~/.zshrc${NC}"
}

# Install fish completion
install_fish() {
  echo -e "${YELLOW}Installing fish completion...${NC}"

  local completion_dir="$HOME/.config/fish/completions"
  mkdir -p "$completion_dir"

  cp fish-completion.fish "$completion_dir/unrdf.fish"

  echo -e "${GREEN}✓ Fish completion installed to $completion_dir/unrdf.fish${NC}"
  echo -e "${YELLOW}Restart fish shell or run: fish_update_completions${NC}"
}

# Main installation
main() {
  local shell="${1:-$(detect_shell)}"

  echo -e "${GREEN}UNRDF Shell Completion Installer${NC}"
  echo -e "Detected shell: ${YELLOW}$shell${NC}"
  echo ""

  case "$shell" in
    bash)
      install_bash
      ;;
    zsh)
      install_zsh
      ;;
    fish)
      install_fish
      ;;
    *)
      echo -e "${RED}Unsupported shell: $shell${NC}"
      echo "Supported shells: bash, zsh, fish"
      exit 1
      ;;
  esac

  echo ""
  echo -e "${GREEN}Installation complete!${NC}"
  echo "Try typing: unrdf <TAB>"
}

# Run installer
main "$@"
