#!/usr/bin/env bash
# UNRDF Bash Completion Script
# Installation: source this file or add to ~/.bashrc
#   source /path/to/unrdf/completions/bash-completion.sh

_unrdf_completions() {
  local cur prev words cword
  _init_completion || return

  # Resources (nouns)
  local resources="graph hook policy sidecar store context plugin completion repl"

  # Graph verbs
  local graph_verbs="list get create update delete validate export describe"

  # Hook verbs
  local hook_verbs="list get create update delete eval history describe"

  # Policy verbs
  local policy_verbs="list get apply validate test describe"

  # Sidecar verbs
  local sidecar_verbs="status logs config restart health"

  # Store verbs
  local store_verbs="import export query stats backup restore"

  # Context verbs
  local context_verbs="list get use create delete current"

  # Plugin verbs
  local plugin_verbs="list install"

  # Completion shells
  local shells="bash zsh fish"

  # First argument - resource selection
  if [[ $cword -eq 1 ]]; then
    COMPREPLY=($(compgen -W "$resources" -- "$cur"))
    return
  fi

  # Second argument - verb selection based on resource
  if [[ $cword -eq 2 ]]; then
    case "${words[1]}" in
      graph)
        COMPREPLY=($(compgen -W "$graph_verbs" -- "$cur"))
        ;;
      hook)
        COMPREPLY=($(compgen -W "$hook_verbs" -- "$cur"))
        ;;
      policy)
        COMPREPLY=($(compgen -W "$policy_verbs" -- "$cur"))
        ;;
      sidecar)
        COMPREPLY=($(compgen -W "$sidecar_verbs" -- "$cur"))
        ;;
      store)
        COMPREPLY=($(compgen -W "$store_verbs" -- "$cur"))
        ;;
      context)
        COMPREPLY=($(compgen -W "$context_verbs" -- "$cur"))
        ;;
      plugin)
        COMPREPLY=($(compgen -W "$plugin_verbs" -- "$cur"))
        ;;
      completion)
        COMPREPLY=($(compgen -W "$shells" -- "$cur"))
        ;;
    esac
    return
  fi

  # Flag completion for all commands
  if [[ "$cur" == -* ]]; then
    local common_flags="--help --version --config --format --output --verbose --quiet --json --yaml --table"
    COMPREPLY=($(compgen -W "$common_flags" -- "$cur"))
    return
  fi

  # File completion for certain verbs
  case "${words[2]}" in
    import|export|backup|restore|create|update|apply)
      COMPREPLY=($(compgen -f -- "$cur"))
      ;;
  esac
}

# Register completion
complete -F _unrdf_completions unrdf
