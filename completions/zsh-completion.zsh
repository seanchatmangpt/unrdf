#compdef unrdf
# UNRDF Zsh Completion Script
# Installation: Add to ~/.zshrc:
#   fpath=(/path/to/unrdf/completions $fpath)
#   autoload -Uz compinit && compinit

_unrdf() {
  local line state

  _arguments -C \
    '1: :->resource' \
    '2: :->verb' \
    '*::arg:->args' \
    '--help[Show help information]' \
    '--version[Show version information]' \
    '--config[Configuration file path]:file:_files' \
    '--format[Output format]:format:(json yaml table)' \
    '--output[Output file]:file:_files' \
    '--verbose[Verbose output]' \
    '--quiet[Quiet mode]' \
    '--json[JSON output]' \
    '--yaml[YAML output]' \
    '--table[Table output]'

  case $state in
    resource)
      _arguments '1:resource:(graph hook policy sidecar store context plugin completion repl)'
      ;;
    verb)
      case $line[1] in
        graph)
          _arguments '2:verb:(list get create update delete validate export describe)'
          ;;
        hook)
          _arguments '2:verb:(list get create update delete eval history describe)'
          ;;
        policy)
          _arguments '2:verb:(list get apply validate test describe)'
          ;;
        sidecar)
          _arguments '2:verb:(status logs config restart health)'
          ;;
        store)
          _arguments '2:verb:(import export query stats backup restore)'
          ;;
        context)
          _arguments '2:verb:(list get use create delete current)'
          ;;
        plugin)
          _arguments '2:verb:(list install)'
          ;;
        completion)
          _arguments '2:shell:(bash zsh fish)'
          ;;
      esac
      ;;
    args)
      case $line[2] in
        import|export|backup|restore|create|update|apply)
          _files
          ;;
        *)
          _files
          ;;
      esac
      ;;
  esac
}

_unrdf "$@"
