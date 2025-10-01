/**
 * @file Shell Completion Generator
 * @module cli-v2/core/completion
 *
 * @description
 * Generates shell completion scripts for bash, zsh, and fish.
 */

/**
 * Generate shell completion script
 * @param {string} shell - Shell type (bash, zsh, fish)
 * @returns {string} Completion script
 */
export function generateCompletion(shell) {
  switch (shell) {
    case 'bash':
      return generateBashCompletion();
    case 'zsh':
      return generateZshCompletion();
    case 'fish':
      return generateFishCompletion();
    default:
      throw new Error(`Unsupported shell: ${shell}`);
  }
}

/**
 * Generate bash completion script
 * @returns {string} Bash completion script
 */
function generateBashCompletion() {
  return `# unrdf bash completion

_unrdf_completion() {
    local cur prev words cword
    _init_completion || return

    local nouns="graph hook policy sidecar store context plugin completion"
    local verbs="list get create update delete apply validate describe export import"

    if [ $cword -eq 1 ]; then
        COMPREPLY=($(compgen -W "$nouns" -- "$cur"))
        return 0
    fi

    if [ $cword -eq 2 ]; then
        COMPREPLY=($(compgen -W "$verbs" -- "$cur"))
        return 0
    fi
}

complete -F _unrdf_completion unrdf
`;
}

/**
 * Generate zsh completion script
 * @returns {string} Zsh completion script
 */
function generateZshCompletion() {
  return `#compdef unrdf

_unrdf() {
    local -a nouns verbs
    nouns=(
        'graph:Manage RDF graphs'
        'hook:Manage knowledge hooks'
        'policy:Manage policy packs'
        'sidecar:Manage KGC sidecar'
        'store:Manage RDF store'
        'context:Manage CLI contexts'
        'plugin:Manage CLI plugins'
        'completion:Generate shell completion'
    )
    verbs=(
        'list:List resources'
        'get:Get resource details'
        'create:Create new resource'
        'update:Update existing resource'
        'delete:Delete resource'
        'apply:Apply configuration'
        'validate:Validate resource'
        'describe:Show detailed information'
    )

    if (( CURRENT == 2 )); then
        _describe 'noun' nouns
    elif (( CURRENT == 3 )); then
        _describe 'verb' verbs
    fi
}

_unrdf
`;
}

/**
 * Generate fish completion script
 * @returns {string} Fish completion script
 */
function generateFishCompletion() {
  return `# unrdf fish completion

complete -c unrdf -f

# Nouns
complete -c unrdf -n '__fish_use_subcommand' -a graph -d 'Manage RDF graphs'
complete -c unrdf -n '__fish_use_subcommand' -a hook -d 'Manage knowledge hooks'
complete -c unrdf -n '__fish_use_subcommand' -a policy -d 'Manage policy packs'
complete -c unrdf -n '__fish_use_subcommand' -a sidecar -d 'Manage KGC sidecar'
complete -c unrdf -n '__fish_use_subcommand' -a store -d 'Manage RDF store'
complete -c unrdf -n '__fish_use_subcommand' -a context -d 'Manage CLI contexts'
complete -c unrdf -n '__fish_use_subcommand' -a plugin -d 'Manage CLI plugins'

# Verbs
complete -c unrdf -n '__fish_seen_subcommand_from graph hook policy sidecar store context' -a list -d 'List resources'
complete -c unrdf -n '__fish_seen_subcommand_from graph hook policy sidecar store context' -a get -d 'Get resource details'
complete -c unrdf -n '__fish_seen_subcommand_from graph hook policy sidecar store context' -a create -d 'Create new resource'
complete -c unrdf -n '__fish_seen_subcommand_from graph hook policy sidecar store context' -a delete -d 'Delete resource'
complete -c unrdf -n '__fish_seen_subcommand_from graph hook policy sidecar store context' -a validate -d 'Validate resource'
`;
}
