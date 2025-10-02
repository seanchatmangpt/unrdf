# UNRDF VS Code Extension

Official Visual Studio Code extension for UNRDF Knowledge Hooks and Policy Packs.

## Features

- **Syntax Highlighting** for `.hook` and `.policy` files
- **Code Snippets** for common patterns
- **Command Integration** with UNRDF CLI
- **IntelliSense** for hook and policy functions
- **Validation** on save

## Installation

### From Marketplace (when published)

1. Open VS Code
2. Press `Cmd+Shift+X` (macOS) or `Ctrl+Shift+X` (Windows/Linux)
3. Search for "UNRDF"
4. Click Install

### From VSIX (development)

```bash
# Package extension
cd vscode-extension
npm run package

# Install
code --install-extension unrdf-vscode-0.1.0.vsix
```

### Development Mode

```bash
# Symlink for development
ln -s /path/to/unrdf/vscode-extension ~/.vscode/extensions/unrdf-vscode

# Reload VS Code
# Press Cmd+Shift+P → "Developer: Reload Window"
```

## Usage

### Syntax Highlighting

Automatic syntax highlighting for:
- `.hook` files - Knowledge Hook definitions
- `.hook.js` / `.hook.mjs` - JavaScript/ESM hooks
- `.policy` files - Policy Pack definitions
- `.policy.js` / `.policy.mjs` - JavaScript/ESM policies

### Code Snippets

Type prefix and press `Tab`:

#### Hook Snippets

- `defhook` - Define a basic hook
- `sparqlhook` - SPARQL query hook
- `valhook` - Validation hook with Zod
- `audithook` - Audit trail hook
- `chainhook` - Chain multiple hooks

Example:
```javascript
defhook<TAB>

// Expands to:
import { defineHook } from 'unrdf';

export const hookName = defineHook({
  name: 'hookName',
  description: 'Hook description',
  version: '1.0.0',

  triggers: {
    events: ['onCreate', 'onUpdate'],
    patterns: {
      subject: '?s',
      predicate: '?p',
      object: '?o'
    }
  },

  async execute(context) {
    const { quad, store, metadata } = context;

    // Hook logic here
    console.log('Hook executed', quad);

    return {
      success: true,
      modified: false
    };
  }
});
```

#### Policy Snippets

- `defpolicy` - Define policy pack
- `shaclpolicy` - SHACL validation rule
- `constraintpolicy` - Resource constraint rule
- `securitypolicy` - Security policy rule
- `compliancepolicy` - Compliance policy

Example:
```javascript
defpolicy<TAB>

// Expands to full policy pack structure
```

### Commands

Access via Command Palette (`Cmd+Shift+P` / `Ctrl+Shift+P`):

#### UNRDF: Validate Hook
Validates the current hook file using UNRDF CLI.

```
Cmd+Shift+P → UNRDF: Validate Hook
```

#### UNRDF: Evaluate Hook
Evaluates the current hook and shows results.

```
Cmd+Shift+P → UNRDF: Evaluate Hook
```

#### UNRDF: Apply Policy Pack
Applies the current policy pack.

```
Cmd+Shift+P → UNRDF: Apply Policy Pack
```

#### UNRDF: Run SPARQL Query
Runs selected SPARQL query against configured endpoint.

```
1. Select SPARQL query text
2. Cmd+Shift+P → UNRDF: Run SPARQL Query
3. View results in Output panel
```

### Context Menu

Right-click in `.hook` or `.policy` files:

- **Validate Hook** - Validate current file
- **Evaluate Hook** - Execute and show results
- **Apply Policy** - Apply policy pack

### Configuration

Configure extension in VS Code settings:

```json
{
  "unrdf.cli.path": "unrdf",
  "unrdf.validation.onSave": true,
  "unrdf.sparql.endpoint": "http://localhost:3030/ds/sparql"
}
```

#### Settings

- **unrdf.cli.path** - Path to UNRDF CLI executable (default: `unrdf`)
- **unrdf.validation.onSave** - Validate hooks on save (default: `true`)
- **unrdf.sparql.endpoint** - Default SPARQL endpoint URL

## Keyboard Shortcuts

Add to `keybindings.json`:

```json
[
  {
    "key": "cmd+shift+v",
    "command": "unrdf.validateHook",
    "when": "resourceLangId == hook"
  },
  {
    "key": "cmd+shift+e",
    "command": "unrdf.evaluateHook",
    "when": "resourceLangId == hook"
  },
  {
    "key": "cmd+shift+a",
    "command": "unrdf.applyPolicy",
    "when": "resourceLangId == policy"
  }
]
```

## Language Features

### Hover Documentation

Hover over hook functions to see documentation:

- `defineHook` - Hook definition API
- `chainHooks` - Hook chaining API
- `definePolicyPack` - Policy pack API

### Syntax Highlighting

Supported syntax elements:

#### Hooks
- Keywords: `defineHook`, `createHook`, `registerHook`
- Events: `onCreate`, `onUpdate`, `onDelete`
- SPARQL: `SELECT`, `WHERE`, `FILTER`, variables (`?var`)
- RDF terms: namespace prefixes (`rdf:`, `rdfs:`, etc.)

#### Policies
- Keywords: `policy`, `pack`, `rule`, `enforce`, `validate`
- Enforcement levels: `mandatory`, `advisory`, `disabled`
- Severity levels: `critical`, `high`, `medium`, `low`, `info`
- Actions: `allow`, `deny`, `audit`, `remediate`

## Development

### Building from Source

```bash
# Install dependencies
npm install

# Compile
npm run compile

# Watch mode
npm run watch

# Package VSIX
npm run package
```

### File Structure

```
vscode-extension/
├── extension.js              # Main extension logic
├── package.json              # Extension manifest
├── language-configuration.json  # Language config
├── syntaxes/
│   ├── hook.tmLanguage.json    # Hook syntax
│   └── policy.tmLanguage.json  # Policy syntax
└── snippets/
    ├── hooks.json              # Hook snippets
    └── policies.json           # Policy snippets
```

### Adding New Snippets

Edit `snippets/hooks.json` or `snippets/policies.json`:

```json
{
  "Snippet Name": {
    "prefix": "trigger",
    "body": [
      "line 1",
      "line 2 with ${1:placeholder}",
      "$0"
    ],
    "description": "Snippet description"
  }
}
```

### Adding New Commands

Edit `extension.js`:

```javascript
// Register command
const disposable = vscode.commands.registerCommand(
  'unrdf.myCommand',
  myCommandHandler
);
context.subscriptions.push(disposable);
```

Add to `package.json`:

```json
{
  "contributes": {
    "commands": [
      {
        "command": "unrdf.myCommand",
        "title": "UNRDF: My Command",
        "category": "UNRDF"
      }
    ]
  }
}
```

## Requirements

- VS Code 1.75.0 or higher
- UNRDF CLI installed and in PATH

## Known Issues

- Large hook files may have slower validation
- SPARQL query execution requires configured endpoint

## Contributing

1. Fork repository
2. Create feature branch
3. Make changes
4. Test in VS Code
5. Submit pull request

## License

MIT License - See LICENSE file for details

## Support

- **Issues**: https://github.com/unrdf/unrdf/issues
- **Discussions**: https://github.com/unrdf/unrdf/discussions
- **Documentation**: https://github.com/unrdf/unrdf/docs
