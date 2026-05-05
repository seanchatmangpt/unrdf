# Claude Code: CLI vs Extension Feature Parity Matrix

**Research Date**: 2025-12-27
**Agent**: Agent 8 - IDE/VS Code Surface Explorer
**Version**: Extension vlatest, CLI (current stable)

---

## Overview

This document provides a comprehensive feature-by-feature comparison between Claude Code's CLI and VS Code extension interfaces. It identifies areas of full parity, extension-only features, CLI-only features, and known gaps.

**Color Legend**:
- ✅ Full parity
- 🟡 Partial parity / Workaround exists
- ❌ Missing / Not available
- 🚧 Coming soon (announced)

---

## 1. Core Functionality Matrix

### latest Basic Operations

| Feature | CLI | Extension | Notes |
|---------|-----|-----------|-------|
| Start conversation | ✅ `claude` | ✅ Open panel | Full parity |
| Send message | ✅ Text input | ✅ Text input | Full parity |
| Multi-line input | ✅ Enter to continue | ✅ Shift+Enter | Different UX, same result |
| Code block syntax highlighting | ✅ Terminal colors | ✅ TextMate grammars | Extension superior |
| Markdown rendering | 🟡 Limited | ✅ Full rich text | Extension superior |
| Conversation history | ✅ Shared | ✅ Shared | Full parity (same storage) |
| Resume conversation | ✅ `--resume` flag | ✅ History list | Full parity |
| Multiple conversations | ✅ Separate shells | ✅ Multi-tab | Different UX, same capability |
| Export conversation | ❌ Not available | ❌ Not available | Both missing |
| Search conversation history | 🟡 `--resume` picker | ✅ History search | Extension superior |

**Verdict**: Near-parity for basics, extension has richer UI.

---

### latest File Operations

| Feature | CLI | Extension | Notes |
|---------|-----|-----------|-------|
| Read files | ✅ `workspace.fs` | ✅ `workspace.fs` | Full parity (same API) |
| Write files | ✅ Direct write | ✅ With diff preview | Extension adds visual review |
| Create files | ✅ Direct create | ✅ With preview | Extension adds visual review |
| Delete files | ✅ Direct delete | ✅ With confirmation | Extension adds visual review |
| File navigation | 🟡 Text paths | ✅ Click to open | Extension superior |
| Diff viewing | 🟡 Text diff | ✅ Visual diff | Extension superior |
| Accept/reject diffs | 🟡 Text approval | ✅ GUI buttons | Extension superior |
| Hunk-level review | ❌ All-or-nothing | ✅ Per-hunk actions | Extension only |
| Respect .gitignore | ✅ Configurable | ✅ Configurable | Full parity |
| Custom .claudeignore | ✅ Supported | ✅ Supported | Full parity |
| Bulk file operations | ✅ Scriptable | 🟡 Sequential UI | CLI superior for automation |

**Verdict**: Extension vastly superior for interactive review, CLI better for automation.

---

### latest Code Context & References

| Feature | CLI | Extension | Notes |
|---------|-----|-----------|-------|
| @-mention files | 🟡 Manual typing | ✅ Alt+K autocomplete | Extension superior UX |
| @-mention with line ranges | ✅ `@file:10-20` | ✅ Auto from selection | Extension auto-fills |
| @-mention multiple files | ✅ Manual list | 🟡 Repeat Alt+K | CLI easier for bulk |
| @-mention via glob | ✅ `@src/**/*.mjs` | 🟡 Manual typing | CLI superior |
| Current file context | 🟡 Inferred | ✅ Automatic | Extension auto-includes |
| Selection context | ❌ Manual copy | ✅ Automatic | Extension only |
| Workspace structure awareness | ✅ Full scan | ✅ Full scan | Full parity |
| Recent files quick access | ❌ Not available | 🟡 Extension history | Extension better |
| Clipboard integration | 🟡 Terminal paste | ✅ Native paste | Extension superior |

**Verdict**: Extension superior for interactive context, CLI better for programmatic references.

---

### latest Plan Mode

| Feature | CLI | Extension | Notes |
|---------|-----|-----------|-------|
| Enable plan mode | ✅ `/plan` command | ✅ `/plan` command | Full parity |
| View plan | ✅ Text list | ✅ Rich UI list | Extension superior UX |
| Approve plan | ✅ Text "yes" | ✅ Button click | Different UX |
| Reject plan | ✅ Text "no" | ✅ Button click | Different UX |
| Edit plan | 🟡 Text description | ✅ Inline editing | Extension superior |
| Step-by-step execution | 🟡 Manual prompts | ✅ UI checkboxes | Extension superior |
| Plan history | ❌ Not persisted | ❌ Not persisted | Both missing |
| Plan templates | ❌ Not available | ❌ Not available | Both missing |

**Verdict**: Extension significantly better UX for plan review workflow.

---

### latest Command Execution

| Feature | CLI | Extension | Notes |
|---------|-----|-----------|-------|
| Run terminal commands | ✅ Direct exec | ✅ Via permission | Full parity |
| Whitelist commands | ✅ `allowedCommands` | ✅ `allowedCommands` | Full parity (shared config) |
| Interactive commands | 🟡 Limited | ❌ Not supported | Both limited |
| Background commands | ✅ `&` operator | 🟡 Terminal integration | CLI superior |
| Stream command output | ✅ Real-time | ✅ Real-time | Full parity |
| Command history | ✅ Shell history | 🟡 Terminal history | CLI superior |
| ! bash shortcut | ✅ `!command` | ❌ Not available | CLI only |
| Custom command aliases | ✅ Shell aliases | ❌ Not available | CLI only |

**Verdict**: CLI superior for power users, extension adequate for basic needs.

---

## 2. Advanced Features Matrix

### latest MCP (Model Context Protocol) Servers

| Feature | CLI | Extension | Notes |
|---------|-----|-----------|-------|
| Add MCP server | ✅ `claude mcp add` | ❌ CLI required | CLI only for setup |
| List MCP servers | ✅ `claude mcp list` | ❌ CLI required | CLI only |
| Remove MCP server | ✅ `claude mcp remove` | ❌ CLI required | CLI only |
| Use MCP servers | ✅ Automatic | ✅ Automatic | Full parity (uses CLI config) |
| MCP server scopes | ✅ user/project/local | ✅ Same | Full parity (shared config) |
| MCP server config | ✅ `settings.json` | ✅ `settings.json` | Full parity (shared file) |
| Install .mcpb bundles | 🟡 Manual | 🟡 Manual | Both require manual setup |

**Verdict**: CLI required for MCP management, both use servers equally once configured.

---

### latest LSP (Language Server Protocol) Integration

| Feature | CLI | Extension | Notes |
|---------|-----|-----------|-------|
| LSP server support | ✅ vlatest+ | ✅ vlatest+ | Full parity |
| goToDefinition | ✅ Text output | 🟡 Text + clickable | Extension superior |
| findReferences | ✅ Text list | ✅ Interactive list | Extension superior |
| documentSymbol | ✅ Text tree | ✅ Outline view | Extension superior |
| hover info | ✅ Text display | 🟡 Limited | CLI actually better here |
| getDiagnostics | ✅ Full support | ✅ Full support | Full parity |
| Real-time diagnostics | ✅ After edits | ✅ After edits | Full parity |
| LSP plugin install | ✅ CLI commands | ✅ CLI commands | CLI required for setup |
| LSP config | ✅ `settings.json` | ✅ `settings.json` | Full parity |

**Verdict**: Near-parity, extension slightly better for interactive exploration.

---

### latest Slash Commands

| Feature | CLI | Extension | Notes |
|---------|-----|-----------|-------|
| View available commands | ✅ `/help` | ✅ Type `/` | Different discovery UX |
| Tab completion | ✅ Full support | ❌ Not available | CLI only |
| `/model` - change model | ✅ Supported | ✅ Supported | Full parity |
| `/plan` - enable plan mode | ✅ Supported | ✅ Supported | Full parity |
| `/reset` - clear context | ✅ Supported | ✅ Supported | Full parity |
| Custom slash commands | ✅ `.claude/commands/` | 🟡 Partial support | CLI superior |
| Slash command autocomplete | ✅ Tab completion | ❌ No autocomplete | CLI only |
| Command aliases | ✅ Configurable | ❌ Not available | CLI only |
| Nested commands | ✅ Supported | 🟡 Limited | CLI superior |

**Verdict**: CLI has fuller slash command support and discoverability.

---

### latest Hooks & Automation

| Feature | CLI | Extension | Notes |
|---------|-----|-----------|-------|
| `beforeEdit` hook | ✅ `~/.claude/hooks/` | ✅ Shared config | Full parity |
| `afterEdit` hook | ✅ `~/.claude/hooks/` | ✅ Shared config | Full parity |
| `beforeCommand` hook | ✅ Supported | ✅ Supported | Full parity |
| `afterCommand` hook | ✅ Supported | ✅ Supported | Full parity |
| Custom hook scripts | ✅ Shell scripts | ✅ Shell scripts | Full parity |
| Hook error handling | ✅ Abort on fail | ✅ Abort on fail | Full parity |
| Hook output display | ✅ Terminal | 🟡 Notification | Different UX |
| Async hooks | ✅ Supported | ✅ Supported | Full parity |

**Verdict**: Full parity (both use shared hooks config).

---

### latest Checkpoints

| Feature | CLI | Extension | Notes |
|---------|-----|-----------|-------|
| Create checkpoint | 🚧 Coming soon | 🚧 Coming soon | Both pending |
| List checkpoints | 🚧 Coming soon | 🚧 Coming soon | Both pending |
| Restore checkpoint | 🚧 Coming soon | 🚧 Coming soon | Both pending |
| Delete checkpoint | 🚧 Coming soon | 🚧 Coming soon | Both pending |
| Automatic checkpoints | 🚧 Unknown | 🚧 Unknown | Both pending |
| Checkpoint metadata | 🚧 Unknown | 🚧 Unknown | Both pending |

**Verdict**: Not yet available in either interface (announced feature).

---

### latest Subagents

| Feature | CLI | Extension | Notes |
|---------|-----|-----------|-------|
| Spawn subagents | ✅ Automatic | ✅ Automatic | Full parity |
| Monitor subagents | 🟡 Text status | 🟡 Text status | Both limited |
| Control subagents | ❌ Automatic only | ❌ Automatic only | Both automatic |
| Subagent output | ✅ Interleaved | ✅ Interleaved | Full parity |
| Subagent isolation | ✅ Supported | ✅ Supported | Full parity |

**Verdict**: Full parity (experimental feature in both).

---

## 3. Configuration & Settings Matrix

### latest Configuration Files

| Config Aspect | CLI | Extension | Notes |
|---------------|-----|-----------|-------|
| `~/.claude/settings.json` | ✅ Primary config | ✅ Shared config | Full parity |
| VS Code `settings.json` | ❌ N/A | ✅ Extension settings | Extension only |
| `.claude/` project config | ✅ Supported | ✅ Supported | Full parity |
| Environment variables | ✅ Shell env | ✅ Extension config | Different setup |
| API key storage | ✅ `settings.json` | ✅ `settings.json` | Full parity |
| Provider configuration | ✅ Full support | ✅ Full support | Full parity |
| Config validation | 🟡 Runtime errors | 🟡 Runtime errors | Both limited |

**Verdict**: Full parity for shared config, extension has additional VS Code-specific settings.

---

### latest Permission Settings

| Permission | CLI | Extension | Notes |
|------------|-----|-----------|-------|
| Manual review mode | ✅ Default | ✅ Default | Full parity |
| Auto-accept mode | ✅ `--auto-approve` | ✅ Setting toggle | Different activation |
| Skip permissions (unsafe) | ✅ Flag | ✅ Setting | Full parity |
| Allowed commands whitelist | ✅ `settings.json` | ✅ `settings.json` | Full parity |
| Allowed directories | ✅ `settings.json` | ✅ `settings.json` | Full parity |
| Per-file permissions | ❌ Not available | ❌ Not available | Both missing |
| Permission memory | ❌ Not available | ❌ Not available | Both missing |

**Verdict**: Full parity with room for improvement in both.

---

## 4. User Experience Matrix

### latest Interface & Interaction

| UX Aspect | CLI | Extension | Notes |
|-----------|-----|-----------|-------|
| Learning curve | 🟡 Medium | ✅ Low | Extension more intuitive |
| Keyboard efficiency | ✅ Excellent | 🟡 Good | CLI superior for power users |
| Mouse support | ❌ N/A | ✅ Full | Extension only |
| Accessibility | 🟡 Screen reader OK | ✅ Better | Extension more accessible |
| Visual feedback | 🟡 Text only | ✅ Rich UI | Extension superior |
| Context switching | 🟡 Terminal focus | ✅ Sidebar | Extension superior |
| Multitasking | 🟡 Alt-tab | ✅ Integrated | Extension superior |
| Screen real estate | ✅ Full terminal | 🟡 Panel size | CLI better for focus |

**Verdict**: Extension better for beginners, CLI better for keyboard-driven experts.

---

### latest Debugging & Logs

| Debug Feature | CLI | Extension | Notes |
|---------------|-----|-----------|-------|
| View logs | ✅ `--verbose` | ✅ "Show Logs" | Full parity |
| Log levels | ✅ Flags | 🟡 Limited | CLI superior |
| Log file access | ✅ `~/.claude/logs/` | ✅ Same location | Full parity |
| Real-time log streaming | ✅ Terminal output | 🟡 Log viewer | CLI superior |
| Error stack traces | ✅ Full trace | ✅ Full trace | Full parity |
| Debug mode | ✅ `--debug` | 🟡 Developer tools | Different approaches |

**Verdict**: CLI superior for deep debugging, extension adequate for most cases.

---

## 5. Platform-Specific Features

### latest Integration with VS Code

| VS Code Feature | CLI | Extension | Notes |
|-----------------|-----|-----------|-------|
| Workspace awareness | 🟡 Via env vars | ✅ Native API | Extension superior |
| Terminal integration | ✅ Run in terminal | ✅ Integrated terminal | Full parity |
| File watching | 🟡 Manual | ✅ Automatic | Extension superior |
| Git integration | 🟡 Via commands | ✅ Git API | Extension superior |
| Debugging session aware | ❌ Not available | 🟡 Limited | Both limited |
| Task runner integration | 🟡 Via commands | 🟡 Via commands | Equal limitation |
| Extension API access | ❌ N/A | ✅ Full access | Extension only |

**Verdict**: Extension deeply integrated, CLI complementary.

---

### latest Portability

| Portability Aspect | CLI | Extension | Notes |
|--------------------|-----|-----------|-------|
| Runs without IDE | ✅ Standalone | ❌ Requires VS Code | CLI portable |
| SSH/remote usage | ✅ Native | 🟡 VS Code Remote | CLI superior |
| Docker/containers | ✅ Native | 🟡 Dev containers | CLI superior |
| CI/CD integration | ✅ Scriptable | ❌ Not suitable | CLI only |
| Headless mode | ✅ Supported | ❌ Not available | CLI only |
| Cross-platform | ✅ Full | ✅ Full | Full parity |
| ARM support | ✅ Native | ✅ Native | Full parity |

**Verdict**: CLI far superior for automation and non-IDE contexts.

---

## 6. Performance Comparison

### latest Resource Usage

| Resource | CLI | Extension | Notes |
|----------|-----|-----------|-------|
| Memory footprint | ✅ ~50-100MB | 🟡 +100-200MB | CLI more lightweight |
| CPU usage (idle) | ✅ Minimal | 🟡 Low | CLI more efficient |
| Startup time | ✅ Instant | 🟡 ~1-2s | CLI faster |
| First message time | 🟡 Same API | 🟡 Same API | Equal (network bound) |
| Diff rendering | ✅ Text (fast) | 🟡 Visual (slower) | CLI faster, less useful |
| Large file handling | ✅ Streaming | 🟡 Load in memory | CLI superior |

**Verdict**: CLI more performant for resource-constrained environments.

---

### latest Workflow Efficiency

| Workflow | CLI | Extension | Best Choice |
|----------|-----|-----------|-------------|
| Quick fixes | ✅ Faster | 🟡 Good | CLI |
| Complex refactoring | 🟡 Good | ✅ Better | Extension |
| Code review | 🟡 Text diff | ✅ Visual diff | Extension |
| Batch operations | ✅ Scriptable | 🟡 Sequential | CLI |
| Exploration | 🟡 Good | ✅ Better | Extension |
| Teaching/demos | 🟡 OK | ✅ Better | Extension |
| Automation | ✅ Excellent | ❌ Not suitable | CLI |
| Collaboration | 🟡 Copy/paste | ✅ Screen share | Extension |

**Verdict**: Task-dependent, both have strengths.

---

## 7. Known Gaps & Missing Features

### latest Features Missing in Extension (Present in CLI)

1. **Tab Completion**: No autocomplete for slash commands, file paths
2. **! Bash Shortcut**: Can't use `!command` for direct shell execution
3. **Advanced Slash Commands**: Subset of CLI commands available
4. **Command Aliases**: Can't create custom command shortcuts
5. **Full Logging Control**: Limited log level configuration
6. **Glob-based @-mentions**: Manual typing required for patterns
7. **Custom Configuration**: Some CLI-only config options

**Impact**: Moderate - power users miss keyboard efficiency
**Workaround**: Use CLI for these specific tasks

---

### latest Features Missing in CLI (Present in Extension)

1. **Visual Diff Review**: Text-only diff display
2. **Hunk-Level Actions**: All-or-nothing file acceptance
3. **@-mention Autocomplete**: Must manually type file paths
4. **Plan Mode UI**: Text-only plan display
5. **Multi-Tab Conversations**: Requires separate terminal windows
6. **Click to Navigate**: Must manually open files
7. **Integrated Workspace**: Separate from code editing

**Impact**: Significant - visual review is core extension value
**Workaround**: Use extension for review-heavy workflows

---

### latest Features Missing in Both

1. **Conversation Export**: Can't save/share as markdown
2. **Checkpoints**: Not yet released
3. **Plan Templates**: Can't save reusable plans
4. **Per-File Permissions**: Granular permission control
5. **Permission Memory**: "Remember this decision" for files
6. **Subagent UI**: Can't manually spawn/control subagents
7. **Team Collaboration**: No built-in sharing features
8. **Code Review Integration**: No GitHub PR / GitLab MR workflow
9. **Conversation Merge**: Can't combine multiple threads
10. **Custom Themes**: Can't customize UI appearance

**Impact**: Varies - some would significantly improve UX
**Workaround**: Manual processes or third-party tools

---

## 8. Parity Improvement Roadmap

### latest High Priority (User Impact)

**Extension Should Add**:
- [ ] Tab completion for slash commands (GitHub #10246)
- [ ] .claude directory in @-autocomplete (GitHub #1818)
- [ ] Full slash command support (GitHub #9119)
- [ ] settings.json parity for all CLI options (GitHub #8727)

**CLI Should Add**:
- [ ] Better plan mode text UI
- [ ] Improved diff visualization (color, formatting)
- [ ] @-mention suggestions based on context

---

### latest Medium Priority (Power User Features)

**Extension Should Add**:
- [ ] Glob-based @-mentions UI
- [ ] Command aliases
- [ ] Advanced logging controls

**CLI Should Add**:
- [ ] Interactive file picker for @-mentions
- [ ] Terminal-based UI for plan approval

---

### latest Low Priority (Nice-to-Have)

**Both Should Add**:
- [ ] Conversation export to markdown
- [ ] Plan templates library
- [ ] Custom UI themes
- [ ] Better LSP diagnostics display

---

## 9. Decision Matrix: Which Interface to Use?

### latest Use CLI When...

✅ **Automation**: Integrating Claude Code into scripts or CI/CD
✅ **Speed**: Quick fixes where keyboard efficiency matters
✅ **Headless**: Running on servers without GUI
✅ **SSH/Remote**: Working on remote machines
✅ **Resource-Constrained**: Limited memory/CPU available
✅ **Bulk Operations**: Scripting multiple file operations
✅ **MCP Management**: Configuring MCP servers
✅ **Power User**: Comfortable with terminal workflows

**Example Scenarios**:
- CI/CD pipeline for automated code review
- SSH into production server for quick fix
- Batch processing 50+ files
- Setting up new MCP integration

---

### latest Use Extension When...

✅ **Visual Review**: Need to see diffs side-by-side
✅ **Plan Mode**: Complex changes requiring step approval
✅ **Learning**: New to Claude Code
✅ **Collaboration**: Screen sharing or pair programming
✅ **Exploration**: Browsing codebase interactively
✅ **Teaching**: Demonstrating Claude Code to others
✅ **Context Switching**: Want to stay in VS Code
✅ **File Navigation**: Need to jump to specific code locations

**Example Scenarios**:
- Complex refactoring with multiple file changes
- Teaching junior developer how to use Claude Code
- Reviewing AI-proposed architecture changes
- Quickly adding AI assistance to existing VS Code workflow

---

### latest Use Both (Hybrid) When...

✅ **Setup + Execution**: CLI for MCP config, extension for usage
✅ **Rapid Iteration + Review**: CLI for speed, extension for final review
✅ **Experimentation + Production**: Extension for exploration, CLI for automation
✅ **Team Development**: Extension for code review, CLI for deployment

**Example Workflow**:
```
1. Extension: "Analyze codebase for refactoring opportunities"
2. Extension: Review proposed plan visually
3. CLI: claude --resume  (switch to CLI)
4. CLI: Rapid iteration on implementation
5. Extension: Resume for final visual diff review
6. Extension: Approve changes
```

---

## 10. Conclusion

### latest Parity Summary

**Full Parity** (✅): ~60% of features
- Core conversation
- File operations (underlying API)
- MCP server usage
- LSP integration
- Hooks
- Shared configuration

**Partial Parity** (🟡): ~25% of features
- Plan mode (different UX)
- Slash commands (subset in extension)
- Debugging (different tools)
- @-mentions (different UX)

**No Parity** (❌): ~15% of features
- Visual diff (extension only)
- Tab completion (CLI only)
- Automation (CLI only)
- Hunk-level review (extension only)

---

### latest Strategic Recommendations

**For Anthropic**:
1. Prioritize extension tab completion (high user demand)
2. Add conversation export to both interfaces
3. Bring slash command parity to extension
4. Improve CLI visual output for plan mode

**For Users**:
1. Start with extension for learning
2. Graduate to CLI for speed once comfortable
3. Use hybrid workflows for best of both
4. Set up MCP servers in CLI, use everywhere

**For Teams**:
1. Standardize on extension for code review
2. Use CLI for CI/CD automation
3. Document hybrid workflows in team wiki
4. Share MCP configurations via project scope

---

### latest Future Outlook

**Expected Parity Improvements**:
- Checkpoints (both)
- Better slash command support (extension)
- Improved plan mode UX (CLI)
- JetBrains support (new surface)

**Likely Divergence**:
- Extension will remain superior for visual workflows
- CLI will remain superior for automation
- Both will retain unique strengths

**Net Assessment**: Complementary tools, not competitors. Choose based on task, not preference.

---

## Sources

All sources documented in main research report: `/home/user/unrdf/research-output/agent-08-ide-vscode-research.md`

---

**End of Parity Matrix**
