# How to Write Diataxis Documentation

Quick guide for writing each of the four documentation types used in UNRDF.

## 1. Tutorials (Learning-Oriented)

### Purpose
Teach someone how to accomplish a **concrete task** in a realistic scenario.

### Who Reads It
- Complete beginners
- People who want to learn by doing
- First-time users of your package

### Key Characteristics
- **Always action-oriented** - "Build X", "Create Y"
- **Assumes minimal knowledge** - But not patronizing
- **Progressive complexity** - Start simple, build up
- **Allows no skipping** - Every step matters
- **Verified** - Steps are tested and work

### Structure

```markdown
# Tutorial: [Concrete Task]

## Overview
[1-2 sentences explaining what you'll learn]

## What You'll Learn
- Concrete skill 1
- Concrete skill 2
- Concrete skill 3

## What You'll Build
[Describe the end result: "a working blog app", "a data importer", etc.]

## Prerequisites
- Node.js 18+
- Basic JavaScript knowledge
- Familiarity with [prerequisite package if any]

## Before You Start
[Any setup steps: npm install, etc.]

## Step 1: [Clear Title]
[Explain what you're doing and why]

```javascript
// Code example
```

[Explanation of what just happened]

## Step 2: [Next Clear Title]
[Continue...]

## Verify It Works
[How to test that your code works]

```
Expected output:
...
```

## What You've Learned
- Skill 1 ✅
- Skill 2 ✅
- Skill 3 ✅

## Next Steps
👉 Read: [Related tutorial]
👉 Explore: [Related how-to guide]
👉 Discover: [Related reference]
```

### Writing Tips

✅ **DO:**
- Write in second person ("You'll create a store...")
- Use real-world examples ("a blog", "a knowledge graph")
- Include intermediate verification steps
- Explain WHY each step matters
- Test every code example before publishing

❌ **DON'T:**
- Explain all options (that's for reference)
- Skip steps to save time
- Use insider terminology without explaining
- Assume the reader knows other packages
- Make it too long (30-45 min max)

### Naming Convention
- `01-getting-started.md` - First experience
- `02-basic-workflow.md` - Common task
- `03-advanced-patterns.md` - Building on skills
- Format: `{number}-{readable-title}.md`

### Length
- **Good:** 10-30 minutes to complete
- **Too long:** >1 hour (split into 2 tutorials)
- **Too short:** <5 minutes (make it a how-to instead)

---

## 2. How-To Guides (Problem-Solving)

### Purpose
Solve a **specific, real-world problem** the user currently has.

### Who Reads It
- Users with a concrete problem
- Developers searching "[package] how to do X"
- Active developers (not beginners)

### Key Characteristics
- **Problem-focused** - Solves ONE specific problem
- **Assumes knowledge** - Knows basics already
- **Solution-forward** - Gets to the answer quickly
- **Flexible** - Reader can skip parts
- **Multiple approaches** - Shows alternatives when relevant

### Structure

```markdown
# How To: [Specific Problem]

## Problem
[Clearly state the problem: "Your queries are slow", "You need to debug a hook", etc.]

## Prerequisites
- Familiarity with [foundational topic]
- [Basic package knowledge expected]

## Solution

### Approach 1: [Direct/Recommended]
[Explanation of why this approach]

```javascript
// Code example
```

[Explanation of what the code does]

### Approach 2: [Alternative if relevant]
[When to use this approach]

```javascript
// Alternative code
```

[When/why to use this alternative]

## Common Issues
- **Issue X:** Solution
- **Issue Y:** Solution

## See Also
- [Related how-to]
- [Reference documentation]
- [Tutorial if they need to learn first]

## Pro Tips
- Tip 1
- Tip 2
```

### Writing Tips

✅ **DO:**
- Get to the solution in the first section
- Provide 2-3 approaches if appropriate
- Include troubleshooting/edge cases
- Link to reference for details
- Use a real code snippet from your tests

❌ **DON'T:**
- Explain concepts from scratch (link to explanation instead)
- Make it step-by-step like a tutorial
- Cover too many problems (one per guide)
- Include every possible option (link to reference)
- Assume zero knowledge

### Naming Convention
- `how-to-optimize-queries.md`
- `how-to-debug-hooks.md`
- `troubleshooting-errors.md`
- `performance-tuning.md`
- Format: `how-to-{problem-statement}.md` or `troubleshooting-{issue}.md`

### Length
- **Good:** 3-10 minutes to read
- **Too long:** >15 minutes (probably too many problems covered)
- **Too short:** <2 minutes (make it part of a larger guide)

---

## 3. Reference (Information-Oriented)

### Purpose
Provide **complete, accurate information** for someone using the feature.

### Who Reads It
- Active users looking up details
- Developers copy-pasting code
- People debugging issues
- People reading in IDE

### Key Characteristics
- **Comprehensive** - Nothing significant left out
- **Precise** - Accurate in every detail
- **Organized** - Easy to scan and navigate
- **Verbose** - Explains all options even if obvious
- **Precise types** - Every parameter type documented

### Structure

```markdown
# Reference: [Topic]

## Overview
[1-2 sentences: what is this?]

## Quick Example
```javascript
// Minimal working example
```

## Detailed Reference

### Function: `functionName()`

```javascript
functionName(param1, param2, options?)
```

**Parameters:**
- `param1` (string, required) - What this parameter does
- `param2` (number, required) - What this parameter does
- `options` (object, optional) - Configuration object:
  - `option1` (boolean, default: false) - What this does
  - `option2` (string, default: "auto") - What this does

**Returns:**
- (Promise<Result>) - Description of return value

**Throws:**
- `TypeError` - Thrown when...
- `ValueError` - Thrown when...

**Examples:**

Basic usage:
```javascript
const result = functionName("value", 42);
```

With options:
```javascript
const result = functionName("value", 42, {
  option1: true,
  option2: "manual"
});
```

### Function: `anotherFunction()`
[Continue for each function...]

## Options Reference

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `option1` | boolean | false | What it does |
| `option2` | string | "auto" | What it does |

## Error Reference

| Error | When | Solution |
|-------|------|----------|
| `StoreNotInitialized` | Store not created | Call `createStore()` first |
| `InvalidQuery` | Bad SPARQL | Check query syntax |

## Configuration Reference

```javascript
// Full configuration object with all options
{
  backend: 'memory',      // 'memory' or 'oxigraph'
  persistence: false,      // Enable persistence?
  maxSize: 1000000,        // Max triples to store
  // ... all options documented
}
```

## Type Definitions

```typescript
interface StoreConfig {
  backend: 'memory' | 'oxigraph';
  persistence: boolean;
  // ... all fields
}

type QueryResult = {
  name: string;
  age: number;
  // ...
};
```

## See Also
- [Tutorial](../TUTORIALS/learning.md)
- [How-To Guide](../HOW-TO/common-task.md)
- [Explanation](../EXPLANATION/concepts.md)
```

### Writing Tips

✅ **DO:**
- Be exhaustive with parameters
- Include real code examples
- Provide a type reference
- Show error cases
- Link to tutorials/explanations for details

❌ **DON'T:**
- Teach concepts (link to explanation)
- Include step-by-step instructions (that's a tutorial)
- Skip "obvious" parameters
- Abbreviate type information
- Hide any configuration options

### Naming Convention
- `API.md` - Main API reference
- `TYPES.md` - Type definitions
- `CONFIGURATION.md` - Config options
- `ERRORS.md` - Error codes and meanings
- `CLI.md` - CLI command reference
- `MIGRATION.md` - Version migration

### Length
- **No limit** - Completeness is more important than brevity
- **Well-organized** - Use tables, sections, clear headings
- **Scannable** - Reader shouldn't have to read linearly

---

## 4. Explanation (Understanding-Oriented)

### Purpose
Help the user **understand** how something works and *why* it's designed that way.

### Who Reads It
- Curious developers wanting to understand
- Contributors learning the codebase
- Users deciding if this is right for them
- People debugging complex issues

### Key Characteristics
- **Conceptual** - Explains ideas, not just code
- **Why-focused** - Explains design decisions
- **Background** - Provides context
- **Generous** - No need to be concise
- **Non-procedural** - Not step-by-step

### Structure

```markdown
# Explanation: [Concept]

## The Big Picture
[Broader context: how does this fit in the system?]

## What Is [Concept]?
[Definition and context]

## How It Works

### Mechanism 1
[Detailed explanation of mechanism]

### Mechanism 2
[Another aspect of how it works]

## Why This Design?

### Design Decision 1
[Alternative approaches and why we chose this one]

### Design Decision 2
[Trade-offs and reasoning]

## Real-World Analogy
[Compare to something familiar to help understanding]

For example, [analogy] works like this...

## When (and When Not) to Use This
[Applicability and limitations]

### Good for:
- Use case 1
- Use case 2

### Not suitable for:
- Wrong use case 1
- Wrong use case 2

## Related Concepts
[Links to related explanations]

## Performance Implications
[How this design affects performance]

## See Also
- [Related explanation](./related-concept.md)
- [How-to when you want to use this](../HOW-TO/using-this.md)
- [Reference documentation](../REFERENCE/API.md)
- [Tutorial that teaches this](../TUTORIALS/learning.md)
```

### Writing Tips

✅ **DO:**
- Explore *why*, not just *what*
- Use analogies and examples (non-code)
- Discuss trade-offs and alternatives
- Explain historical context if relevant
- Be generous with space and detail

❌ **DON'T:**
- Provide step-by-step instructions (use tutorial)
- Be a reference manual (use reference)
- Assume complete beginners (assume some knowledge)
- Rush to conclusions (explain thoroughly)
- Focus on how to use (focus on understanding)

### Naming Convention
- `architecture.md` - How it's organized
- `design-decisions.md` - Why these choices?
- `concepts.md` - Key conceptual ideas
- `performance.md` - Performance details
- `comparison.md` - vs other approaches/products
- `history.md` - Why this evolved this way
- Format: `{concept}.md`

### Length
- **Good:** 10-30 minutes to read
- **Can be long:** Thoughtful, deep explanations are welcome
- **Should be skimmable** - Use headings and structure

---

## Quick Decision Tree

```
User is asking...

├─ "How do I get started?"
│  └─> TUTORIAL: Show them a complete example
│
├─ "How do I do X?"
│  └─> HOW-TO: Solve the specific problem
│
├─ "What does this do?"
│  └─> REFERENCE: Complete information
│
└─ "Why does it work this way?"
   └─> EXPLANATION: Conceptual understanding
```

---

## Consistency Guidelines

### Tone
- **Tutorials:** Encouraging, step-by-step
- **How-To:** Direct, practical
- **Reference:** Formal, precise
- **Explanation:** Educational, thoughtful

### Voice
- Use second person in tutorials ("You'll create...")
- Use imperative in how-to ("Create X...")
- Use descriptive in reference ("The function returns...")
- Use passive/descriptive in explanation ("The system is designed to...")

### Code Examples
- **Tutorials:** Verbose, with explanations
- **How-To:** Direct, focused on solution
- **Reference:** Complete signatures with all options
- **Explanation:** Simplified, conceptual (pseudo-code OK)

### Links
- **Tutorials:** Link to relevant how-to and reference
- **How-To:** Link to reference for details, tutorials for learning
- **Reference:** Link to how-to for practical use, explanation for concepts
- **Explanation:** Link to tutorials and how-tos for practical application

---

## Template Checklist

### Before Publishing

- [ ] **Content:** All sections filled (no "TODO")
- [ ] **Examples:** All code examples tested and working
- [ ] **Links:** All cross-references valid
- [ ] **Tone:** Consistent with Diataxis type
- [ ] **Audience:** Appropriate for intended readers
- [ ] **Length:** Not too long, not too short
- [ ] **Clarity:** Grammar and spelling checked
- [ ] **Structure:** Headings clear and hierarchical
- [ ] **References:** Cited properly if needed

---

## Tools & Resources

### Markdown Tips
- Use `## Headings` for sections (not h1)
- Use `code()` for identifiers
- Use ` ```javascript` for code blocks
- Use tables for reference material
- Use **bold** for key terms first use

### Testing Code Examples
- [ ] Copy example into fresh project
- [ ] Follow example exactly as written
- [ ] Verify it produces expected output
- [ ] Try common variations

### Getting Feedback
- Share draft with subject matter expert
- Have a beginner read tutorials
- Have an expert review reference
- Check with documentation team

---

**Ready to write?** Pick a package and start with a tutorial! 📝
