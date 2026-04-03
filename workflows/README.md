# UNRDF Workflows

Reusable workflows for common software development processes using systematic methodology (SPARC, DfLSS, Lean Six Sigma).

## Available Workflows

### 1. 80/20 Fill Gaps Workflow

**Purpose**: Identify and complete incomplete capabilities in a codebase using 80/20 thinking (quality-first approach)

**Key Principles**:
- **80/20 Rule**: 20% of capabilities deliver 80% of value
- **Quality-First**: Value = Quality + Consistency + Maintainability
- **DfLSS Aligned**: Design for Lean Six Sigma (prevent defects AND waste)

**Files**:
- [`80-20-fill-gaps.mjs`](./80-20-fill-gaps.mjs) - Workflow definition (JavaScript/ESM)
- [`80-20-fill-gaps-readme.md`](./80-20-fill-gaps-readme.md) - Complete documentation
- [`80-20-fill-gaps.mermaid`](./80-20-fill-gaps.mermaid) - Visual workflow diagram

**Usage**:
```bash
# Execute workflow
node -e "import('./80-20-fill-gaps.mjs').then(m => console.log(m.workflow))"

# View as diagram
cat 80-20-fill-gaps.mermaid

# Export formats
node -e "import('./80-20-fill-gaps.mjs').then(m => {
  console.log(m.exportWorkflow('yaml'))   // YAML format
  console.log(m.exportWorkflow('mermaid')) // Mermaid diagram
  console.log(m.exportWorkflow('json'))    // JSON format
})"
```

**Workflow Steps**:
1. 📊 **Scan** - Find incomplete capabilities (15 min)
2. 📋 **Identify** - Categorize gaps (15 min)
3. 🎯 **Prioritize** - Select top 20% by 80/20 (10 min)
4. 📝 **Plan** - Create implementation plan (20 min)
5. 💻 **Implement** - Complete gaps in parallel (90 min)
6. ✅ **Validate** - Verify with quality checks (15 min)
7. 🗺️ **Next Steps** - Plan remaining work (10 min)

**Total Time**: ~3 hours

**Success Criteria**:
- ✓ Top 20% of gaps completed
- ✓ 80%+ of value delivered
- ✓ 0 regressions
- ✓ 100% type coverage
- ✓ 80%+ test coverage
- ✓ Complete documentation

**Integration**:
- Integrates with Andon signals (`/andon-signals`)
- Uses root cause analysis (`/root-cause-analysis`)
- Applies DMAIC methodology (`/dmaic-problem-solving`)
- Implements Poka-Yoke design (`/poka-yoke-design`)

**Real-World Example** (UNRDF Monorepo):
- Gaps identified: 6
- Gaps completed (top 20%): 4
- Value delivered: 80%+
- Time taken: 3 hours
- Results: Type safety, validation, documentation improvements

---

## Workflow Structure

Each workflow includes:

### Definition (`*.mjs`)
- Workflow steps with inputs/outputs
- Decision gates and branches
- Success criteria
- Metrics and tracking
- Integration points
- Example execution

### Documentation (`*-readme.md`)
- Complete overview
- Step-by-step instructions
- Value framework
- Success criteria details
- Tools and integration
- Usage examples

### Visualization (`*.mermaid`)
- Visual workflow diagram
- Decision points and gates
- Process flow
- Step duration and effort

---

## How to Use Workflows

### As a Reference
Read the documentation to understand the process:
```bash
cat 80-20-fill-gaps-readme.md
```

### As a Checklist
Use the steps as a checklist for your work:
- [ ] Step 1: Scan
- [ ] Step 2: Identify
- [ ] Step 3: Prioritize
- [ ] Step 4: Plan
- [ ] Step 5: Implement
- [ ] Step 6: Validate
- [ ] Step 7: Next Steps

### As a Template
Create new workflows by:
1. Copy the structure from an existing workflow
2. Adapt the steps for your problem
3. Define success criteria
4. Document metrics
5. Create visualization

### With Claude Flow Agents
```javascript
// Execute with task orchestrator
Task("80-20 fill gaps",
  "Execute complete workflow for identifying and completing capabilities",
  "task-orchestrator"
)
```

---

## Quality Standards

All workflows follow quality-first principles:

✅ **Type Safety**: 100% JSDoc types or Zod schemas
✅ **Testing**: 80%+ test coverage minimum
✅ **Documentation**: Complete with examples
✅ **Consistency**: Follows project patterns
✅ **Validation**: Success criteria verified
✅ **Metrics**: Tracked and reported

---

## Related Commands

- `/80-20-fill-gaps` - Execute workflow from CLI
- `/andon-signals` - Visual problem management
- `/root-cause-analysis` - 5 Whys methodology
- `/dmaic-problem-solving` - Lean Six Sigma DMAIC
- `/poka-yoke-design` - Error-proofing design

---

## Workflow Principles

### 80/20 Thinking
- Focus on the vital few (20%) that deliver most value (80%)
- Value includes quality, consistency, and maintainability
- Quality is HIGH value, not optional

### Quality-First Approach
- Prevent defects (Six Sigma) and waste (Lean) from the start
- DfLSS (Design for Lean Six Sigma) not DFSS (quality-only)
- Build quality in, don't test it in

### Systematic Process
- Clear steps with decision gates
- Measurable success criteria
- Documented metrics
- Repeatable process

### Integration
- Workflows coordinate with each other
- Share common principles
- Enable scaling from single features to large programs

---

## Metrics & Reporting

All workflows include:
- **Baseline Metrics**: Starting measurements
- **Target Metrics**: Success criteria
- **Actual Metrics**: Results achieved
- **Report Template**: Standardized format

Example:
```markdown
## 80/20 Fill Gaps - Completion Report

| Metric | Target | Actual |
|--------|--------|--------|
| Gaps completed | ~2-3 | 4 |
| Value delivered | ≥80% | 85% |
| Test coverage | ≥80% | 100% |
| Regressions | 0 | 0 |
| Type coverage | 100% | 100% |
```

---

## Implementation Notes

### File Organization
```
workflows/
├── README.md                    # This file
├── 80-20-fill-gaps.mjs         # Workflow definition (ESM)
├── 80-20-fill-gaps-readme.md   # Complete documentation
└── 80-20-fill-gaps.mermaid     # Visual diagram
```

### Dependencies
- Node.js 18+ (ESM modules)
- No external dependencies for workflow definitions
- Integration with Claude Flow agents (optional)

### Integration Points
- **CI/CD**: Export workflows as GitHub Actions
- **Documentation**: Reference in team docs
- **Automation**: Execute via Claude Flow
- **Reporting**: Generate reports from metrics

---

## Creating New Workflows

To create a new workflow:

1. **Copy Template**:
   ```bash
   cp 80-20-fill-gaps.mjs my-workflow.mjs
   ```

2. **Define Steps**:
   - Identify 5-7 clear steps
   - Add decision gates between steps
   - Define inputs/outputs for each step

3. **Document**:
   - Create comprehensive README
   - Include examples
   - Provide success criteria

4. **Visualize**:
   - Create Mermaid diagram
   - Show decision points
   - Indicate timing and effort

5. **Integrate**:
   - Link to related workflows
   - Export to different formats
   - Connect to agents

---

## Support & Documentation

- **Complete Example**: See [`docs/COMPLETION_REPORT_2025_12_20.md`](../docs/COMPLETION_REPORT_2025_12_20.md)
- **Signal Management**: See [`docs/ANDON_SIGNALS.md`](../docs/ANDON_SIGNALS.md)
- **CLI Command**: See `/80-20-fill-gaps` command documentation

---

**Version**: 1.0.0
**Status**: Production Ready
**Last Updated**: 2025-12-21
**License**: MIT
