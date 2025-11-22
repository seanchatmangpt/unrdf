# Claude Code Commands

This directory contains slash commands for Claude Code that implement comprehensive development workflows based on Lean Six Sigma, Design for Lean Six Sigma (DfLSS), and expert engineering practices.

## Available Commands

### Quality & Process Improvement

#### `/kaizen-improvement`

Continuous improvement workflow - Make small, incremental improvements rather than big rewrites. Implements Plan-Do-Check-Act (PDCA) cycle for sustainable, low-risk improvements.

**Use when**: Making incremental improvements, refactoring, code quality improvements

#### `/root-cause-analysis`

5 Whys root cause analysis - Find underlying causes of problems, not just symptoms. Implements systematic problem analysis with prevention measures.

**Use when**: Debugging issues, investigating failures, preventing problem recurrence

#### `/poka-yoke-design`

Error prevention through design - Make invalid states unrepresentable using Zod validation and JSDoc types. Implement runtime validation and type safety through schemas.

**Use when**: Preventing errors at runtime, adding type safety with Zod, enforcing invariants with validation

#### `/eliminate-muda`

Waste elimination - Identify and eliminate the 8 types of waste (Muda) in code and process. Focus on value-adding activities.

**Use when**: Optimizing code, removing dead code, eliminating unnecessary complexity

#### `/eliminate-mura`

Standardization - Eliminate unevenness (Mura) through consistent patterns and standards. Standardize processes for predictable outcomes.

**Use when**: Standardizing code patterns, enforcing consistency, reducing variability

#### `/eliminate-muri`

Overburden reduction - Identify and eliminate unreasonable strain (Muri) on systems, people, or processes. Reduce complexity, coupling, and cognitive load.

**Use when**: Reducing code complexity, decoupling modules, improving readability, optimizing resource usage

### Problem Solving

#### `/dmaic-problem-solving`

Define-Measure-Analyze-Improve-Control workflow for systematic problem solving. Implements data-driven improvement process.

**Use when**: Solving complex problems, systematic improvement, data-driven decisions

#### `/dmedi-design-process`

Define-Measure-Explore-Develop-Implement workflow for new designs. Implements Design for Lean Six Sigma (DfLSS) methodology.

**Use when**: Designing new features, creating new systems, greenfield development

#### `/triz-problem-solving`

Theory of Inventive Problem Solving - Systematic innovation patterns for technical contradictions and inventive solutions.

**Use when**: Solving contradictions, finding innovative solutions, technical challenges

#### `/fmea`

Failure Mode and Effects Analysis - Proactive risk assessment and prevention. Identify potential failures before they occur.

**Use when**: Risk assessment, new feature development, critical path analysis

### Design & Planning

#### `/robust-design`

Design systems robust to variation and noise. Implements Taguchi methods for resilient designs.

**Use when**: Designing reliable systems, handling edge cases, building resilient code

#### `/concept-selection`

Systematic concept evaluation - Pugh matrix for objective concept selection based on criteria weighting.

**Use when**: Choosing between design alternatives, evaluating trade-offs, decision making

#### `/voice-of-customer-qfd`

Quality Function Deployment - Translate customer needs into technical requirements using House of Quality.

**Use when**: Requirements gathering, feature prioritization, customer-focused design

### Development Practices

#### `/80-20-fill-gaps`

Pareto principle application - Focus on the 20% that delivers 80% of value. Fill critical gaps systematically.

**Use when**: Prioritizing work, identifying critical gaps, maximizing value delivery

#### `/expert-testing-patterns`

Expert testing patterns - Comprehensive testing strategies including AAA pattern, real collaborators, behavior verification.

**Use when**: Writing tests, improving test quality, test-driven development

#### `/verify-tests`

Systematic test verification - Ensure tests are correct, complete, and valuable. Validate test quality.

**Use when**: Reviewing tests, ensuring test coverage, validating test effectiveness

### Process Control

#### `/andon-signals`

Visual management - Recognize and respond to problem signals. Stop and fix issues immediately.

**Use when**: Monitoring build/test failures, detecting quality issues, rapid response

#### `/gemba-walk`

Go to the source - Observe actual code and systems where work happens. Understand reality vs. assumptions.

**Use when**: Understanding systems, investigating issues, learning codebase

### Release Management

#### `/release-preparation`

Comprehensive release readiness checklist - Ensure code is ready for production deployment.

**Use when**: Preparing for releases, pre-deployment verification, quality gates

## Command Categories

### By Purpose

**Improvement**: `/kaizen-improvement`, `/eliminate-muda`, `/eliminate-mura`, `/eliminate-muri`, `/80-20-fill-gaps`

**Problem Solving**: `/root-cause-analysis`, `/dmaic-problem-solving`, `/triz-problem-solving`, `/fmea`

**Design**: `/dmedi-design-process`, `/poka-yoke-design`, `/robust-design`, `/concept-selection`

**Testing**: `/expert-testing-patterns`, `/verify-tests`

**Process Control**: `/andon-signals`, `/gemba-walk`, `/release-preparation`

**Requirements**: `/voice-of-customer-qfd`

### By Workflow Complexity

**Quick (5-10 minutes)**: `/andon-signals`, `/gemba-walk`, `/80-20-fill-gaps`

**Medium (15-30 minutes)**: `/kaizen-improvement`, `/eliminate-muda`, `/eliminate-mura`, `/eliminate-muri`, `/expert-testing-patterns`

**Comprehensive (30+ minutes)**: `/dmaic-problem-solving`, `/dmedi-design-process`, `/root-cause-analysis`, `/fmea`, `/robust-design`

## Methodology Alignment

These commands implement **Design for Lean Six Sigma (DfLSS)** methodology, which integrates:

- **Lean** (efficiency, waste elimination)
- **Six Sigma** (quality, defect prevention)
- **Expert Engineering Practices** (type safety, testing, patterns)

### Key Principles

1. **Continuous Improvement** - Small improvements compound over time
2. **Root Cause Focus** - Fix underlying causes, not symptoms
3. **Type-Level Prevention** - Make errors impossible through design
4. **Data-Driven** - Use measurements and verification
5. **Customer Focus** - Deliver value to users

## Integration Patterns

Commands are designed to work together:

- **Problem → Analysis → Solution**: `/andon-signals` → `/root-cause-analysis` → `/kaizen-improvement`
- **New Feature**: `/voice-of-customer-qfd` → `/dmedi-design-process` → `/expert-testing-patterns`
- **Quality Improvement**: `/verify-tests` → `/root-cause-analysis` → `/poka-yoke-design`
- **Code Quality**: `/gemba-walk` → `/eliminate-muda` → `/eliminate-mura` → `/eliminate-muri`

## Usage Tips

1. **Start Small**: Use `/80-20-fill-gaps` to identify highest-value work
2. **Fix Problems**: Use `/root-cause-analysis` for debugging
3. **Improve Continuously**: Use `/kaizen-improvement` for incremental improvements
4. **Prevent Errors**: Use `/poka-yoke-design` for type safety
5. **New Designs**: Use `/dmedi-design-process` for new features

## DfLSS vs DFSS

**Critical Distinction**: These commands implement **DfLSS** (Design for Lean Six Sigma), not DFSS (Design for Six Sigma).

- **DFSS**: Quality focus only (defect prevention)
- **DfLSS**: Efficiency + Quality (waste elimination + defect prevention)

**Why DfLSS**: Addresses both efficiency and quality from the start, preventing both defects AND waste. See `/root-cause-analysis` for detailed explanation of why conflating DfLSS with DFSS is a fundamental methodology error.

## Command Format

Each command follows a consistent format:

1. **Purpose** - What the command does and why
2. **Workflow Overview** - High-level steps
3. **Step-by-Step Instructions** - Detailed guidance
4. **Examples** - Concrete implementations
5. **Best Practices** - Expert insights
6. **Integration** - How it connects to other commands

## Contributing

When adding new commands:

1. Follow existing command structure
2. Include concrete examples
3. Integrate with existing commands
4. Document methodology alignment
5. Add to this README

## References

- Chicago TDD Tools - Project using these commands
- [Process Documentation](../../docs/process/) - Related process docs
- [Architecture Patterns](../../cookbook/src/architecture-patterns/) - Design patterns
- [Testing Patterns](../../cookbook/src/testing-patterns/) - Testing guidance
