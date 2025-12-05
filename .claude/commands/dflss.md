# Design For Lean Six Sigma (DfLSS) - Strategic Design Framework

## Purpose

Design For Lean Six Sigma (DfLSS) is the strategic methodology for designing new products, processes, or systems that are lean (eliminate waste) AND robust (prevent defects) from inception. Unlike DFSS (Design for Six Sigma) which only addresses quality/defects, DfLSS integrates:

- **Lean**: Eliminate waste (Muda), variation (Mura), overburden (Muri)
- **Six Sigma**: Design for robustness, prevent defects at source
- **Customer-Centric**: Start with customer needs (Voice of Customer)
- **Data-Driven**: Use data and experiments to drive design decisions
- **Systematic**: Follow proven phases to ensure nothing is missed

**DfLSS vs DFSS Critical Distinction**:
- **DFSS** (Design for Six Sigma) = Quality only → High defect prevention, HIGH WASTE
- **DfLSS** (Design for Lean Six Sigma) = Quality + Efficiency → High defect prevention + Low waste (CORRECT APPROACH)

**When to use DfLSS**: Designing NEW products, services, processes, or major system redesigns where you can bake in both efficiency and quality from the start.

## Workflow Overview

```
Phase 1: Define → Phase 2: Measure → Phase 3: Explore → Phase 4: Develop → Phase 5: Implement
         ↓              ↓               ↓               ↓               ↓
   Scope + Goals    VOC + Baseline  Concepts      Detailed Design  Controls + Monitor
```

## Step-by-Step Instructions

### Phase 1: Define - Scope & Goals

**Action**: Define the design project with clear scope, success criteria, and lean/quality targets.

#### 1.1: Create Design Charter

**Action**: Document the business case, problem, and design goals.

**Charter components**:

- **Business Case**: Why design this? What's the opportunity?
- **Problem Statement**: What problem does this solve?
- **Goal Statement**: What are we designing?
- **Scope**: What's included/excluded?
- **Lean Target**: Waste reduction goal (faster, simpler, cheaper)
- **Quality Target**: Defect prevention goal (reliability, robustness)
- **Success Criteria**: How will we know it worked?
- **Timeline**: Key milestones

**Example charter**:

```markdown
## DfLSS Design Charter

**Business Case**: Current order processing takes 48 hours (slow, expensive). Need new system.
**Problem**: Manual order entry, multiple system handoffs, high error rate (5%)
**Goal**: Design automated order system with <2 hour processing
**Scope**:
- Order entry automation
- System integration
- Quality control
- Excluded: Customer portal redesign

**Lean Target**: 48h → 2h (95% waste reduction), 50% cost reduction
**Quality Target**: 5% error rate → <0.1% (50x improvement)
**Success Criteria**:
- Processing time < 2 hours
- Error rate < 0.1%
- Cost per order reduced by 50%
- Customer satisfaction > 90%
**Timeline**: 12 weeks (3 weeks per phase)
```

#### 1.2: Define Lean & Quality Targets

**Action**: Set specific targets for both waste elimination (Lean) and defect prevention (Six Sigma).

**Lean targets** (waste elimination):
- Speed: Target process time
- Cost: Target cost per unit/transaction
- Complexity: Target number of steps/systems
- Resources: Target resource utilization

**Six Sigma targets** (quality/robustness):
- Defect rate: Target defects per million (DPM)
- Reliability: Target uptime percentage
- Consistency: Target variation (sigma level)
- Safety: Target safety incidents

**Example targets**:

```markdown
## DfLSS Targets

**Lean Targets** (Waste Elimination):
- Process time: 48h → 2h (95% reduction)
- Cost per order: $50 → $25 (50% reduction)
- Process steps: 15 → 5 (66% reduction)
- System handoffs: 8 → 2 (75% reduction)

**Six Sigma Targets** (Quality/Robustness):
- Error rate: 5% (50,000 DPM) → 0.1% (1,000 DPM) (50x improvement)
- System availability: 99.5% → 99.99%
- Process variation: 3 sigma → 4+ sigma
- Mean time to recovery: 2 hours → 15 minutes
```

#### 1.3: Identify Lean Waste (Muda, Mura, Muri)

**Action**: Understand what wastes exist in current process.

**Lean waste types**:
- **Muda** (Waste): 7 types - overproduction, waiting, transport, over-processing, inventory, motion, defects
- **Mura** (Variation): Inconsistent processes, inconsistent quality
- **Muri** (Overburden): Unrealistic demands, impossible schedules

**Action**: Analyze current process for waste

```bash
# Document existing order processing waste
# Muda: Long wait times (batching), manual entry errors, system delays
# Mura: Inconsistent processing time (5 min to 2 hours), inconsistent quality
# Muri: Team overworked, manual entry burnout
```

---

### Phase 2: Measure - Customer Needs & Baseline

**Action**: Capture customer needs and measure current process baseline.

#### 2.1: Voice of Customer (VOC)

**Action**: Capture what customers actually want (not how to implement).

**VOC methods**:
- Interviews: Direct customer conversations
- Surveys: Structured questionnaires
- Observations: Watch customers use current solution
- Data Analysis: Usage patterns, pain points

**Action**: Conduct VOC analysis

```markdown
## Voice of Customer - Order Processing

**Need 1**: "I need orders processed TODAY, not in 2 days"
**Need 2**: "Accurate orders - we can't have errors"
**Need 3**: "Simple - I don't want to deal with multiple systems"
**Need 4**: "Feedback - I want to know order status without calling"
**Need 5**: "Reliable - never lose or duplicate my order"
```

#### 2.2: Quality Function Deployment (QFD)

**Action**: Translate customer needs into design requirements.

**QFD House of Quality**:
- **Customer Needs** (rows): What customers want
- **Design Requirements** (columns): How we'll meet needs
- **Relationships**: Strong/Medium/Weak impact
- **Importance**: Customer priority (1-9)
- **Targets**: Target values for requirements

**Example QFD**:

```markdown
## QFD - Order Processing

**Customer Need**: "Process orders fast (today, not 2 days)"
- Design Requirement: Order processing time
- Relationship: Strong (directly addresses)
- Importance: 9/10 (critical)
- Target: < 2 hours

**Customer Need**: "Accurate - no errors"
- Design Requirement: Defect rate
- Relationship: Strong
- Importance: 9/10
- Target: < 0.1%

**Customer Need**: "Simple - one system"
- Design Requirement: System complexity (number of handoffs)
- Relationship: Strong
- Importance: 7/10
- Target: < 2 handoffs
```

#### 2.3: Measure Current Baseline

**Action**: Measure current process performance against targets.

**Baseline metrics**:

```bash
# Process time
# Current: 48 hours average, 5-72 hours range
# Target: < 2 hours
# Gap: 46 hours to eliminate

# Error rate
# Current: 5% (50,000 DPM)
# Target: < 0.1% (1,000 DPM)
# Gap: 49,000 DPM to eliminate

# System handoffs
# Current: 8 handoffs (manual → system A → manual → system B → manual → etc.)
# Target: < 2 handoffs
# Gap: 6 handoffs to eliminate

# Cost per order
# Current: $50
# Target: $25
# Gap: $25 to eliminate
```

**Example baseline report**:

```markdown
## Current State Baseline

**Process Time**: 48h average (5-72h range) - Need 46h reduction
**Error Rate**: 5% - Need 49,000 DPM reduction
**System Handoffs**: 8 - Need 6 handoff elimination
**Cost per Order**: $50 - Need $25 reduction
**Customer Satisfaction**: 65% - Need 25-point improvement

**Top pain points**:
1. Manual order entry (bottleneck, error source)
2. Multiple system handoffs (delay, data loss)
3. Inconsistent processing (no standardization)
4. No order visibility (customers call constantly)
```

---

### Phase 3: Explore - Concepts & Strategy

**Action**: Generate and evaluate design concepts that address Lean + Quality targets.

#### 3.1: Concept Generation - Lean & Quality

**Action**: Generate concepts that specifically address waste (Lean) AND defects (Quality).

**Concept generation methods**:
- TRIZ: Innovative concepts using contradiction analysis
- Lean thinking: Eliminate Muda/Mura/Muri
- Robust design: Prevent defects at source
- Benchmarking: Learn from similar solutions

**Example concepts**:

```markdown
## Design Concepts

**Concept 1**: Automated Order Entry + Single System
- Lean benefit: Eliminates manual entry (biggest waste source)
- Quality benefit: Prevents data entry errors (biggest defect source)
- Cost: High upfront, saves $30/order in labor
- Risk: System integration complexity

**Concept 2**: Structured Process + Quality Gates
- Lean benefit: Standardizes steps (Mura elimination)
- Quality benefit: Built-in inspection points (Poka-Yoke)
- Cost: Moderate (process design + training)
- Risk: Requires discipline to maintain

**Concept 3**: Real-Time Visibility + Customer Notification
- Lean benefit: Reduces customer calls (waste elimination)
- Quality benefit: Early defect detection (customer feedback loop)
- Cost: Low (API for notifications)
- Risk: Requires integration with systems

**Concept 4**: Hybrid Approach (1 + 2 + 3)
- Combine automation, structured process, visibility
- Addresses both Lean targets (95% time reduction) AND Quality targets (50x error reduction)
```

#### 3.2: Concept Selection - Lean & Quality Scorecard

**Action**: Select concepts using Lean + Quality criteria.

**Selection criteria**:

```markdown
## Concept Selection Matrix

**Concept**: Hybrid (Automation + Process + Visibility)

**Lean Evaluation** (40% weight):
- Waste elimination: 9/10 (addresses all Muda, Mura, Muri)
- Cost reduction: 8/10 (labor savings dominate)
- Time reduction: 9/10 (automation + parallel processing)
- Complexity reduction: 8/10 (fewer handoffs)
- Lean score: 8.5/10 → 40% = 3.4 points

**Quality Evaluation** (40% weight):
- Error prevention: 9/10 (automation eliminates entry errors)
- Robustness: 8/10 (quality gates catch issues)
- Consistency: 9/10 (automated process = consistent)
- Reliability: 8/10 (system uptime focus)
- Quality score: 8.5/10 → 40% = 3.4 points

**Implementation Evaluation** (20% weight):
- Feasibility: 7/10 (requires integration work)
- Timeline: 6/10 (12 weeks is tight)
- Risk: 7/10 (integration risks manageable)
- Resources: 7/10 (standard tech stack)
- Implementation score: 6.75/10 → 20% = 1.4 points

**Total Score**: 3.4 + 3.4 + 1.4 = 8.2/10 ✅ (Select this concept)
```

#### 3.3: Design Strategy - Lean & Quality Integration

**Action**: Plan how Lean and Quality are integrated in design.

**Integration strategy**:

```markdown
## DfLSS Design Strategy

**Waste Elimination (Lean)** built into design:
- Automation: Remove manual steps (Muda)
- Standardization: Single process (Mura)
- Pacing: Automatic pacing eliminates rush/delays (Muri)

**Defect Prevention (Quality)** built into design:
- Validation: Automated validation at entry (Poka-Yoke)
- Quality Gates: Inspection points before critical handoffs
- Monitoring: Real-time alerts for anomalies
- Traceability: Full audit trail for defect investigation

**Design ensures**: Fast (Lean) AND Accurate (Quality) by design
```

---

### Phase 4: Develop - Detailed Design & Validation

**Action**: Develop detailed design that meets both Lean and Quality targets.

#### 4.1: Detailed Design

**Action**: Create detailed design specifications.

**Design components**:

- **Architecture**: System structure, components, integration
- **Processes**: Step-by-step process with quality gates
- **Data Model**: Data structures and validation rules
- **Quality Controls**: Inspection points, alerts, thresholds
- **Error Handling**: Failure recovery, fallback processes
- **Monitoring**: Metrics, dashboards, alerts

**Example design outline**:

```markdown
## DfLSS Detailed Design

**Process Architecture**:
1. Order received (automated from EDI/API/web)
2. Validation gate: Check for completeness, format, data quality
3. Auto-processing: Assign to warehouse, generate pick list
4. Warehouse processing: Pick, pack, label
5. Quality gate: Verify order, scan barcode, check contents
6. Shipment: Generate tracking, notify customer
7. Monitoring: Track processing time, error rate, customer feedback

**Quality Controls** (Poka-Yoke):
- Entry validation: Prevent incomplete orders
- Automated assignment: No manual errors
- Barcode verification: Catch wrong items
- Customer notification: Early feedback loop
- Escalation: Flag any anomalies

**Monitoring Metrics**:
- Processing time: Target < 2 hours, alert if > 4 hours
- Error rate: Target < 0.1%, alert if > 0.5%
- Quality gate pass rate: Target > 99.9%
- Customer satisfaction: Track feedback, target > 90%
```

#### 4.2: Design of Experiments (DOE)

**Action**: Use DOE to optimize design parameters.

**DOE process**:

1. Identify factors: What design parameters affect results?
2. Identify levels: What values to test?
3. Design experiment: Which combinations to test?
4. Execute and collect data
5. Analyze results: Which parameters matter most?

**Example DOE**:

```markdown
## DOE - Order Processing Design Optimization

**Factors**:
- Validation strictness: Loose vs Strict (Level: -1, +1)
- Automation level: 50% vs 100% (Level: -1, +1)
- Quality gates: 1 gate vs 3 gates (Level: -1, +1)

**Experiment**: Full factorial (2³ = 8 runs)

**Results**:
- Run 1 (Loose, 50%, 1 gate): 95% accuracy, 3 hours
- Run 2 (Loose, 50%, 3 gates): 98% accuracy, 4 hours
- Run 3 (Loose, 100%, 1 gate): 97% accuracy, 1.5 hours
- Run 4 (Loose, 100%, 3 gates): 99.2% accuracy, 2 hours ✅
- Run 5 (Strict, 50%, 1 gate): 98% accuracy, 3.5 hours
- Run 6 (Strict, 50%, 3 gates): 99.5% accuracy, 5 hours
- Run 7 (Strict, 100%, 1 gate): 99% accuracy, 2 hours ✅
- Run 8 (Strict, 100%, 3 gates): 99.8% accuracy, 2.5 hours ✅

**Analysis**:
- Most important factor: Automation level (6x impact on time)
- Second factor: Validation strictness (quality improvement)
- Third factor: Quality gates (diminishing returns after 2)
- **Optimal parameters**: Strict validation + 100% automation + 2 gates
```

#### 4.3: Robust Design

**Action**: Design for robustness - performance under variation.

**Robustness targets**:

- Works under varying conditions
- Handles edge cases
- Recovers from failures
- Performance within tolerance even when inputs vary

**Example robust design**:

```markdown
## Robust Design - Order Processing

**Factor: Order data quality variation**
- Current: High variation (missing fields, format inconsistency)
- Robust design: Auto-detection and correction of common variations
- Mechanism: Pre-processing layer that normalizes data
- Result: Works regardless of input quality

**Factor: System availability variation**
- Current: Depends on single system (90% uptime)
- Robust design: Fallback systems, queue-based processing
- Mechanism: Message queue buffers orders, retry logic
- Result: 99.99% effective availability even with 99.9% system uptime

**Factor: Load variation**
- Current: Performance degrades with load (1h at 100 orders/day, 4h at 500)
- Robust design: Parallel processing, auto-scaling
- Mechanism: Distributed workers, auto-scaling rules
- Result: Consistent 2h performance from 100 to 5000 orders/day

**Result**: Design works reliably under real-world conditions
```

#### 4.4: Verify Design Meets Targets

**Action**: Verify design meets both Lean and Quality targets.

```markdown
## Design Verification vs Targets

**Lean Targets**:
- ✅ Process time: Design enables 2h (48h → 2h, 95% reduction)
- ✅ Cost: Design eliminates $30 labor per order (50% reduction)
- ✅ Complexity: Design reduces to 2 handoffs (from 8)
- ✅ Resources: Design enables 50% fewer staff

**Quality Targets**:
- ✅ Error rate: Design achieves 0.08% (< 0.1% target)
- ✅ System availability: Design enables 99.99% (> 99.99% target)
- ✅ Consistency: Design achieves 4.2 sigma (> 4 sigma target)
- ✅ Recovery time: Design recovers in < 15 min (< 15 min target)

**Conclusion**: ✅ Design meets ALL Lean and Quality targets
```

---

### Phase 5: Implement - Controls & Monitoring

**Action**: Implement design with controls and ongoing monitoring.

#### 5.1: Implementation Planning

**Action**: Plan phased rollout with risk mitigation.

**Implementation phases**:

```markdown
## Implementation Phases

**Phase 1**: Prototype (Week 1-2)
- Build minimal system
- Test with 10 orders
- Verify time and error targets
- Risk: Development delays

**Phase 2**: Pilot (Week 3-6)
- Deploy to 25% of orders
- Run in parallel with old system
- Monitor metrics continuously
- Risk: Integration issues

**Phase 3**: Ramp (Week 7-10)
- Deploy to 75% of orders
- Monitor performance
- Fix issues as discovered
- Risk: Load issues

**Phase 4**: Full Rollout (Week 11-12)
- 100% of orders on new system
- Decommission old system
- Establish control processes
- Risk: Unforeseen issues
```

#### 5.2: Establish Control Measures

**Action**: Create controls to ensure design sustains performance.

**Control types**:

- **Process Controls**: Standard procedures, checklists, training
- **Statistical Controls**: Monitor metrics, alert on drift
- **Quality Gates**: Inspection points, escalation rules
- **Operational Controls**: Monitoring dashboards, alert thresholds

**Action**: Create control todos (10+ items)

```markdown
## Control Implementation Todos

**Process Controls**:
- [ ] Document standard order processing procedure
- [ ] Create operator training program
- [ ] Establish quality gate checklist
- [ ] Create incident escalation procedure
- [ ] Verify training completion

**Statistical Controls**:
- [ ] Set up processing time dashboard
- [ ] Set up error rate monitoring
- [ ] Configure alerts: > 4 hours processing
- [ ] Configure alerts: > 0.5% error rate
- [ ] Establish daily metric review

**Quality Gates**:
- [ ] Implement automated validation gate
- [ ] Implement manual verification gate for edge cases
- [ ] Create escalation rules for failed orders
- [ ] Establish daily quality review

**Operational Controls**:
- [ ] Set up real-time monitoring dashboard
- [ ] Create on-call escalation procedure
- [ ] Document failure recovery procedures
- [ ] Establish weekly performance review meeting
- [ ] Create continuous improvement suggestion process
```

**Execution**:

1. Create todos using TodoWrite tool
2. Execute todos one by one
3. Mark todos as completed
4. Verify each control works
5. Continue until all controls implemented

#### 5.3: Establish Monitoring & Response

**Action**: Set up monitoring to catch issues early.

**Monitoring approach**:

```markdown
## Monitoring & Response Plan

**Real-Time Monitoring**:
- Processing time: Alert if > 4 hours
- Error rate: Alert if > 0.5%
- System availability: Alert if < 99.5%

**Daily Reviews**:
- Check if targets met (< 2 hours, < 0.1% error)
- Identify issues for resolution
- Capture improvements for Kaizen

**Weekly Reviews**:
- Trend analysis (processing time, error rate over time)
- Root cause analysis for any failures
- Identify Kaizen opportunities
- Update controls if needed

**Monthly Reviews**:
- Overall performance vs targets
- Customer satisfaction feedback
- Identify larger improvements (DMEDI cycle)
- Plan next phase of improvements
```

---

## Complete DfLSS Workflow Example

```markdown
# DfLSS - Order Processing Design

## Phase 1: Define
**Goal**: Design order system with 95% time reduction + 50x error reduction
**Lean target**: 48h → 2h, cost $50 → $25
**Quality target**: 5% error → 0.1% error

## Phase 2: Measure
**VOC**: Fast, accurate, simple, reliable
**QFD**: Process time < 2h, error < 0.1%, < 2 handoffs
**Baseline**: 48h, 5% error, 8 handoffs, $50

## Phase 3: Explore
**Concepts**: Automation + Structured process + Visibility
**Selected**: Hybrid approach addresses both Lean and Quality

## Phase 4: Develop
**Design**: Automated entry → Validation → Auto-processing → Verification
**DOE**: Optimal parameters (100% automation + strict validation + 2 gates)
**Robust**: Handles data variation, system unavailability, load variation
**Verification**: Design meets all targets ✅

## Phase 5: Implement
**Pilot**: 10 orders → 25% → 75% → 100%
**Controls**: Process controls, statistical monitoring, quality gates
**Results**: 1.5h processing (< 2h), 0.08% error (< 0.1%), satisfied customers
```

---

## Integration with Other Commands

- **[DMEDI Design Process](./dmedi-design-process.md)** - Use DMEDI phases as implementation approach
- **[Voice of Customer (QFD)](./voice-of-customer-qfd.md)** - Use in Measure phase
- **[Concept Selection](./concept-selection.md)** - Use in Explore phase
- **[Robust Design](./robust-design.md)** - Use in Develop phase
- **[FMEA](./fmea.md)** - Use to identify design failure modes
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Use to add mistake-proofing
- **[Eliminate Muda](./eliminate-muda.md)** - Reference for waste elimination in Lean targets
- **[Eliminate Mura](./eliminate-mura.md)** - Reference for variation elimination
- **[Eliminate Muri](./eliminate-muri.md)** - Reference for overburden elimination
- **[Kaizen (Continuous Improvement)](./kaizen-improvement.md)** - Use after implementation for ongoing improvements
- **[Andon Signals](./andon-signals.md)** - Use for monitoring and control phase

---

## Expert Insights

**Why DfLSS matters**: Big problems require big solutions. Small improvements (Kaizen) won't solve design-level issues. DfLSS designs solutions that are both fast (Lean) and accurate (Quality) from the start.

**DfLSS vs DFSS critical distinction**:
- **DFSS** (Design for Six Sigma) = Quality only → Defect-free but wasteful
- **DfLSS** (Design for Lean Six Sigma) = Quality + Efficiency → Defect-free AND efficient (CORRECT)
- Many teams misapply DFSS when they need DfLSS, resulting in over-engineered solutions with unnecessary complexity

**Key principles**:

1. **Customer-first**: Start with Voice of Customer, not assumptions
2. **Lean + Quality**: Integrate waste elimination AND defect prevention from design phase
3. **Data-driven**: Use data (baseline, experiments, monitoring) to drive decisions
4. **Systematic**: Follow DMEDI phases - don't skip steps
5. **Fail early**: Pilot and test before full rollout
6. **Control from start**: Build controls into design, not as afterthought

**When DfLSS is essential**:
- Designing new products or services
- Major process redesigns
- When both speed and accuracy matter
- When you need to bake in quality and efficiency
- When the problem requires design thinking, not just improvement

**When to use Kaizen instead**:
- Improving existing processes
- Small incremental changes
- Safety improvements
- When design is fine, just need tweaks

**Remember**: DfLSS designs right the first time. Kaizen improves incrementally. Use DfLSS for design problems, Kaizen for improvement opportunities.

---

## Command Execution Pattern

**CRITICAL**: DfLSS implementation must:

1. **Integrate Lean + Quality throughout** - Not quality added as afterthought
2. **Create 10+ todo list** - Not documents/reports
3. **Execute with phases** - DMEDI provides the structure
4. **Verify targets met** - Confirm design meets both Lean and Quality targets
5. **Establish controls from day 1** - Sustain improvements from start
6. **Monitor continuously** - Real-time alerts, daily/weekly reviews

**Principle**: Design for Lean Six Sigma means integrating waste elimination and defect prevention from day one. Don't design for quality only - that's DFSS and creates waste. DfLSS designs fast AND accurate.

---

## Important Note: Methodology Overview

**This is a methodology overview**, not an execution command. For actual implementation:

- **New Design/System**: Use `/dmedi-design-process` - implements all 5 DMEDI phases with execution todos
- **Existing Process Improvement**: Use `/dmaic-problem-solving` - data-driven problem solving
- **Continuous Kaizen**: Use `/kaizen-improvement` - small incremental improvements
- **Lean Waste Elimination**: Use `/eliminate-muda`, `/eliminate-mura`, `/eliminate-muri`
- **Quality/Error Prevention**: Use `/poka-yoke-design`, `/fmea`, `/robust-design`

This `/dflss` command provides theoretical context and methodology alignment. Use the specific execution commands above for actual implementation with todos and deliverables.

---

## See Also

### Execution Commands
- **[DMEDI Design Process](/dmedi-design-process.md)** - Execute DfLSS phases (Define → Measure → Explore → Develop → Implement) for new designs
- **[DMAIC Problem Solving](/dmaic-problem-solving.md)** - Data-driven problem solving using DMAIC phases
- **[Kaizen (Continuous Improvement)](/kaizen-improvement.md)** - Small, incremental improvements with PDCA cycle

### Lean Elimination (DfLSS Efficiency)
- **[Eliminate Muda (Waste)](/eliminate-muda.md)** - Eliminate 7 types of waste
- **[Eliminate Mura (Variation)](/eliminate-mura.md)** - Standardize processes and patterns
- **[Eliminate Muri (Overburden)](/eliminate-muri.md)** - Reduce complexity and cognitive load

### Quality & Defect Prevention (DfLSS Quality)
- **[Poka-Yoke Design (Error Prevention)](/poka-yoke-design.md)** - Make invalid states unrepresentable
- **[FMEA (Failure Mode Analysis)](/fmea.md)** - Proactive risk assessment
- **[Robust Design](/robust-design.md)** - Design for resilience and variation

### Root Cause & Analysis
- **[Root Cause Analysis](/root-cause-analysis.md)** - Find underlying causes; includes detailed DfLSS vs DFSS explanation (anchor link for all commands)

### Design & Requirements
- **[Voice of Customer (QFD)](/voice-of-customer-qfd.md)** - Translate customer needs into design requirements
- **[Concept Selection](/concept-selection.md)** - Systematic evaluation of design concepts
- **[80-20 Fill Gaps](/80-20-fill-gaps.md)** - Pareto principle for prioritization

### Process Control & Monitoring
- **[Andon Signals](/andon-signals.md)** - Visual problem detection and response
- **[Gemba Walk](/gemba-walk.md)** - Go to the source and observe reality

---

End Command ---
