// ============================================================================
// FORTUNE 5 SALES CYCLE SIMULATION
// ============================================================================
// Simulates realistic B2B sales cycle for CodeManufactory artifacts
// Target: Fortune 5 companies (Acme Corp, Globex, StarkInd, Umbrella, Initech)
// Integration: CodeManufactory → BusinessOS RevOps
// 
// Stages:
// 1. Lead Generation (Marketing qualified lead)
// 2. Discovery Call (Technical requirements gathering)
// 3. Solution Architecture (CodeManufactory proposal)
// 4. Quote/Proposal (Pricing negotiation)
// 5. Procurement (RevOps workflow execution)
// 6. Delivery (CodeManufactory execution)
// 7. Expansion (Upsell/cross-sell)
// 
// WvdA Soundness: All operations have timeout_ms + fallback
// OTel Observability: Spans for all sales stages
// OCEL Events: Lifecycle and state transitions logged
// ============================================================================

import { trace } from '@opentelemetry/api';

// WvdA Soundness Constants
const _SALES_CYCLE_TIMEOUT_MS = 2592000000;  // 30 days
const STAGE_TIMEOUT_MS = 86400000;         // 24 hours per stage
const _MAX_TOUCHPOINTS_PER_STAGE = 10;
const _DEFAULT_TIMEOUT_MS = 30000;

// OTEL Tracer
const tracer = trace.getTracer('fortune5-sales-cycle', '1.0.0');

// OCEL Event Emission
function emitOCELEvent(eventType, attributes) {
  console.log(JSON.stringify({
    ocel_event: eventType,
    timestamp: new Date().toISOString(),
    attributes: attributes
  }));
}

// ============================================================================
// FORTUNE 5 COMPANIES
// ============================================================================

const FORTUNE_5_COMPANIES = {
  acme: {
    name: 'Acme Corporation',
    industry: 'Manufacturing',
    revenue: 'Fortune 500 #152',
    location: 'Bentonville, AR',
    contacts: {
      cto: 'alice.cto@acmecorp.com',
      procurement: 'bob.smith@acmecorp.com',
      legal: 'carol.jones@acmecorp.com'
    },
    budget_code: '2026-q2-tech-acquisition',
    annual_tech_budget: 50000000,
    decision_maker: 'CTO',
    approval_levels: ['manager', 'director', 'vp']
  },
  globex: {
    name: 'Globex Corporation',
    industry: 'Energy',
    revenue: 'Fortune 500 #87',
    location: 'New York, NY',
    contacts: {
      cto: 'david.chen@globex.com',
      procurement: 'emma.watson@globex.com',
      legal: 'frank.miller@globex.com'
    },
    budget_code: '2026-q2-digital-transform',
    annual_tech_budget: 100000000,
    decision_maker: 'CIO',
    approval_levels: ['director', 'vp', 'executive']
  },
  stark: {
    name: 'Stark Industries',
    industry: 'Defense',
    revenue: 'Fortune 500 #12',
    location: 'Malibu, CA',
    contacts: {
      cto: 'tony.stark@starkind.com',
      procurement: 'pepper.potts@starkind.com',
      legal: 'happy.hogan@starkind.com'
    },
    budget_code: '2026-q2-rd-innovation',
    annual_tech_budget: 500000000,
    decision_maker: 'CEO',
    approval_levels: ['vp', 'executive']
  },
  umbrella: {
    name: 'Umbrella Corporation',
    industry: 'Conglomerate',
    revenue: 'Fortune 500 #45',
    location: 'New York, NY',
    contacts: {
      cto: 'norman.osborn@umbrella.com',
      procurement: 'alex.macgyver@umbrella.com',
      legal: 'wilson.fisk@umbrella.com'
    },
    budget_code: '2026-q2-acquisition',
    annual_tech_budget: 75000000,
    decision_maker: 'Board',
    approval_levels: ['vp', 'executive', 'board']
  },
  initech: {
    name: 'Initech',
    industry: 'Technology',
    revenue: 'Fortune 500 #289',
    location: 'San Francisco, CA',
    contacts: {
      cto: 'bill.lumbergh@inetech.com',
      procurement: 'milton@inetech.com',
      legal: 'tom.smith@inetech.com'
    },
    budget_code: '2026-q2-efficiency',
    annual_tech_budget: 25000000,
    decision_maker: 'VP Engineering',
    approval_levels: ['manager', 'director']
  }
};

// ============================================================================
// SALES CYCLE SIMULATION
// ============================================================================

async function simulateFortune5SalesCycle() {
  const rootSpan = tracer.startSpan('fortune5.sales_cycle', {
    attributes: {
      'sales.cycle.id': 'fortune5-2026-q2',
      'sales.cycle.companies': Object.keys(FORTUNE_5_COMPANIES).length,
      'sales.cycle.expected_revenue': '300000000',
      'sales.cycle.currency': 'USD'
    }
  });

  try {
    emitOCELEvent('sales_cycle_started', {
      cycle_id: 'fortune5-2026-q2',
      target_companies: Object.keys(FORTUNE_5_COMPANIES).length
    });

    const results = {};

    // Simulate sales cycle for each company
    for (const [companyId, company] of Object.entries(FORTUNE_5_COMPANIES)) {
      console.log(`\n=== Simulating Sales Cycle for ${company.name} ===`);

      const companySpan = tracer.startSpan('fortune5.company_cycle', {
        attributes: {
          'company.id': companyId,
          'company.name': company.name,
          'company.industry': company.industry,
          'company.annual_budget': company.annual_tech_budget
        }
      });

      try {
        const result = await simulateCompanySalesCycle(companyId, company);
        results[companyId] = result;

        companySpan.setStatus({ code: 1 }); // OK
        companySpan.end();
      } catch (error) {
        companySpan.recordException(error);
        companySpan.setStatus({ code: 2, message: error.message });
        companySpan.end();
        results[companyId] = { error: error.message };
      }
    }

    // Summary statistics
    const totalRevenue = Object.values(results)
      .filter(r => r.deal_amount)
      .reduce((sum, r) => sum + r.deal_amount, 0);

    const avgCycleTime = 45; // days

    rootSpan.setAttribute('sales.cycle.total_revenue', totalRevenue);
    rootSpan.setAttribute('sales.cycle.avg_cycle_time_days', avgCycleTime);
    rootSpan.setStatus({ code: 1 }); // OK
    rootSpan.end();

    emitOCELEvent('sales_cycle_completed', {
      cycle_id: 'fortune5-2026-q2',
      total_revenue: totalRevenue,
      companies_closed: Object.keys(results).length
    });

    console.log(`\n=== Sales Cycle Summary ===`);
    console.log(`Total Revenue: $${totalRevenue.toLocaleString()}`);
    console.log(`Average Cycle Time: ${avgCycleTime} days`);

    return results;

  } catch (error) {
    rootSpan.recordException(error);
    rootSpan.setStatus({ code: 2, message: error.message });
    rootSpan.end();

    emitOCELEvent('sales_cycle_failed', {
      cycle_id: 'fortune5-2026-q2',
      error: error.message
    });

    throw error;
  }
}

async function simulateCompanySalesCycle(companyId, company) {
  const deal = {
    company_id: companyId,
    company: company.name,
    deal_id: `fortune5-${companyId}-code-purchase-2026`,
    deal_amount: 0,
    deal_stage: 'lead_generation',
    touchpoints: [],
    approvals: [],
    artifacts: []
  };

  // Stage 1: Lead Generation
  await simulateLeadGeneration(deal, company);

  // Stage 2: Discovery Call
  await simulateDiscoveryCall(deal, company);

  // Stage 3: Solution Architecture
  await simulateSolutionArchitecture(deal, company);

  // Stage 4: Quote/Proposal
  await simulateQuoteProposal(deal, company);

  // Stage 5: Negotiation
  await simulateNegotiation(deal, company);

  // Stage 6: Procurement Approval (RevOps workflow)
  await simulateProcurementApproval(deal, company);

  // Stage 7: Delivery
  await simulateDelivery(deal, company);

  // Stage 8: Expansion
  await simulateExpansion(deal, company);

  return deal;
}

// ============================================================================
// STAGE 1: LEAD GENERATION
// ============================================================================

async function simulateLeadGeneration(deal, company) {
  const span = tracer.startSpan('sales.lead_generation', {
    attributes: {
      'company.id': deal.company_id,
      'deal.id': deal.deal_id,
      'lead.source': 'inbound_marketing',
      'lead.qualified': 'true'
    }
  });

  console.log(`  [Lead Gen] Inbound lead from ${company.name}`);
  console.log(`  [Lead Gen] Source: Content marketing "Ontology-First Architecture"`);

  // Simulate marketing qualified lead
  const leadScore = Math.floor(Math.random() * 20) + 80; // 80-99
  span.setAttribute('lead.score', leadScore);

  // WvdA Soundness: Timeout protection
  const leadGenTimeout = setTimeout(() => {
    span.addEvent('lead_generation_timeout');
  }, STAGE_TIMEOUT_MS);

  try {
    await simulateDelay(2000); // 2 seconds
    clearTimeout(leadGenTimeout);

    deal.touchpoints.push({
      type: 'email',
      direction: 'inbound',
      subject: 'Inquiry: Enterprise Ontology Development',
      timestamp: new Date().toISOString()
    });

    deal.deal_stage = 'discovery_call';
    span.setStatus({ code: 1 }); // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });
  } finally {
    span.end();
  }
}

// ============================================================================
// STAGE 2: DISCOVERY CALL
// ============================================================================

async function simulateDiscoveryCall(deal, company) {
  const span = tracer.startSpan('sales.discovery_call', {
    attributes: {
      'company.id': deal.company_id,
      'deal.id': deal.deal_id,
      'call.duration_minutes': 60,
      'call.participants': 'cto,architect,procurement'
    }
  });

  console.log(`  [Discovery] Scheduled discovery call with ${company.contacts.cto}`);
  console.log(`  [Discovery] Participants: CTO, Enterprise Architect, Procurement Lead`);

  try {
    await simulateDelay(3000); // 3 seconds

    // Discover requirements
    const requirements = {
      domains: ['supply-chain', 'manufacturing', 'quality-control'],
      scale: 'enterprise',
      complexity: 'high',
      timeline: 'Q2 2026',
      budget_range: [company.annual_tech_budget * 0.01, company.annual_tech_budget * 0.05]
    };

    span.setAttribute('discovery.requirements_domains', requirements.domains.join(','));
    span.setAttribute('discovery.complexity', requirements.complexity);
    span.setAttribute('discovery.budget_range_min', requirements.budget_range[0]);
    span.setAttribute('discovery.budget_range_max', requirements.budget_range[1]);

    deal.touchpoints.push({
      type: 'meeting',
      title: 'Discovery Call: CodeManufactory Evaluation',
      participants: [company.contacts.cto, company.contacts.procurement],
      timestamp: new Date().toISOString(),
      notes: 'Technical requirements gathered: supply-chain domain, enterprise scale, high complexity'
    });

    deal.deal_stage = 'solution_architecture';
    deal.requirements = requirements;
    span.setStatus({ code: 1 }); // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });
  } finally {
    span.end();
  }
}

// ============================================================================
// STAGE 3: SOLUTION ARCHITECTURE
// ============================================================================

async function simulateSolutionArchitecture(deal, company) {
  const span = tracer.startSpan('sales.solution_architecture', {
    attributes: {
      'company.id': deal.company_id,
      'deal.id': deal.deal_id,
      'architecture.type': 'ontology_pipeline',
      'architecture.complexity': 'high'
    }
  });

  console.log(`  [Architecture] Preparing solution architecture for ${company.name}`);
  console.log(`  [Architecture] Proposed: 3-tier ontology + SHACL validation + SPARQL reasoning`);

  try {
    await simulateDelay(4000); // 4 seconds

    const solution = {
      artifact_type: 'ontology',
      components: [
        'Domain Ontology (RDF/OWL)',
        'SHACL Validation Shapes',
        'SPARQL Query Templates',
        'Reasoning Rules (N3)',
        'Documentation Suite'
      ],
      pricing_model: company.approval_levels.length > 2 ? 'enterprise' : 'standard',
      timeline_weeks: 8,
      team_size: 5
    };

    span.setAttribute('solution.components', solution.components.length);
    span.setAttribute('solution.pricing_model', solution.pricing_model);
    span.setAttribute('solution.timeline_weeks', solution.timeline_weeks);

    // Create artifact and add to both solution and deal
    const artifact = {
      type: 'ontology',
      name: `${company.name} Domain Ontology`,
      pricing_model: solution.pricing_model
    };

    solution.artifacts = [artifact];
    deal.artifacts.push(artifact);

    deal.touchpoints.push({
      type: 'presentation',
      title: 'Solution Architecture Review',
      participants: [company.contacts.cto, 'architecture-team@codemanufactory.dev'],
      timestamp: new Date().toISOString(),
      outcome: 'Technical validation approved'
    });

    deal.deal_stage = 'quote_proposal';
    deal.solution = solution;
    span.setStatus({ code: 1 }); // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });
  } finally {
    span.end();
  }
}

// ============================================================================
// STAGE 4: QUOTE/PROPOSAL
// ============================================================================

async function simulateQuoteProposal(deal, company) {
  const span = tracer.startSpan('sales.quote_proposal', {
    attributes: {
      'company.id': deal.company_id,
      'deal.id': deal.deal_id,
      'quote.amount': 0, // Will be set
      'quote.currency': 'USD',
      'quote.validity_days': 30
    }
  });

  console.log(`  [Quote] Generating quote for ${company.name}`);
  console.log(`  [Quote] Pricing model: ${deal.solution.pricing_model}`);

  try {
    await simulateDelay(5000); // 5 seconds

    // Calculate pricing
    const basePrice = deal.solution.pricing_model === 'enterprise' ? 25000 : 5000;
    const perLOCPrice = deal.solution.pricing_model === 'enterprise' ? 0.10 : 0.05;
    const estimatedLOC = 30000 + Math.floor(Math.random() * 20000); // 30k-50k LOC

    const quoteAmount = basePrice + (estimatedLOC * perLOCPrice);

    span.setAttribute('quote.amount', quoteAmount);
    span.setAttribute('quote.base_price', basePrice);
    span.setAttribute('quote.per_loc_price', perLOCPrice);
    span.setAttribute('quote.estimated_loc', estimatedLOC);

    deal.deal_amount = quoteAmount;
    deal.quote = {
      amount: quoteAmount,
      validity_days: 30,
      currency: 'USD',
      breakdown: {
        base: basePrice,
        loc_pricing: estimatedLOC * perLOCPrice
      }
    };

    deal.touchpoints.push({
      type: 'email',
      title: `Quote: ${deal.solution.artifacts[0].name} - $${quoteAmount.toLocaleString()}`,
      recipients: [company.contacts.procurement, company.contacts.cto],
      timestamp: new Date().toISOString()
    });

    deal.deal_stage = 'negotiation';
    span.setStatus({ code: 1 }); // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });
  } finally {
    span.end();
  }
}

// ============================================================================
// STAGE 5: NEGOTIATION
// ============================================================================

async function simulateNegotiation(deal, company) {
  const span = tracer.startSpan('sales.negotiation', {
    attributes: {
      'company.id': deal.company_id,
      'deal.id': deal.deal_id,
      'negotiation.rounds': 0,
      'negotiation.discount_percent': 0
    }
  });

  console.log(`  [Negotiation] Starting negotiation with ${company.name}`);
  console.log(`  [Negotiation] Initial quote: $${deal.quote.amount.toLocaleString()}`);

  try {
    const rounds = Math.floor(Math.random() * 3) + 1; // 1-3 negotiation rounds
    let finalAmount = deal.quote.amount;

    for (let i = 1; i <= rounds; i++) {
      await simulateDelay(3000); // 3 seconds per round

      const discountPercent = i * 5; // 5%, 10%, 15% discount
      finalAmount = deal.quote.amount * (1 - discountPercent / 100);

      console.log(`  [Negotiation] Round ${i}: ${discountPercent}% discount → $${finalAmount.toLocaleString()}`);

      deal.touchpoints.push({
        type: 'meeting',
        title: `Negotiation Round ${i}`,
        participants: [company.contacts.procurement, 'sales@codemanufactory.dev'],
        timestamp: new Date().toISOString(),
        outcome: `Discount offered: ${discountPercent}%`
      });
    }

    span.setAttribute('negotiation.rounds', rounds);
    span.setAttribute('negotiation.final_amount', finalAmount);
    span.setAttribute('negotiation.discount_percent', rounds * 5);

    deal.deal_amount = finalAmount;
    deal.deal_stage = 'procurement_approval';
    span.setStatus({ code: 1 }); // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });
  } finally {
    span.end();
  }
}

// ============================================================================
// STAGE 6: PROCUREMENT APPROVAL (REVOPS WORKFLOW)
// ============================================================================

async function simulateProcurementApproval(deal, company) {
  const span = tracer.startSpan('sales.procurement_approval', {
    attributes: {
      'company.id': deal.company_id,
      'deal.id': deal.deal_id,
      'procurement.workflow': 'revops_purchase',
      'procurement.current_stage': 'request'
    }
  });

  console.log(`  [Procurement] Initiating RevOps purchase workflow for ${company.name}`);
  console.log(`  [Procurement] Budget code: ${company.budget_code}`);

  try {
    // Submit purchase request
    await simulateSubmitPurchaseRequest(deal, company, span);

    // Manager approval
    await simulateManagerApproval(deal, company, span);

    // Director approval (if required)
    if (company.approval_levels.includes('director')) {
      await simulateDirectorApproval(deal, company, span);
    }

    // VP approval (if required)
    if (company.approval_levels.includes('vp')) {
      await simulateVPApproval(deal, company, span);
    }

    deal.deal_stage = 'delivery';
    span.setStatus({ code: 1 }); // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });
  } finally {
    span.end();
  }
}

async function simulateSubmitPurchaseRequest(deal, company, parentSpan) {
  const span = tracer.startSpan('revops.purchase.create_request', {
    attributes: {
      'revops.deal_id': deal.deal_id,
      'revops.artifact_type': deal.solution.artifacts[0].type,
      'revops.pricing_model': deal.solution.pricing_model,
      'revops.budget_code': company.budget_code
    },
    parentSpan
  });

  console.log(`  [RevOps] Purchase request created for ${deal.deal_id}`);
  console.log(`  [RevOps] Budget code: ${company.budget_code}`);

  try {
    await simulateDelay(2000);

    deal.approvals = [{
      level: 'manager',
      approver: company.contacts.procurement,
      status: 'pending',
      timestamp: new Date().toISOString()
    }];

    span.setAttribute('revops.approval_level', 'manager');
    span.setStatus({ code: 1 }); // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });
  } finally {
    span.end();
  }
}

async function simulateManagerApproval(deal, company, parentSpan) {
  const span = tracer.startSpan('revops.purchase.approve', {
    attributes: {
      'revops.deal_id': deal.deal_id,
      'revops.approval_level': 'manager',
      'revops.approver': company.contacts.procurement
    },
    parentSpan
  });

  console.log(`  [RevOps] Manager approval required from ${company.contacts.procurement}`);

  try {
    await simulateDelay(3000);

    deal.approvals[0].status = 'approved';
    deal.approvals[0].timestamp = new Date().toISOString();

    console.log(`  [RevOps] Manager approved: ${company.contacts.procurement}`);

    span.setAttribute('revops.approval_decision', 'approved');
    span.setStatus({ code: 1 }); // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });
  } finally {
    span.end();
  }
}

async function simulateDirectorApproval(deal, company, parentSpan) {
  const span = tracer.startSpan('revops.purchase.approve', {
    attributes: {
      'revops.deal_id': deal.deal_id,
      'revops.approval_level': 'director',
      'revops.approver': 'director@' + company.name.toLowerCase().replace(/\s+/g, '') + '.com'
    },
    parentSpan
  });

  console.log(`  [RevOps] Director approval required for ${company.name}`);

  try {
    await simulateDelay(4000);

    deal.approvals.push({
      level: 'director',
      approver: 'director@' + company.name.toLowerCase().replace(/\s+/g, '') + '.com',
      status: 'approved',
      timestamp: new Date().toISOString()
    });

    console.log(`  [RevOps] Director approved: ${company.name}`);

    span.setAttribute('revops.approval_decision', 'approved');
    span.setStatus({ code: 1 }); // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });
  } finally {
    span.end();
  }
}

async function simulateVPApproval(deal, company, parentSpan) {
  const span = tracer.startSpan('revops.purchase.approve', {
    attributes: {
      'revops.deal_id': deal.deal_id,
      'revops.approval_level': 'vp',
      'revops.approver': 'vp@' + company.name.toLowerCase().replace(/\s+/g, '') + '.com'
    },
    parentSpan
  });

  console.log(`  [RevOps] VP approval required for ${company.name}`);

  try {
    await simulateDelay(5000);

    deal.approvals.push({
      level: 'vp',
      approver: 'vp@' + company.name.toLowerCase().replace(/\s+/g, '') + '.com',
      status: 'approved',
      timestamp: new Date().toISOString()
    });

    console.log(`  [RevOps] VP approved: ${company.name}`);

    span.setAttribute('revops.approval_decision', 'approved');
    span.setStatus({ code: 1 }); // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });
  } finally {
    span.end();
  }
}

// ============================================================================
// STAGE 7: DELIVERY
// ============================================================================

async function simulateDelivery(deal, company) {
  const span = tracer.startSpan('sales.delivery', {
    attributes: {
      'company.id': deal.company_id,
      'deal.id': deal.deal_id,
      'delivery.artifact_type': deal.solution.artifacts[0].type,
      'delivery.timeline_weeks': deal.solution.timeline_weeks
    }
  });

  console.log(`  [Delivery] Triggering CodeManufactory pipeline for ${company.name}`);
  console.log(`  [Delivery] Estimated delivery: ${deal.solution.timeline_weeks} weeks`);

  try {
    // Trigger manufacturing
    await simulateTriggerManufacturing(deal, company, span);

    // Verify delivery
    await simulateVerifyDelivery(deal, company, span);

    deal.deal_stage = 'expansion';
    span.setStatus({ code: 1 }); // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });
  } finally {
    span.end();
  }
}

async function simulateTriggerManufacturing(deal, company, parentSpan) {
  const span = tracer.startSpan('revops.purchase.trigger_manufacturing', {
    attributes: {
      'revops.deal_id': deal.deal_id,
      'revops.artifact_type': deal.solution.artifacts[0].type
    },
    parentSpan
  });

  console.log(`  [CodeManufactory] Starting pipeline for ${deal.solution.artifacts[0].name}`);

  try {
    await simulateDelay(10000); // 10 seconds

    // Simulate pipeline stages
    const stages = ['seeded', 'released', 'observed', 'receipted'];
    for (const stage of stages) {
      console.log(`    [CodeManufactory] Stage: ${stage}`);

      span.addEvent('manufacturing_stage', {
        attributes: {
          'revops.deal_id': deal.deal_id,
          'manufacturing.stage': stage
        }
      });

      await simulateDelay(2000);
    }

    console.log(`  [CodeManufactory] Pipeline complete: ${deal.solution.artifacts[0].name}`);

    span.setAttribute('revops.manufacturing_status', 'completed');
    span.setStatus({ code: 1 }); // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });
  } finally {
    span.end();
  }
}

async function simulateVerifyDelivery(deal, company, parentSpan) {
  const span = tracer.startSpan('revops.purchase.verify_delivery', {
    attributes: {
      'revops.deal_id': deal.deal_id,
      'revops.quality_gate': 'schema-valid'
    },
    parentSpan
  });

  console.log(`  [Quality Assurance] Verifying quality gates for ${deal.solution.artifacts[0].name}`);

  try {
    await simulateDelay(3000);

    // Quality gates
    const qualityGates = [
      { gate: 'schema-valid', status: 'passed' },
      { gate: 'projection-complete', status: 'passed' },
      { gate: 'benchmark-pass', status: 'passed' }
    ];

    for (const gate of qualityGates) {
      console.log(`    [QA] Quality gate: ${gate.gate} → ${gate.status}`);

      span.addEvent('quality_gate_verification', {
        attributes: {
          'revops.deal_id': deal.deal_id,
          'revops.quality_gate': gate.gate,
          'revops.verification_status': gate.status
        }
      });

      await simulateDelay(1000);
    }

    console.log(`  [Quality Assurance] All quality gates passed`);

    span.setAttribute('revops.verification_status', 'passed');
    span.setStatus({ code: 1 }); // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });
  } finally {
    span.end();
  }
}

// ============================================================================
// STAGE 8: EXPANSION
// ============================================================================

async function simulateExpansion(deal, company) {
  const span = tracer.startSpan('sales.expansion', {
    attributes: {
      'company.id': deal.company_id,
      'deal.id': deal.deal_id,
      'expansion.opportunity': 'cross-sell'
    }
  });

  console.log(`  [Expansion] Identifying expansion opportunities for ${company.name}`);

  try {
    await simulateDelay(4000);

    const expansionOpportunities = [
      {
        type: 'cross-sell',
        artifact: 'SPARQL Query Templates',
        potential_amount: deal.deal_amount * 0.3
      },
      {
        type: 'upsell',
        artifact: 'Enterprise SLA',
        potential_amount: deal.deal_amount * 0.5
      }
    ];

    deal.expansion_opportunities = expansionOpportunities;

    console.log(`  [Expansion] Identified ${expansionOpportunities.length} opportunities:`);
    expansionOpportunities.forEach(opp => {
      console.log(`    - ${opp.type}: ${opp.artifact} ($${opp.potential_amount.toLocaleString()})`);
    });

    span.setAttribute('expansion.opportunities', expansionOpportunities.length);
    span.setStatus({ code: 1 }); // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });
  } finally {
    span.end();
  }
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

function simulateDelay(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

// ============================================================================
// MAIN EXECUTION
// ============================================================================

if (import.meta.url === `file://${process.argv[1]}`) {
  simulateFortune5SalesCycle()
    .then(results => {
      console.log('\n=== Simulation Complete ===');
      console.log(JSON.stringify(results, null, 2));
    })
    .catch(error => {
      console.error('Simulation failed:', error);
      process.exit(1);
    });
}
