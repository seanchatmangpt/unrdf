/**
 * @unrdf/hooks - N3 Forward-Chaining Reasoning Example
 *
 * Demonstrates semantic inference using N3 rules and EYE reasoner.
 * Derives new facts from existing data through logical rules.
 *
 * Use cases:
 * - RDFS/OWL-like reasoning (class hierarchies, property inference)
 * - Business rule execution (access control, workflow logic)
 * - Data enrichment (derive properties from rules)
 * - Compliance checking (derive compliance status)
 */

import { createStore } from '@unrdf/oxigraph';
import { evaluateCondition } from '../src/hooks/condition-evaluator.mjs';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';

const { namedNode, literal, quad } = DataFactory;

console.log('=== N3 Forward-Chaining Reasoning Example ===\n');

// Example 1: Simple class inheritance
console.log('1. Simple Class Inheritance\n');

const classInheritanceCondition = {
  kind: 'n3',
  rules: `
    @prefix : <http://example.org/> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

    # Rule: If X is subclass of Y and Z is instance of X, then Z is instance of Y
    { ?x rdfs:subClassOf ?y . ?z a ?x } => { ?z a ?y } .
  `,
  askQuery: 'ASK { ?s a <http://example.org/Animal> }'
};

console.log('Rules:');
console.log(classInheritanceCondition.rules);
console.log('');
console.log('Scenario:');
console.log('  Data: Dog subClassOf Animal, Fido a Dog');
console.log('  Inference: Fido a Animal (derived)');
console.log('  Query: ASK { ?s a Animal } → true\n');

// Example 2: Access control reasoning
console.log('2. Access Control Reasoning\n');

const accessControlCondition = {
  kind: 'n3',
  rules: `
    @prefix : <http://example.org/> .

    # Rule 1: Admins can access everything
    { ?user a :Admin } => { ?user :canAccess :SecureArea } .

    # Rule 2: Group members inherit group permissions
    { ?user :isMemberOf ?group . ?group :hasPermission ?perm }
      => { ?user :hasPermission ?perm } .

    # Rule 3: Users with edit permission need audit logging
    { ?user :hasPermission :EditContent }
      => { ?user :requiresAuditLog true } .

    # Rule 4: Audit logging required if accessing secure area
    { ?user :canAccess :SecureArea }
      => { ?user :requiresAuditLog true } .
  `,
  askQuery: 'ASK { ?user :requiresAuditLog true }'
};

console.log('Rules demonstrate multi-level inference:');
console.log('  Rule 1: Admin role → access permission');
console.log('  Rule 2: Group membership → inherit permissions');
console.log('  Rule 3: Edit permission → audit requirement');
console.log('  Rule 4: Secure access → audit requirement');
console.log('');
console.log('Scenario:');
console.log('  Data: alice a Admin, bob :isMemberOf :editors');
console.log('  Step 1: alice a Admin → alice :canAccess :SecureArea');
console.log('  Step 2: alice :canAccess :SecureArea → alice :requiresAuditLog true');
console.log('  Step 3: bob :isMemberOf :editors → check group permissions');
console.log('  Result: Audit requirements derived from roles\n');

// Example 3: Business process workflow
console.log('3. Business Process Workflow Reasoning\n');

const workflowCondition = {
  kind: 'n3',
  rules: `
    @prefix : <http://example.org/> .

    # Rule 1: Large trades require compliance review
    { ?trade a :Trade ; :amount ?amt . ?amt > 1000000 }
      => { ?trade :requiresCompliance true } .

    # Rule 2: Compliance required trades need risk assessment
    { ?trade :requiresCompliance true }
      => { ?trade :requiresRiskAssessment true } .

    # Rule 3: Risk assessment must be approved by senior reviewer
    { ?trade :requiresRiskAssessment true }
      => { ?trade :requiresApprovalLevel :Senior } .

    # Rule 4: High-risk trades need compliance officer sign-off
    { ?trade :riskLevel :High ; :requiresApprovalLevel :Senior }
      => { ?trade :requiresComplianceOfficer true } .

    # Rule 5: Anything needing compliance officer needs audit trail
    { ?trade :requiresComplianceOfficer true }
      => { ?trade :auditTrailRequired true } .
  `,
  askQuery: 'ASK { ?trade :auditTrailRequired true }'
};

console.log('Multi-step workflow rules:');
console.log('  Trade amount > $1M → requires compliance');
console.log('  Requires compliance → requires risk assessment');
console.log('  Requires risk assessment → requires senior approval');
console.log('  High risk + senior approval → needs compliance officer');
console.log('  Needs compliance officer → needs audit trail');
console.log('');
console.log('Execution:');
console.log('  Input: Trade with amount $1.5M');
console.log('  Derives: auditTrailRequired = true');
console.log('  Effect: Create audit record in system\n');

// Example 4: Data enrichment
console.log('4. Data Enrichment Through Reasoning\n');

const enrichmentCondition = {
  kind: 'n3',
  rules: `
    @prefix : <http://example.org/> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

    # Rule 1: Calculate age from birthDate
    { ?person :birthDate ?birth }
      => { ?person :age ?age } .  # Would need SPARQL for real calculation

    # Rule 2: Categorize by age
    { ?person :age ?age . ?age > 65 }
      => { ?person :category :Senior } .

    { ?person :age ?age . ?age >= 18 . ?age <= 65 }
      => { ?person :category :Adult } .

    { ?person :age ?age . ?age < 18 }
      => { ?person :category :Minor } .

    # Rule 3: Seniors get special benefits
    { ?person :category :Senior }
      => { ?person :eligibleFor :SeniorDiscount } .

    # Rule 4: Mark complete profiles
    { ?person :name ?n ; :email ?e ; :category ?c }
      => { ?person :profileComplete true } .
  `,
  askQuery: 'ASK { ?p :profileComplete true }'
};

console.log('Enrichment rules:');
console.log('  Birthdate → derive age');
console.log('  Age → categorize as Senior/Adult/Minor');
console.log('  Senior → eligible for discount');
console.log('  Complete profile (name + email + category) → mark as complete');
console.log('');
console.log('Input: Person with name, email, age');
console.log('Output: Category, discounts, completeness flag\n');

// Example 5: Compliance rule engine
console.log('5. Compliance Rule Engine\n');

const complianceCondition = {
  kind: 'n3',
  rules: `
    @prefix : <http://example.org/> .

    # GDPR Rules
    { ?entity a :PersonalData ; :storedIn ?jurisdiction .
      ?jurisdiction :region ?region . ?region != :EU }
      => { ?entity :gdprViolation :DataResidency } .

    # PCI-DSS Rules
    { ?entity a :CardData ; :encryptionLevel ?level .
      ?level < 256 }
      => { ?entity :pciViolation :InsufficientEncryption } .

    # SOX Rules (for public companies)
    { ?company a :PublicCompany ;
               :hasFinancialData ?data }
      => { ?company :requiresSoxCompliance true } .

    # Any violation → requires remediation
    { ?entity :gdprViolation ?v } => { ?entity :requiresRemediation true } .
    { ?entity :pciViolation ?v } => { ?entity :requiresRemediation true } .

    # Mark non-compliant if has violations
    { ?entity :requiresRemediation true }
      => { ?entity :complianceStatus :NonCompliant } .
  `,
  askQuery: 'ASK { ?entity :complianceStatus :NonCompliant }'
};

console.log('Compliance engine checks:');
console.log('  GDPR: Personal data must be in EU jurisdiction');
console.log('  PCI-DSS: Card data must be 256-bit encrypted');
console.log('  SOX: Public companies must audit financial data');
console.log('  Any violation → mark non-compliant\n');

// Performance considerations
console.log('6. Performance Considerations\n');

console.log('Forward-chaining semantics:');
console.log('  - All applicable rules fire until fixpoint');
console.log('  - Derives all possible new facts');
console.log('  - Complete closure of logical consequences');
console.log('');
console.log('Performance factors:');
console.log('  1. Rule count: More rules = more inference steps');
console.log('  2. Data size: Large graphs = more match attempts');
console.log('  3. Rule complexity: Join patterns in antecedents');
console.log('  4. Iteration count: Fixpoint may require many passes');
console.log('');
console.log('Optimization tips:');
console.log('  ✓ Materialize frequently derived facts');
console.log('  ✓ Use specific patterns (avoid ?x ?p ?o)');
console.log('  ✓ Order rules: simple → complex');
console.log('  ✗ Avoid rules that derive rule premises');
console.log('  ✗ Don\'t use reasoning for high-frequency checks\n');

// Full hook example
console.log('7. Complete Hook with N3 Reasoning\n');

console.log(`
const dataEnrichmentHook = {
  name: 'enriched-trade-data',

  condition: {
    kind: 'n3',
    rules: \`
      @prefix : <http://example.org/> .

      { ?trade a :Trade ; :amount ?amt . ?amt > 500000 }
        => { ?trade :largeTradeIndicator true } .

      { ?trade :largeTradeIndicator true }
        => { ?trade :requiresReview true } .

      { ?trade :requiresReview true ; :counterparty ?cp }
        => { ?cp :hadLargeTrade true } .
    \`,
    askQuery: 'ASK { ?t :requiresReview true }'
  },

  effects: [{
    kind: 'sparql-construct',
    query: \`
      CONSTRUCT {
        ?trade :reviewStatus ex:Pending ;
               :derivedFrom ex:N3Rules ;
               :timestamp ?now .
      }
      WHERE {
        ?trade :requiresReview true .
        BIND (NOW() as ?now)
      }
    \`
  }]
};

Execution:
1. Trade added with amount > $500K
2. N3 reasoning: derives :requiresReview true
3. Condition ASK: "is there a :requiresReview?" → true
4. Effect executes: marks trade as pending review
5. Counterparty flagged for trading pattern analysis
`);

console.log('\n=== Example Complete ===');
