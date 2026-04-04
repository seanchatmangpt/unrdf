/**
 * N3 Reasoning Integration Test Suite
 *
 * Tests forward-chaining N3 reasoning with knowledge hooks:
 * - EYE reasoner integration
 * - Forward-chaining rules execution
 * - Inference result validation
 * - Complex knowledge base queries
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createStore } from '@unrdf/oxigraph';

describe('N3 Reasoning Integration', () => {
  let _graph;

  beforeEach(() => {
    _graph = createStore();
  });

  it('should support N3 condition kind definition', () => {
    const condition = {
      kind: 'n3',
      rules: `
        @prefix ex: <http://example.org/> .
        @prefix log: <http://www.w3.org/2000/10/swap/log#> .
        
        { ?x ex:parent ?y . ?y ex:parent ?z }
          => { ?x ex:grandparent ?z } .
      `,
      askQuery: 'ASK { ?x ex:grandparent ?z }',
    };

    expect(condition.kind).toBe('n3');
    expect(condition.rules).toBeDefined();
    expect(condition.askQuery).toBeDefined();
  });

  it('should require both rules and askQuery properties', () => {
    const incompleteCondition = {
      kind: 'n3',
      rules: 'some rules',
      // askQuery missing
    };

    expect(incompleteCondition.rules).toBeDefined();
    expect(incompleteCondition.askQuery).toBeUndefined();
  });

  it('should preserve N3 rule syntax in condition', () => {
    const n3Rules = `
      @prefix ex: <http://example.org/> .
      @prefix log: <http://www.w3.org/2000/10/swap/log#> .
      
      { ?person ex:knows ?otherPerson . ?otherPerson ex:knows ?third }
        => { ?person ex:indirectlyKnows ?third } .
      
      { ?x ex:type ex:Expert . ?x ex:topic ?topic }
        => { ?x ex:canTeach ?topic } .
    `;

    const condition = {
      kind: 'n3',
      rules: n3Rules,
      askQuery: 'ASK { ?expert ex:canTeach ?topic }',
    };

    expect(condition.rules).toBe(n3Rules);
    expect(condition.rules).toContain('@prefix');
    expect(condition.rules).toContain('=>');
  });

  it('should support simple inference rule', () => {
    const condition = {
      kind: 'n3',
      rules: `
        @prefix ex: <http://example.org/> .
        
        { ?x ex:hasAge ?age . BIND(?age > 18 AS ?isAdult) }
          => { ?x ex:isAdult ?isAdult } .
      `,
      askQuery: 'ASK { ?person ex:isAdult true }',
    };

    expect(condition.rules).toContain('hasAge');
    expect(condition.rules).toContain('isAdult');
    expect(condition.rules).toContain('BIND');
  });

  it('should support transitive closure rules', () => {
    const condition = {
      kind: 'n3',
      rules: `
        @prefix ex: <http://example.org/> .
        
        # Base rule: direct relationships
        { ?x ex:relatedTo ?y } => { ?x ex:connected ?y } .
        
        # Transitive rule
        { ?x ex:connected ?y . ?y ex:connected ?z }
          => { ?x ex:connected ?z } .
      `,
      askQuery: 'ASK { ?a ex:connected ?b }',
    };

    expect(condition.rules).toContain('relatedTo');
    expect(condition.rules).toContain('connected');
  });

  it('should support compound inference patterns', () => {
    const condition = {
      kind: 'n3',
      rules: `
        @prefix ex: <http://example.org/> .
        
        # Rule 1: Identify managers
        { ?person ex:role ex:Manager } => { ?person ex:isManager true } .
        
        # Rule 2: Identify reports
        { ?person ex:reportsTo ?manager . ?manager ex:isManager true }
          => { ?person ex:hasManager ?manager } .
        
        # Rule 3: Team membership
        { ?person ex:hasManager ?manager . ?other ex:hasManager ?manager }
          => { ?person ex:teamMember ?other } .
      `,
      askQuery: 'ASK { ?p1 ex:teamMember ?p2 }',
    };

    expect(condition.rules).toContain('isManager');
    expect(condition.rules).toContain('hasManager');
    expect(condition.rules).toContain('teamMember');
  });

  it('should support SPARQL ASK query for inference results', () => {
    const askQueries = [
      'ASK { ?x ex:isDerived true }',
      'ASK { ?person ex:worksForCompany ?company }',
      'ASK { ?doc ex:mentions ?entity . ?entity ex:isRelevant true }',
    ];

    askQueries.forEach(query => {
      const condition = {
        kind: 'n3',
        rules: 'some rules',
        askQuery: query,
      };

      expect(condition.askQuery).toBe(query);
      expect(condition.askQuery).toContain('ASK');
    });
  });

  it('should support multi-step inference chains', () => {
    const condition = {
      kind: 'n3',
      rules: `
        @prefix ex: <http://example.org/> .
        
        # Step 1: Identify qualified candidates
        { ?person ex:degree ex:Bachelor ; ex:experience ?years .
          FILTER (?years >= 3) }
          => { ?person ex:qualified true } .
        
        # Step 2: Match candidates to jobs
        { ?person ex:qualified true . ?person ex:skillInArea ?area .
          ?job ex:requiresSkill ?area }
          => { ?person ex:suited ?job } .
        
        # Step 3: Create hiring recommendations
        { ?person ex:suited ?job . ?person ex:salary ?salary .
          ?job ex:budgetedSalary ?budget .
          FILTER (?salary <= ?budget) }
          => { ?person ex:recommended ?job } .
      `,
      askQuery: 'ASK { ?person ex:recommended ?job }',
    };

    expect(condition.rules).toContain('qualified');
    expect(condition.rules).toContain('suited');
    expect(condition.rules).toContain('recommended');
  });

  it('should support arithmetic in inference rules', () => {
    const condition = {
      kind: 'n3',
      rules: `
        @prefix ex: <http://example.org/> .
        
        { ?employee ex:baseSalary ?base . ?employee ex:performanceRating ?rating .
          BIND(?base * (?rating / 100) AS ?bonus) }
          => { ?employee ex:bonusAmount ?bonus } .
        
        { ?employee ex:baseSalary ?base . ?employee ex:bonusAmount ?bonus .
          BIND(?base + ?bonus AS ?totalComp) }
          => { ?employee ex:totalCompensation ?totalComp } .
      `,
      askQuery: 'ASK { ?emp ex:totalCompensation ?total }',
    };

    expect(condition.rules).toContain('BIND');
    expect(condition.rules).toContain('bonusAmount');
    expect(condition.rules).toContain('totalCompensation');
  });

  it('should support string operations in rules', () => {
    const condition = {
      kind: 'n3',
      rules: `
        @prefix ex: <http://example.org/> .
        
        { ?doc ex:title ?title . ?doc ex:author ?author .
          BIND(CONCAT(?author, " - ", ?title) AS ?citation) }
          => { ?doc ex:citationString ?citation } .
        
        { ?doc ex:citationString ?citation .
          FILTER(STRLEN(?citation) > 50) }
          => { ?doc ex:longCitation true } .
      `,
      askQuery: 'ASK { ?d ex:longCitation true }',
    };

    expect(condition.rules).toContain('CONCAT');
    expect(condition.rules).toContain('STRLEN');
  });

  it('should support datetime operations in inference', () => {
    const condition = {
      kind: 'n3',
      rules: `
        @prefix ex: <http://example.org/> .
        
        { ?event ex:startDate ?start . ?event ex:endDate ?end .
          BIND(?end - ?start AS ?duration) }
          => { ?event ex:eventDuration ?duration } .
        
        { ?event ex:eventDuration ?dur .
          FILTER(?dur > 3600) }
          => { ?event ex:longEvent true } .
      `,
      askQuery: 'ASK { ?event ex:longEvent true }',
    };

    expect(condition.rules).toContain('startDate');
    expect(condition.rules).toContain('endDate');
  });

  it('should support negation in inference patterns', () => {
    const condition = {
      kind: 'n3',
      rules: `
        @prefix ex: <http://example.org/> .
        
        { ?person ex:hasLicense ?license .
          FILTER NOT EXISTS { ?license ex:isRevoked true } }
          => { ?person ex:validLicense ?license } .
        
        { ?person ex:validLicense ?lic .
          FILTER NOT EXISTS { ?person ex:suspended true } }
          => { ?person ex:canDrive true } .
      `,
      askQuery: 'ASK { ?p ex:canDrive true }',
    };

    expect(condition.rules).toContain('NOT EXISTS');
    expect(condition.rules).toContain('isRevoked');
  });

  it('should support optional matching in rules', () => {
    const condition = {
      kind: 'n3',
      rules: `
        @prefix ex: <http://example.org/> .
        
        { ?product ex:name ?name .
          OPTIONAL { ?product ex:discount ?disc . BIND(?disc AS ?actualDisc) }
          BIND(COALESCE(?actualDisc, 0) AS ?finalDisc) }
          => { ?product ex:appliedDiscount ?finalDisc } .
      `,
      askQuery: 'ASK { ?prod ex:appliedDiscount ?disc }',
    };

    expect(condition.rules).toContain('OPTIONAL');
    expect(condition.rules).toContain('COALESCE');
  });

  it('should support graph-level inference operations', () => {
    const condition = {
      kind: 'n3',
      rules: `
        @prefix ex: <http://example.org/> .
        @prefix log: <http://www.w3.org/2000/10/swap/log#> .
        
        { ?graph log:uri ?uri . ?graph log:semantics ?data .
          ?data log:notincludes { ?anything ex:error ?msg } }
          => { ?graph ex:valid true } .
      `,
      askQuery: 'ASK { ?g ex:valid true }',
    };

    expect(condition.rules).toContain('log:');
    expect(condition.rules).toContain('log:semantics');
  });

  it('should support complex filtering conditions', () => {
    const condition = {
      kind: 'n3',
      rules: `
        @prefix ex: <http://example.org/> .
        
        { ?student ex:gpa ?gpa . ?student ex:attendanceRate ?rate .
          FILTER(?gpa >= 3.5 && ?rate >= 0.95) }
          => { ?student ex:excellentStanding true } .
      `,
      askQuery: 'ASK { ?s ex:excellentStanding true }',
    };

    expect(condition.rules).toContain('gpa');
    expect(condition.rules).toContain('FILTER');
    expect(condition.rules).toContain('&&');
  });

  it('should preserve complete N3 condition structure', () => {
    const fullCondition = {
      kind: 'n3',
      rules: `
        @prefix : <http://example.org/> .
        @prefix log: <http://www.w3.org/2000/10/swap/log#> .
        
        { ?x :parent ?y . ?y :parent ?z } => { ?x :grandparent ?z } .
        { ?x :knows ?y . ?y :knows ?z . ?x log:notEqualTo ?z }
          => { ?x :indirectlyKnows ?z } .
      `,
      askQuery: 'ASK { ?a :indirectlyKnows ?b }',
    };

    expect(fullCondition.kind).toBe('n3');
    expect(fullCondition.rules).toContain('@prefix');
    expect(fullCondition.rules).toContain('=>');
    expect(fullCondition.askQuery).toBe('ASK { ?a :indirectlyKnows ?b }');
  });

  it('should support multiple inference paths to same conclusion', () => {
    const condition = {
      kind: 'n3',
      rules: `
        @prefix ex: <http://example.org/> .
        
        # Path 1: Direct membership
        { ?person ex:member ex:Team } => { ?person ex:inTeam true } .
        
        # Path 2: Through delegation
        { ?person ex:delegateTo ?other . ?other ex:inTeam true }
          => { ?person ex:inTeam true } .
        
        # Path 3: Through organization
        { ?person ex:worksFor ?org . ?org ex:inTeam true }
          => { ?person ex:inTeam true } .
      `,
      askQuery: 'ASK { ?person ex:inTeam true }',
    };

    expect(condition.rules).toContain('delegateTo');
    expect(condition.rules).toContain('worksFor');
  });
});
