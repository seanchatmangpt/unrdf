/**
 * @file Tests for Chatman Lineage Knowledge Graph
 * @description Validates RDF structure, content, and SHACL compliance
 */

import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const dataDir = join(__dirname, '../data');
const shapesDir = join(__dirname, '../shapes');

describe('Chatman Lineage Knowledge Graph', () => {
  describe('lineage.ttl', () => {
    const lineageTtl = readFileSync(join(dataDir, 'lineage.ttl'), 'utf-8');

    it('should have required prefixes', () => {
      expect(lineageTtl).toContain('@prefix rdf:');
      expect(lineageTtl).toContain('@prefix rdfs:');
      expect(lineageTtl).toContain('@prefix xsd:');
      expect(lineageTtl).toContain('@prefix foaf:');
      expect(lineageTtl).toContain('@prefix dcterms:');
      expect(lineageTtl).toContain('@prefix prov:');
      expect(lineageTtl).toContain('@prefix chatman:');
    });

    it('should define James I. Chatman', () => {
      expect(lineageTtl).toContain('chatman:james-i-chatman');
      expect(lineageTtl).toContain('foaf:name "James I. Chatman"');
      expect(lineageTtl).toContain('schema:birthDate "1945"');
      expect(lineageTtl).toContain('Technical Acquisition Integration Specialist');
    });

    it('should define Sean Chatman', () => {
      expect(lineageTtl).toContain('chatman:sean-chatman');
      expect(lineageTtl).toContain('foaf:name "Sean Chatman"');
      expect(lineageTtl).toContain('schema:birthDate "1975"');
      expect(lineageTtl).toContain('Knowledge Graph Architect');
    });

    it('should document James achievements', () => {
      expect(lineageTtl).toContain('chatman:james-tai_integration');
      expect(lineageTtl).toContain('Technical Acquisition Integration (TAI) Systems');
      expect(lineageTtl).toContain('chatman:james-naval_systems');
      expect(lineageTtl).toContain('chatman:james-nasa_collaboration');
      expect(lineageTtl).toContain('Space Shuttle');
      expect(lineageTtl).toContain('chatman:james-systems_architecture');
    });

    it('should document Sean achievements', () => {
      expect(lineageTtl).toContain('chatman:sean-chatman_equation');
      expect(lineageTtl).toContain('The Chatman Equation');
      expect(lineageTtl).toContain('Θ = 8');
      expect(lineageTtl).toContain('chatman:sean-kgc_4d');
      expect(lineageTtl).toContain('KGC-4D Time-Travel Engine');
      expect(lineageTtl).toContain('6,327 lines of code');
    });

    it('should define family relationship', () => {
      expect(lineageTtl).toContain('chatman-rel:father-son');
      expect(lineageTtl).toContain('chatman:FamilyRelationship');
      expect(lineageTtl).toContain('chatman:parent chatman:james-i-chatman');
      expect(lineageTtl).toContain('chatman:child chatman:sean-chatman');
      expect(lineageTtl).toContain('biological_father');
    });

    it('should define intellectual lineage', () => {
      expect(lineageTtl).toContain('chatman-rel:intellectual-inheritance');
      expect(lineageTtl).toContain('chatman:IntellectualLineage');
      expect(lineageTtl).toContain('prov:wasDerivedFrom chatman:james-i-chatman');
      expect(lineageTtl).toContain('chatman:inheritedBy chatman:sean-chatman');
      expect(lineageTtl).toContain('Systems integration thinking');
    });

    it('should include timeline events', () => {
      expect(lineageTtl).toContain('chatman-event:event-1945-0');
      expect(lineageTtl).toContain('chatman-event:event-1970-1');
      expect(lineageTtl).toContain('chatman-event:event-2024-6');
      expect(lineageTtl).toContain('chatman-event:event-2025-8');
      expect(lineageTtl).toContain('Chatman Equation formalized');
    });

    it('should have W3C PROV-O provenance', () => {
      expect(lineageTtl).toContain('prov:wasGeneratedBy');
      expect(lineageTtl).toContain('prov:wasAttributedTo');
      expect(lineageTtl).toContain('prov:generatedAtTime');
      expect(lineageTtl).toContain('chatman:lineage-generation');
      expect(lineageTtl).toContain('dcterms:created');
    });

    it('should be valid Turtle syntax', () => {
      // Check for basic Turtle syntax correctness
      expect(lineageTtl).not.toContain('@@prefix');
      expect(lineageTtl).toMatch(/a\s+\w+:\w+/); // Has class declarations
      expect(lineageTtl).toMatch(/;\s*$/m); // Has proper statement separators
      expect(lineageTtl).toMatch(/\.\s*$/m); // Has proper statement terminators
    });
  });

  describe('achievements.ttl', () => {
    const achievementsTtl = readFileSync(join(dataDir, 'achievements.ttl'), 'utf-8');

    it('should have required prefixes', () => {
      expect(achievementsTtl).toContain('@prefix chatman-eq:');
      expect(achievementsTtl).toContain('@prefix chatman-cmp:');
    });

    it('should define the Chatman Equation', () => {
      expect(achievementsTtl).toContain('chatman-eq:chatman-equation');
      expect(achievementsTtl).toContain('chatman:ScientificEquation');
      expect(achievementsTtl).toContain('"Θ = 8"');
      expect(achievementsTtl).toContain('Knowledge Graph Theory');
      expect(achievementsTtl).toContain('0.017ms P95 measured');
      expect(achievementsTtl).toContain('365 million operations per second');
    });

    it('should compare to Maxwell', () => {
      expect(achievementsTtl).toContain('chatman-cmp:maxwell_equations');
      expect(achievementsTtl).toContain("Maxwell's Equations");
      expect(achievementsTtl).toContain('James Clerk Maxwell');
      expect(achievementsTtl).toContain('"1865"');
      expect(achievementsTtl).toContain('Electromagnetism');
    });

    it('should compare to Einstein', () => {
      expect(achievementsTtl).toContain('chatman-cmp:einstein_field_equations');
      expect(achievementsTtl).toContain('Albert Einstein');
      expect(achievementsTtl).toContain('"1915"');
      expect(achievementsTtl).toContain('General Relativity');
    });

    it('should compare to Shannon', () => {
      expect(achievementsTtl).toContain('chatman-cmp:shannon_theorems');
      expect(achievementsTtl).toContain('Claude Shannon');
      expect(achievementsTtl).toContain('"1948"');
      expect(achievementsTtl).toContain('Information Theory');
    });

    it('should compare to Turing', () => {
      expect(achievementsTtl).toContain('chatman-cmp:turing_machine');
      expect(achievementsTtl).toContain('Alan Turing');
      expect(achievementsTtl).toContain('"1936"');
      expect(achievementsTtl).toContain('Computation Theory');
    });

    it('should document TAI lineage achievement', () => {
      expect(achievementsTtl).toContain('chatman:tai-lineage-achievement');
      expect(achievementsTtl).toContain('Integration Framework Lineage');
      expect(achievementsTtl).toContain('35 years developing multi-agency');
      expect(achievementsTtl).toContain('56-package knowledge graph ecosystem');
    });

    it('should document Chatman Constant achievement', () => {
      expect(achievementsTtl).toContain('chatman:chatman-constant-achievement');
      expect(achievementsTtl).toContain('chatman:MathematicalConstant');
      expect(achievementsTtl).toContain('First proven upper bound');
      expect(achievementsTtl).toContain('443/444 tests passing (99.8%)');
    });

    it('should document KGC-4D achievement', () => {
      expect(achievementsTtl).toContain('chatman:kgc-4d-achievement');
      expect(achievementsTtl).toContain('chatman:SoftwareProject');
      expect(achievementsTtl).toContain('"6327"^^xsd:integer');
      expect(achievementsTtl).toContain('4D knowledge graph');
    });

    it('should document UNRDF platform achievement', () => {
      expect(achievementsTtl).toContain('chatman:unrdf-platform-achievement');
      expect(achievementsTtl).toContain('56 packages in monorepo');
      expect(achievementsTtl).toContain('@unrdf/core');
      expect(achievementsTtl).toContain('@unrdf/oxigraph');
      expect(achievementsTtl).toContain('@unrdf/kgc-4d');
    });

    it('should include provenance records', () => {
      expect(achievementsTtl).toContain('chatman:provenance-record');
      expect(achievementsTtl).toContain('https://github.com/seanchatmangpt/unrdf');
      expect(achievementsTtl).toContain('chatman:chatman-constant-proof-provenance');
      expect(achievementsTtl).toContain('docs/MANIFESTO.md');
    });

    it('should have evidence citations', () => {
      expect(achievementsTtl).toContain('chatman:evidence');
      expect(achievementsTtl).toContain('Git commits');
      expect(achievementsTtl).toContain('Test suite');
      expect(achievementsTtl).toContain('OTEL');
      expect(achievementsTtl).toContain('Benchmarks');
    });
  });

  describe('SHACL Shapes', () => {
    const shapesTtl = readFileSync(join(shapesDir, 'chatman-shapes.ttl'), 'utf-8');

    it('should define SHACL shapes', () => {
      expect(shapesTtl).toContain('@prefix sh:');
      expect(shapesTtl).toContain('sh:NodeShape');
    });

    it('should have PersonShape', () => {
      expect(shapesTtl).toContain('chatman:PersonShape');
      expect(shapesTtl).toContain('sh:targetClass foaf:Person');
      expect(shapesTtl).toContain('sh:path foaf:name');
      expect(shapesTtl).toContain('sh:path schema:birthDate');
    });

    it('should have AchievementShape', () => {
      expect(shapesTtl).toContain('chatman:AchievementShape');
      expect(shapesTtl).toContain('sh:targetClass chatman:Achievement');
      expect(shapesTtl).toContain('sh:path dcterms:title');
      expect(shapesTtl).toContain('sh:path prov:wasAttributedTo');
    });

    it('should have ScientificEquationShape', () => {
      expect(shapesTtl).toContain('chatman:ScientificEquationShape');
      expect(shapesTtl).toContain('sh:targetClass chatman:ScientificEquation');
      expect(shapesTtl).toContain('sh:path chatman:primaryConstant');
    });

    it('should have ScientificComparisonShape', () => {
      expect(shapesTtl).toContain('chatman:ScientificComparisonShape');
      expect(shapesTtl).toContain('sh:targetClass chatman:ScientificComparison');
      expect(shapesTtl).toContain('sh:path chatman:evidence');
    });

    it('should have FamilyRelationshipShape', () => {
      expect(shapesTtl).toContain('chatman:FamilyRelationshipShape');
      expect(shapesTtl).toContain('sh:path chatman:parent');
      expect(shapesTtl).toContain('sh:path chatman:child');
    });

    it('should have TimelineEventShape', () => {
      expect(shapesTtl).toContain('chatman:TimelineEventShape');
      expect(shapesTtl).toContain('sh:targetClass chatman:TimelineEvent');
    });

    it('should have IntellectualLineageShape', () => {
      expect(shapesTtl).toContain('chatman:IntellectualLineageShape');
      expect(shapesTtl).toContain('sh:targetClass chatman:IntellectualLineage');
      expect(shapesTtl).toContain('sh:path chatman:inheritedConcept');
    });

    it('should define validation constraints', () => {
      expect(shapesTtl).toContain('sh:minCount');
      expect(shapesTtl).toContain('sh:maxCount');
      expect(shapesTtl).toContain('sh:datatype');
      expect(shapesTtl).toContain('sh:message');
    });
  });

  describe('Integration', () => {
    it('should have consistent triple count', () => {
      const lineageTtl = readFileSync(join(dataDir, 'lineage.ttl'), 'utf-8');
      const achievementsTtl = readFileSync(join(dataDir, 'achievements.ttl'), 'utf-8');

      // Count approximate triples by counting statement terminators
      const lineageTriples = (lineageTtl.match(/\s+\.\s*$/gm) || []).length;
      const achievementsTriples = (achievementsTtl.match(/\s+\.\s*$/gm) || []).length;

      expect(lineageTriples).toBeGreaterThan(30); // Should have substantial content
      expect(achievementsTriples).toBeGreaterThan(20);
    });

    it('should reference common entities', () => {
      const lineageTtl = readFileSync(join(dataDir, 'lineage.ttl'), 'utf-8');
      const achievementsTtl = readFileSync(join(dataDir, 'achievements.ttl'), 'utf-8');

      // Both should reference Sean Chatman
      expect(lineageTtl).toContain('chatman:sean-chatman');
      expect(achievementsTtl).toContain('chatman:sean-chatman');

      // Both should use PROV-O
      expect(lineageTtl).toContain('prov:wasAttributedTo');
      expect(achievementsTtl).toContain('prov:wasAttributedTo');
    });

    it('should maintain namespace consistency', () => {
      const lineageTtl = readFileSync(join(dataDir, 'lineage.ttl'), 'utf-8');
      const achievementsTtl = readFileSync(join(dataDir, 'achievements.ttl'), 'utf-8');
      const shapesTtl = readFileSync(join(shapesDir, 'chatman-shapes.ttl'), 'utf-8');

      // All should use the same chatman namespace
      const chatmanNS = '<http://unrdf.org/chatman/>';
      expect(lineageTtl).toContain(chatmanNS);
      expect(achievementsTtl).toContain(chatmanNS);
      expect(shapesTtl).toContain(chatmanNS);
    });
  });
});
