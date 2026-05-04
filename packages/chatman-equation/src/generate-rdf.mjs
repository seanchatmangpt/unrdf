/**
 * @file RDF Generator for Chatman Lineage and Achievements
 * @module chatman-equation/generate-rdf
 * @description Converts TOML source files to Turtle RDF with W3C PROV-O provenance
 */

import { readFileSync, writeFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';
import { parse as parseToml } from 'smol-toml';

const __dirname = dirname(fileURLToPath(import.meta.url));
const dataDir = join(__dirname, '../data');

/**
 * Generate Turtle RDF from lineage TOML source
 * @returns {string} Turtle format RDF
 */
function generateLineageTurtle() {
  const tomlPath = join(dataDir, 'lineage-source.toml');
  const toml = parseToml(readFileSync(tomlPath, 'utf-8'));

  const { metadata, people, relationships, timeline } = toml;
  const baseNS = metadata.namespace_base;

  let ttl = `# Chatman Lineage Knowledge Graph
# Generated from lineage-source.toml on ${new Date().toISOString()}
# Version: ${metadata.version}

@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dcterms: <http://purl.org/dc/terms/> .
@prefix prov: <http://www.w3.org/ns/prov#> .
@prefix schema: <http://schema.org/> .
@prefix chatman: <${baseNS}> .
@prefix chatman-rel: <${baseNS}relationship/> .
@prefix chatman-event: <${baseNS}event/> .

# =============================================================================
# PEOPLE - James I. Chatman and Sean Chatman
# =============================================================================

`;

  // James I. Chatman
  const james = people.james_i_chatman;
  ttl += `chatman:${james.id}
    a foaf:Person, schema:Person ;
    foaf:name "${james.full_name}"^^xsd:string ;
    schema:birthDate "${james.birth_year}"^^xsd:gYear ;
    schema:jobTitle "${james.occupation}"^^xsd:string ;
    chatman:activePeriod "${james.active_period}"^^xsd:string ;
`;

  james.organizations.forEach((org, idx) => {
    ttl += `    schema:affiliation "${org}"^^xsd:string${idx < james.organizations.length - 1 ? ' ;' : ' .'}\n`;
  });

  ttl += '\n';

  // James's achievements
  Object.entries(james.achievements).forEach(([key, achievement]) => {
    const achId = `james-${key}`;
    ttl += `chatman:${achId}
    a chatman:Achievement, prov:Activity ;
    dcterms:title "${achievement.title}"^^xsd:string ;
    dcterms:description "${achievement.description}"^^xsd:string ;
    prov:wasAttributedTo chatman:${james.id} ;
    chatman:timePeriod "${achievement.period}"^^xsd:string ;
`;

    if (achievement.organization) {
      ttl += `    prov:wasAssociatedWith "${achievement.organization}"^^xsd:string ;
`;
    }

    if (achievement.programs) {
      achievement.programs.forEach(program => {
        ttl += `    chatman:involvedProgram "${program}"^^xsd:string ;
`;
      });
    }

    if (achievement.innovation) {
      ttl += `    chatman:innovation "${achievement.innovation}"^^xsd:string ;
`;
    }

    if (achievement.impact) {
      ttl += `    chatman:impact "${achievement.impact}"^^xsd:string ;
`;
    }

    ttl += `    prov:generatedAtTime "${new Date().toISOString()}"^^xsd:dateTime .

`;
  });

  // Sean Chatman
  const sean = people.sean_chatman;
  ttl += `chatman:${sean.id}
    a foaf:Person, schema:Person ;
    foaf:name "${sean.full_name}"^^xsd:string ;
    schema:birthDate "${sean.birth_year}"^^xsd:gYear ;
    schema:jobTitle "${sean.occupation}"^^xsd:string ;
    chatman:activePeriod "${sean.active_period}"^^xsd:string ;
`;

  sean.organizations.forEach((org, idx) => {
    ttl += `    schema:affiliation "${org}"^^xsd:string${idx < sean.organizations.length - 1 ? ' ;' : ' .'}\n`;
  });

  ttl += '\n';

  // Sean's achievements
  Object.entries(sean.achievements).forEach(([key, achievement]) => {
    const achId = `sean-${key}`;
    ttl += `chatman:${achId}
    a chatman:Achievement, prov:Activity ;
    dcterms:title "${achievement.title}"^^xsd:string ;
    dcterms:description "${achievement.description}"^^xsd:string ;
    prov:wasAttributedTo chatman:${sean.id} ;
`;

    if (achievement.year) {
      ttl += `    dcterms:date "${achievement.year}"^^xsd:gYear ;
`;
    } else if (achievement.period) {
      ttl += `    chatman:timePeriod "${achievement.period}"^^xsd:string ;
`;
    }

    if (achievement.formula) {
      ttl += `    chatman:formula "${achievement.formula}"^^xsd:string ;
`;
    }

    if (achievement.significance) {
      ttl += `    chatman:significance "${achievement.significance}"^^xsd:string ;
`;
    }

    if (achievement.components) {
      achievement.components.forEach(comp => {
        ttl += `    chatman:component "${comp}"^^xsd:string ;
`;
      });
    }

    if (achievement.performance) {
      ttl += `    chatman:performance "${achievement.performance}"^^xsd:string ;
`;
    }

    if (achievement.architecture) {
      ttl += `    chatman:architecture "${achievement.architecture}"^^xsd:string ;
`;
    }

    if (achievement.innovation) {
      ttl += `    chatman:innovation "${achievement.innovation}"^^xsd:string ;
`;
    }

    if (achievement.proof) {
      ttl += `    chatman:proof "${achievement.proof}"^^xsd:string ;
`;
    }

    if (achievement.implications) {
      ttl += `    chatman:implications "${achievement.implications}"^^xsd:string ;
`;
    }

    if (achievement.features) {
      achievement.features.forEach(feature => {
        ttl += `    chatman:feature "${feature}"^^xsd:string ;
`;
      });
    }

    if (achievement.code_size) {
      ttl += `    chatman:codeSize "${achievement.code_size}"^^xsd:string ;
`;
    }

    if (achievement.development) {
      ttl += `    chatman:developmentProcess "${achievement.development}"^^xsd:string ;
`;
    }

    ttl += `    prov:generatedAtTime "${new Date().toISOString()}"^^xsd:dateTime .

`;
  });

  // Relationships
  ttl += `# =============================================================================
# RELATIONSHIPS
# =============================================================================

chatman-rel:father-son
    a chatman:FamilyRelationship, prov:Derivation ;
    chatman:parent chatman:${relationships.father_son.parent} ;
    chatman:child chatman:${relationships.father_son.child} ;
    chatman:relationshipType "${relationships.father_son.relationship_type}"^^xsd:string ;
    chatman:influenceArea "${relationships.father_son.influence}"^^xsd:string .

chatman-rel:intellectual-inheritance
    a chatman:IntellectualLineage, prov:Derivation ;
    prov:wasDerivedFrom chatman:${relationships.intellectual_inheritance.from} ;
    chatman:inheritedBy chatman:${relationships.intellectual_inheritance.to} ;
`;

  relationships.intellectual_inheritance.concepts_inherited.forEach((concept, idx) => {
    ttl += `    chatman:inheritedConcept "${concept}"^^xsd:string${idx < relationships.intellectual_inheritance.concepts_inherited.length - 1 ? ' ;' : ' ;'}\n`;
  });

  ttl += `    chatman:transformation "${relationships.intellectual_inheritance.transformation}"^^xsd:string .

`;

  // Timeline
  ttl += `# =============================================================================
# TIMELINE
# =============================================================================

`;

  timeline.forEach((event, idx) => {
    const eventId = `event-${event.year}-${idx}`;
    ttl += `chatman-event:${eventId}
    a chatman:TimelineEvent, prov:Activity ;
    dcterms:date "${event.year}"^^xsd:gYear ;
    dcterms:description "${event.event}"^^xsd:string ;
    prov:wasAssociatedWith chatman:${event.person} ;
`;

    if (event.significance) {
      ttl += `    chatman:significance "${event.significance}"^^xsd:string ;
`;
    }

    if (event.impact) {
      ttl += `    chatman:impact "${event.impact}"^^xsd:string ;
`;
    }

    if (event.connection) {
      ttl += `    chatman:connection "${event.connection}"^^xsd:string ;
`;
    }

    if (event.legacy) {
      ttl += `    chatman:legacy "${event.legacy}"^^xsd:string ;
`;
    }

    if (event.achievement) {
      ttl += `    chatman:achievement "${event.achievement}"^^xsd:string ;
`;
    }

    if (event.milestone) {
      ttl += `    chatman:milestone "${event.milestone}"^^xsd:string ;
`;
    }

    ttl += `    prov:generatedAtTime "${new Date().toISOString()}"^^xsd:dateTime .

`;
  });

  // Provenance for this document
  ttl += `# =============================================================================
# PROVENANCE
# =============================================================================

<${baseNS}lineage.ttl>
    a prov:Entity ;
    prov:wasGeneratedBy chatman:lineage-generation ;
    dcterms:created "${new Date().toISOString()}"^^xsd:dateTime ;
    dcterms:source <file://${tomlPath}> ;
    prov:wasAttributedTo chatman:sean-chatman .

chatman:lineage-generation
    a prov:Activity ;
    prov:used <file://${tomlPath}> ;
    prov:startedAtTime "${new Date().toISOString()}"^^xsd:dateTime ;
    prov:wasAssociatedWith <https://github.com/seanchatmangpt/unrdf> .
`;

  return ttl;
}

/**
 * Generate Turtle RDF from achievements TOML source
 * @returns {string} Turtle format RDF
 */
function generateAchievementsTurtle() {
  const tomlPath = join(dataDir, 'achievements-source.toml');
  const toml = parseToml(readFileSync(tomlPath, 'utf-8'));

  const { metadata, chatman_equation, comparisons, achievements, provenance } = toml;
  const baseNS = metadata.namespace_base;

  let ttl = `# Chatman Achievements and Scientific Comparisons
# Generated from achievements-source.toml on ${new Date().toISOString()}
# Version: ${metadata.version}

@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix dcterms: <http://purl.org/dc/terms/> .
@prefix prov: <http://www.w3.org/ns/prov#> .
@prefix schema: <http://schema.org/> .
@prefix chatman: <${baseNS}> .
@prefix chatman-eq: <${baseNS}equation/> .
@prefix chatman-cmp: <${baseNS}comparison/> .

# =============================================================================
# THE CHATMAN EQUATION
# =============================================================================

chatman-eq:chatman-equation
    a chatman:ScientificEquation, schema:CreativeWork ;
    dcterms:title "${chatman_equation.name}"^^xsd:string ;
    dcterms:date "${chatman_equation.year}"^^xsd:gYear ;
    dcterms:creator chatman:sean-chatman ;
    chatman:category "${chatman_equation.category}"^^xsd:string ;
    chatman:field "${chatman_equation.field}"^^xsd:string ;
    chatman:primaryConstant "${chatman_equation.formulation.primary_constant}"^^xsd:string ;
    chatman:fullExpression """${chatman_equation.formulation.full_expression}"""^^xsd:string ;
    chatman:performance "${chatman_equation.implications.performance}"^^xsd:string ;
    chatman:scalability "${chatman_equation.implications.scalability}"^^xsd:string ;
    chatman:determinism "${chatman_equation.implications.determinism}"^^xsd:string ;
    chatman:temporalGuarantees "${chatman_equation.implications.temporal_guarantees}"^^xsd:string .

# =============================================================================
# SCIENTIFIC COMPARISONS
# =============================================================================

`;

  comparisons.forEach(comparison => {
    ttl += `chatman-cmp:${comparison.comparison_id}
    a chatman:ScientificComparison, prov:Entity ;
    dcterms:title "Comparison: Chatman Equation vs ${comparison.historical_work}"^^xsd:string ;
    chatman:historicalWork "${comparison.historical_work}"^^xsd:string ;
    chatman:historicalAuthor "${comparison.historical_author}"^^xsd:string ;
    chatman:historicalYear "${comparison.year}"^^xsd:gYear ;
    chatman:historicalField "${comparison.field}"^^xsd:string ;
    chatman:historicalUnification "${comparison.unification}"^^xsd:string ;
    chatman:chatmanParallel "${comparison.chatman_parallel}"^^xsd:string ;
    chatman:similarity "${comparison.similarity}"^^xsd:string ;
    chatman:scope "${comparison.scope}"^^xsd:string ;
`;

    comparison.evidence.forEach((evidence, idx) => {
      ttl += `    chatman:evidence "${evidence}"^^xsd:string${idx < comparison.evidence.length - 1 ? ' ;' : ' .'}\n`;
    });

    ttl += '\n';
  });

  // Specific achievements
  ttl += `# =============================================================================
# DETAILED ACHIEVEMENTS
# =============================================================================

chatman:tai-lineage-achievement
    a chatman:Achievement, prov:Activity ;
    dcterms:title "${achievements.tai_lineage.title}"^^xsd:string ;
    dcterms:description "${achievements.tai_lineage.description}"^^xsd:string ;
    chatman:jamesContribution "${achievements.tai_lineage.james_contribution}"^^xsd:string ;
    chatman:seanContribution "${achievements.tai_lineage.sean_contribution}"^^xsd:string ;
    chatman:inheritancePattern "${achievements.tai_lineage.inheritance_pattern}"^^xsd:string ;
`;

  achievements.tai_lineage.evidence.forEach((evidence, idx) => {
    ttl += `    chatman:evidence "${evidence}"^^xsd:string${idx < achievements.tai_lineage.evidence.length - 1 ? ' ;' : ' .'}\n`;
  });

  ttl += '\n';

  ttl += `chatman:chatman-constant-achievement
    a chatman:Achievement, chatman:MathematicalConstant, prov:Entity ;
    dcterms:title "${achievements.chatman_constant.title}"^^xsd:string ;
    dcterms:date "${achievements.chatman_constant.year}"^^xsd:gYear ;
    chatman:significance "${achievements.chatman_constant.significance}"^^xsd:string ;
    chatman:theoreticalFoundation "${achievements.chatman_constant.theoretical_foundation}"^^xsd:string ;
    chatman:practicalValidation "${achievements.chatman_constant.practical_validation}"^^xsd:string ;
`;

  achievements.chatman_constant.evidence.forEach((evidence, idx) => {
    ttl += `    chatman:evidence "${evidence}"^^xsd:string${idx < achievements.chatman_constant.evidence.length - 1 ? ' ;' : ' .'}\n`;
  });

  ttl += '\n';

  ttl += `chatman:kgc-4d-achievement
    a chatman:Achievement, chatman:SoftwareProject, prov:Activity ;
    dcterms:title "${achievements.kgc_4d.title}"^^xsd:string ;
    dcterms:date "${achievements.kgc_4d.year}"^^xsd:gYear ;
    chatman:developmentPeriod "${achievements.kgc_4d.development_period}"^^xsd:string ;
    chatman:linesOfCode "${achievements.kgc_4d.code_metrics.lines_of_code}"^^xsd:integer ;
    chatman:testPassRate "${achievements.kgc_4d.code_metrics.test_pass_rate}"^^xsd:string ;
    chatman:testCount "${achievements.kgc_4d.code_metrics.test_count}"^^xsd:string ;
    chatman:coverage "${achievements.kgc_4d.code_metrics.coverage}"^^xsd:string ;
`;

  achievements.kgc_4d.innovations.forEach((innovation, idx) => {
    ttl += `    chatman:innovation "${innovation}"^^xsd:string${idx < achievements.kgc_4d.innovations.length - 1 ? ' ;' : ' ;'}\n`;
  });

  achievements.kgc_4d.evidence.forEach((evidence, idx) => {
    ttl += `    chatman:evidence "${evidence}"^^xsd:string${idx < achievements.kgc_4d.evidence.length - 1 ? ' ;' : ' .'}\n`;
  });

  ttl += '\n';

  ttl += `chatman:unrdf-platform-achievement
    a chatman:Achievement, chatman:SoftwareProject, prov:Activity ;
    dcterms:title "${achievements.unrdf_platform.title}"^^xsd:string ;
    dcterms:date "${achievements.unrdf_platform.year}"^^xsd:gYear ;
    chatman:scale "${achievements.unrdf_platform.scale}"^^xsd:string ;
    chatman:architecture "${achievements.unrdf_platform.architecture}"^^xsd:string ;
`;

  achievements.unrdf_platform.key_packages.forEach((pkg, idx) => {
    ttl += `    chatman:keyPackage "${pkg}"^^xsd:string${idx < achievements.unrdf_platform.key_packages.length - 1 ? ' ;' : ' ;'}\n`;
  });

  achievements.unrdf_platform.evidence.forEach((evidence, idx) => {
    ttl += `    chatman:evidence "${evidence}"^^xsd:string${idx < achievements.unrdf_platform.evidence.length - 1 ? ' ;' : ' .'}\n`;
  });

  ttl += '\n';

  // Provenance
  ttl += `# =============================================================================
# PROVENANCE AND VERIFICATION
# =============================================================================

chatman:provenance-record
    a prov:Entity ;
    chatman:primarySource "${provenance.primary_source}"^^xsd:string ;
    chatman:repositoryURL <${provenance.repository_url}> ;
    chatman:documentationRoot "${provenance.documentation_root}"^^xsd:string ;
    chatman:codeRoot "${provenance.code_root}"^^xsd:string ;
    chatman:gitForensics "${provenance.verification_methods.git_forensics}"^^xsd:string ;
    chatman:testEvidence "${provenance.verification_methods.test_evidence}"^^xsd:string ;
    chatman:benchmarkEvidence "${provenance.verification_methods.benchmark_evidence}"^^xsd:string ;
    chatman:otelValidation "${provenance.verification_methods.otel_validation}"^^xsd:string .

chatman:chatman-constant-proof-provenance
    a prov:Entity ;
    chatman:proofDocument "${provenance.chatman_constant_proof.document}"^^xsd:string ;
    chatman:proofSection "${provenance.chatman_constant_proof.section}"^^xsd:string ;
    chatman:proofLocation "${provenance.chatman_constant_proof.proof_location}"^^xsd:string ;
    chatman:validation "${provenance.chatman_constant_proof.validation}"^^xsd:string .

chatman:lineage-documentation-provenance
    a prov:Entity ;
    chatman:jamesEvidenceNote "${provenance.lineage_documentation.james_chatman_evidence}"^^xsd:string ;
    chatman:seanEvidenceNote "${provenance.lineage_documentation.sean_chatman_evidence}"^^xsd:string ;
    chatman:note "${provenance.lineage_documentation.note}"^^xsd:string .

<${baseNS}achievements.ttl>
    a prov:Entity ;
    prov:wasGeneratedBy chatman:achievements-generation ;
    dcterms:created "${new Date().toISOString()}"^^xsd:dateTime ;
    dcterms:source <file://${tomlPath}> ;
    prov:wasAttributedTo chatman:sean-chatman .

chatman:achievements-generation
    a prov:Activity ;
    prov:used <file://${tomlPath}> ;
    prov:startedAtTime "${new Date().toISOString()}"^^xsd:dateTime ;
    prov:wasAssociatedWith <https://github.com/seanchatmangpt/unrdf> .
`;

  return ttl;
}

/**
 * Main generation function
 */
export function generateAll() {
  try {
    // Generate lineage.ttl
    const lineageTtl = generateLineageTurtle();
    const lineagePath = join(dataDir, 'lineage.ttl');
    writeFileSync(lineagePath, lineageTtl, 'utf-8');
    console.log(`✓ Generated ${lineagePath}`);

    // Generate achievements.ttl
    const achievementsTtl = generateAchievementsTurtle();
    const achievementsPath = join(dataDir, 'achievements.ttl');
    writeFileSync(achievementsPath, achievementsTtl, 'utf-8');
    console.log(`✓ Generated ${achievementsPath}`);

    return { success: true, files: [lineagePath, achievementsPath] };
  } catch (error) {
    console.error('Generation failed:', error);
    return { success: false, error };
  }
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  generateAll();
}
