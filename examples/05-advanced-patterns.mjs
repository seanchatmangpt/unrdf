/**
 * Example 05: Advanced Patterns
 *
 * This example demonstrates:
 * - Graph traversal algorithms
 * - Inferencing and reasoning
 * - Data transformation pipelines
 * - Performance optimization
 * - Real-world use cases
 *
 * Run: node examples/05-advanced-patterns.mjs
 */

import {
  createStore,
  namedNode,
  literal,
  executeSelectSync,
  FOAF,
  RDF,
  RDFS
} from '@unrdf/core';

console.log('=== Example 05: Advanced Patterns ===\n');

const ex = (name) => namedNode(`http://example.org/${name}`);
const store = createStore();

// ============================================================================
// Setup: Create a realistic organizational knowledge graph
// ============================================================================
console.log('--- Setting up Organizational Knowledge Graph ---');

const org = {
  people: [
    { id: 'alice', name: 'Alice Smith', role: 'Engineering Manager', dept: 'Engineering', skills: ['Leadership', 'Backend'] },
    { id: 'bob', name: 'Bob Johnson', role: 'Senior Developer', dept: 'Engineering', skills: ['Backend', 'Databases'] },
    { id: 'carol', name: 'Carol White', role: 'DevOps Engineer', dept: 'Engineering', skills: ['DevOps', 'Cloud'] },
    { id: 'dave', name: 'Dave Brown', role: 'Junior Developer', dept: 'Engineering', skills: ['Frontend'] },
    { id: 'eve', name: 'Eve Davis', role: 'Product Manager', dept: 'Product', skills: ['Product', 'Strategy'] },
    { id: 'frank', name: 'Frank Miller', role: 'Designer', dept: 'Design', skills: ['UI/UX', 'Design'] }
  ],
  relationships: [
    ['alice', 'manages', 'bob'],
    ['alice', 'manages', 'carol'],
    ['alice', 'manages', 'dave'],
    ['bob', 'mentors', 'dave'],
    ['alice', 'knows', 'eve'],
    ['eve', 'knows', 'frank']
  ],
  projects: [
    { id: 'proj1', name: 'Platform Rewrite', members: ['alice', 'bob', 'carol'] },
    { id: 'proj2', name: 'Mobile App', members: ['dave', 'frank'] },
    { id: 'proj3', name: 'Data Pipeline', members: ['bob', 'carol'] }
  ]
};

// Add people
for (const person of org.people) {
  store.addQuad(ex(person.id), RDF.type, FOAF.Person);
  store.addQuad(ex(person.id), FOAF.name, literal(person.name));
  store.addQuad(ex(person.id), ex('role'), literal(person.role));
  store.addQuad(ex(person.id), ex('department'), literal(person.dept));

  for (const skill of person.skills) {
    store.addQuad(ex(person.id), ex('hasSkill'), literal(skill));
  }
}

// Add relationships
for (const [from, relation, to] of org.relationships) {
  store.addQuad(ex(from), ex(relation), ex(to));
}

// Add projects
for (const project of org.projects) {
  store.addQuad(ex(project.id), RDF.type, ex('Project'));
  store.addQuad(ex(project.id), RDFS.label, literal(project.name));

  for (const member of project.members) {
    store.addQuad(ex(project.id), ex('hasMember'), ex(member));
    store.addQuad(ex(member), ex('worksOn'), ex(project.id));
  }
}

console.log(`  ✅ Created knowledge graph with ${store.size} triples\n`);

// ============================================================================
// Pattern 1: Transitive Closure (Find all reports)
// ============================================================================
console.log('--- Pattern 1: Organizational Hierarchy (Transitive) ---');

function findAllReports(managerId, store) {
  const reports = new Set();
  const queue = [managerId];

  while (queue.length > 0) {
    const current = queue.shift();
    const directReports = store.getQuads(current, ex('manages'), null);

    for (const quad of directReports) {
      const report = quad.object;
      if (!reports.has(report.value)) {
        reports.add(report.value);
        queue.push(report);
      }
    }
  }

  return Array.from(reports);
}

const aliceReports = findAllReports(ex('alice'), store);
console.log(`  Alice's organization (${aliceReports.length} people):`);

for (const reportId of aliceReports) {
  const person = namedNode(reportId);
  const nameQuad = store.getQuads(person, FOAF.name, null)[0];
  const roleQuad = store.getQuads(person, ex('role'), null)[0];

  if (nameQuad && roleQuad) {
    const directReport = store.getQuads(ex('alice'), ex('manages'), person).length > 0;
    const prefix = directReport ? '  →' : '    →';
    console.log(`${prefix} ${nameQuad.object.value} (${roleQuad.object.value})`);
  }
}

// ============================================================================
// Pattern 2: Skill Matrix & Team Formation
// ============================================================================
console.log('\n--- Pattern 2: Skill-based Team Formation ---');

function findPeopleBySkills(requiredSkills, store) {
  const candidates = new Map();

  const peopleQuads = store.getQuads(null, RDF.type, FOAF.Person);

  for (const quad of peopleQuads) {
    const person = quad.subject;
    const skillQuads = store.getQuads(person, ex('hasSkill'), null);
    const skills = skillQuads.map(q => q.object.value);

    const matchCount = requiredSkills.filter(s => skills.includes(s)).length;
    if (matchCount > 0) {
      candidates.set(person.value, {
        skills,
        matchCount,
        coverage: matchCount / requiredSkills.length
      });
    }
  }

  return candidates;
}

const projectSkills = ['Backend', 'DevOps', 'Cloud'];
console.log(`  Looking for team with skills: ${projectSkills.join(', ')}`);

const candidates = findPeopleBySkills(projectSkills, store);
const sorted = Array.from(candidates.entries())
  .sort((a, b) => b[1].matchCount - a[1].matchCount);

console.log(`\n  Top candidates:`);
for (const [personId, data] of sorted.slice(0, 3)) {
  const person = namedNode(personId);
  const nameQuad = store.getQuads(person, FOAF.name, null)[0];
  const matchedSkills = projectSkills.filter(s => data.skills.includes(s));

  console.log(`    ${nameQuad.object.value}`);
  console.log(`      Matched skills: ${matchedSkills.join(', ')}`);
  console.log(`      Coverage: ${(data.coverage * 100).toFixed(0)}%`);
}

// ============================================================================
// Pattern 3: Collaborative Filtering (Recommendations)
// ============================================================================
console.log('\n--- Pattern 3: Project Recommendations ---');

function recommendProjects(personId, store) {
  const person = ex(personId);

  // Find people working on same projects
  const myProjects = store.getQuads(person, ex('worksOn'), null);
  const colleagues = new Set();

  for (const projQuad of myProjects) {
    const project = projQuad.object;
    const members = store.getQuads(project, ex('hasMember'), null);

    for (const memberQuad of members) {
      if (memberQuad.object.value !== person.value) {
        colleagues.add(memberQuad.object.value);
      }
    }
  }

  // Find projects my colleagues work on
  const recommendations = new Map();

  for (const colleagueId of colleagues) {
    const colleague = namedNode(colleagueId);
    const theirProjects = store.getQuads(colleague, ex('worksOn'), null);

    for (const projQuad of theirProjects) {
      const project = projQuad.object;

      // Skip if I'm already on this project
      const alreadyMember = store.getQuads(project, ex('hasMember'), person).length > 0;
      if (alreadyMember) continue;

      const count = recommendations.get(project.value) || 0;
      recommendations.set(project.value, count + 1);
    }
  }

  return Array.from(recommendations.entries())
    .sort((a, b) => b[1] - a[1])
    .map(([projId, score]) => ({
      project: namedNode(projId),
      score
    }));
}

const daveRecommendations = recommendProjects('dave', store);
console.log(`  Recommended projects for Dave:`);

for (const { project, score } of daveRecommendations) {
  const labelQuad = store.getQuads(project, RDFS.label, null)[0];
  const projectName = labelQuad ? labelQuad.object.value : project.value;

  console.log(`    ${projectName} (${score} colleague${score > 1 ? 's' : ''} working on it)`);
}

// ============================================================================
// Pattern 4: Data Quality Metrics
// ============================================================================
console.log('\n--- Pattern 4: Data Quality Metrics ---');

function calculateQualityMetrics(store) {
  const metrics = {
    totalPeople: 0,
    withEmail: 0,
    withSkills: 0,
    withProjects: 0,
    avgSkillsPerPerson: 0,
    avgProjectsPerPerson: 0
  };

  const peopleQuads = store.getQuads(null, RDF.type, FOAF.Person);
  metrics.totalPeople = peopleQuads.length;

  let totalSkills = 0;
  let totalProjects = 0;

  for (const quad of peopleQuads) {
    const person = quad.subject;

    const emails = store.getQuads(person, FOAF.mbox, null);
    if (emails.length > 0) metrics.withEmail++;

    const skills = store.getQuads(person, ex('hasSkill'), null);
    if (skills.length > 0) metrics.withSkills++;
    totalSkills += skills.length;

    const projects = store.getQuads(person, ex('worksOn'), null);
    if (projects.length > 0) metrics.withProjects++;
    totalProjects += projects.length;
  }

  metrics.avgSkillsPerPerson = (totalSkills / metrics.totalPeople).toFixed(1);
  metrics.avgProjectsPerPerson = (totalProjects / metrics.totalPeople).toFixed(1);

  return metrics;
}

const quality = calculateQualityMetrics(store);
console.log(`  Knowledge Graph Quality Report:`);
console.log(`    Total people: ${quality.totalPeople}`);
console.log(`    With email: ${quality.withEmail} (${(quality.withEmail / quality.totalPeople * 100).toFixed(0)}%)`);
console.log(`    With skills: ${quality.withSkills} (${(quality.withSkills / quality.totalPeople * 100).toFixed(0)}%)`);
console.log(`    With projects: ${quality.withProjects} (${(quality.withProjects / quality.totalPeople * 100).toFixed(0)}%)`);
console.log(`    Avg skills/person: ${quality.avgSkillsPerPerson}`);
console.log(`    Avg projects/person: ${quality.avgProjectsPerPerson}`);

// ============================================================================
// Pattern 5: SPARQL-based Analytics
// ============================================================================
console.log('\n--- Pattern 5: Cross-Departmental Collaboration ---');

const collaboration = executeSelectSync(store, `
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?project ?dept1 ?dept2 (COUNT(?dept2) as ?count) WHERE {
    ?project a ex:Project ;
             ex:hasMember ?member1 ;
             ex:hasMember ?member2 .

    ?member1 ex:department ?dept1 .
    ?member2 ex:department ?dept2 .

    FILTER(?dept1 < ?dept2)
  }
  GROUP BY ?project ?dept1 ?dept2
  HAVING (COUNT(?dept2) > 0)
`);

console.log(`  Cross-departmental projects:`);
for (const row of collaboration) {
  const projLabel = store.getQuads(row.get('project'), RDFS.label, null)[0];
  const projectName = projLabel ? projLabel.object.value : row.get('project').value;

  console.log(`    ${projectName}: ${row.get('dept1').value} + ${row.get('dept2').value}`);
}

// ============================================================================
// Summary
// ============================================================================
console.log('\n--- Summary ---');
console.log('  Advanced patterns demonstrated:');
console.log('  ✓ Transitive closure (org hierarchy)');
console.log('  ✓ Skill-based matching');
console.log('  ✓ Collaborative filtering');
console.log('  ✓ Data quality metrics');
console.log('  ✓ Cross-entity analytics');
console.log('  ✓ Real-world organizational modeling');

console.log('\n--- Final Statistics ---');
console.log(`  Total triples: ${store.size}`);
console.log(`  Total people: ${org.people.length}`);
console.log(`  Total projects: ${org.projects.length}`);
console.log(`  Total relationships: ${org.relationships.length}`);

console.log('\n✅ Example complete!');
console.log('\nYou now have the tools to build production RDF applications!');
console.log('Check out the documentation for more advanced features.');
