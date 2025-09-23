#!/usr/bin/env node

/**
 * Zod Integration Example
 * 
 * This example demonstrates Zod schema integration with RDF:
 * - Creating Zod schemas for RDF data
 * - Validating RDF data against Zod schemas
 * - Converting between RDF and structured data
 * - Type-safe RDF operations
 */

import { useStore, useGraph, useTurtle, useZod } from 'unrdf';
import { z } from 'zod';

console.log('🔒 UNRDF Zod Integration Example\n');

async function main() {
  try {
    // Initialize components
    const store = useStore();
    const graph = useGraph(store);
    const turtle = useTurtle();
    const zodHelper = useZod();

    // Example 1: Basic Zod schema for Person
    console.log('📝 Example 1: Basic Person Schema');
    
    const PersonSchema = z.object({
      id: z.string().url(),
      name: z.string().min(1),
      age: z.number().int().min(0).max(150).optional(),
      email: z.string().email().optional(),
      knows: z.array(z.string().url()).default([])
    });

    // Convert Zod schema to RDF shape
    const personShape = zodHelper.createShape(PersonSchema);
    console.log('✅ Person Zod schema created and converted to RDF shape');

    // Sample person data
    const personData = {
      id: 'http://example.org/john',
      name: 'John Doe',
      age: 30,
      email: 'john@example.com',
      knows: ['http://example.org/jane', 'http://example.org/bob']
    };

    // Validate data against schema
    const validatedPerson = PersonSchema.parse(personData);
    console.log('✅ Person data validated against Zod schema');

    // Convert structured data to RDF
    const personQuads = zodHelper.toRdf(validatedPerson, PersonSchema);
    await graph.addQuads(personQuads);
    console.log(`✅ Converted person data to ${personQuads.length} RDF triples`);

    // Example 2: Complex nested schema
    console.log('\n📝 Example 2: Complex Organization Schema');
    
    const AddressSchema = z.object({
      street: z.string(),
      city: z.string(),
      state: z.string().length(2),
      zipCode: z.string().regex(/^\d{5}(-\d{4})?$/)
    });

    const EmployeeSchema = z.object({
      id: z.string().url(),
      name: z.string(),
      position: z.string(),
      salary: z.number().positive().optional(),
      startDate: z.string().datetime().optional()
    });

    const OrganizationSchema = z.object({
      id: z.string().url(),
      name: z.string(),
      address: AddressSchema,
      employees: z.array(EmployeeSchema),
      founded: z.string().datetime(),
      revenue: z.number().positive().optional()
    });

    // Sample organization data
    const orgData = {
      id: 'http://example.org/techcorp',
      name: 'Tech Corp',
      address: {
        street: '123 Main St',
        city: 'San Francisco',
        state: 'CA',
        zipCode: '94105'
      },
      employees: [
        {
          id: 'http://example.org/ceo',
          name: 'CEO Name',
          position: 'Chief Executive Officer',
          salary: 200000,
          startDate: '2020-01-01T00:00:00Z'
        },
        {
          id: 'http://example.org/cto',
          name: 'CTO Name',
          position: 'Chief Technology Officer',
          salary: 180000,
          startDate: '2020-02-01T00:00:00Z'
        }
      ],
      founded: '2015-01-01T00:00:00Z',
      revenue: 10000000
    };

    // Validate and convert to RDF
    const validatedOrg = OrganizationSchema.parse(orgData);
    const orgQuads = zodHelper.toRdf(validatedOrg, OrganizationSchema);
    await graph.addQuads(orgQuads);
    console.log(`✅ Converted organization data to ${orgQuads.length} RDF triples`);

    // Example 3: Convert RDF back to structured data
    console.log('\n📝 Example 3: RDF to Structured Data Conversion');
    
    // Query for person data
    const personQuery = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ex: <http://example.org/>
      
      SELECT ?person ?name ?age ?email WHERE {
        ?person a foaf:Person ;
          foaf:name ?name .
        OPTIONAL { ?person foaf:age ?age }
        OPTIONAL { ?person foaf:email ?email }
      }
    `;

    const personResults = await graph.query(personQuery);
    const personBindings = [];
    for await (const binding of personResults) {
      personBindings.push({
        id: binding.get('person').value,
        name: binding.get('name').value,
        age: binding.get('age') ? parseInt(binding.get('age').value) : undefined,
        email: binding.get('email')?.value
      });
    }

    // Validate converted data against schema
    console.log('\n🔄 Converting RDF back to structured data:');
    for (const personBinding of personBindings) {
      try {
        const validated = PersonSchema.parse(personBinding);
        console.log(`  ✅ Validated: ${validated.name} (${validated.id})`);
      } catch (error) {
        console.log(`  ❌ Validation failed for ${personBinding.name}: ${error.message}`);
      }
    }

    // Example 4: Schema evolution and validation
    console.log('\n📝 Example 4: Schema Evolution');
    
    // Create an evolved schema with new required field
    const EvolvedPersonSchema = PersonSchema.extend({
      phone: z.string().regex(/^\+?[\d\s\-\(\)]+$/).optional(),
      department: z.string().optional()
    });

    // Test with old data (should still work)
    const oldPersonData = {
      id: 'http://example.org/oldperson',
      name: 'Old Person',
      age: 25
    };

    try {
      const validatedOld = EvolvedPersonSchema.parse(oldPersonData);
      console.log('✅ Old data compatible with evolved schema');
    } catch (error) {
      console.log(`❌ Old data incompatible: ${error.message}`);
    }

    // Test with new data including new fields
    const newPersonData = {
      id: 'http://example.org/newperson',
      name: 'New Person',
      age: 28,
      email: 'new@example.com',
      phone: '+1-555-123-4567',
      department: 'Engineering'
    };

    try {
      const validatedNew = EvolvedPersonSchema.parse(newPersonData);
      console.log('✅ New data validated against evolved schema');
      
      // Convert to RDF
      const newPersonQuads = zodHelper.toRdf(validatedNew, EvolvedPersonSchema);
      await graph.addQuads(newPersonQuads);
      console.log(`✅ Added ${newPersonQuads.length} new triples to graph`);
    } catch (error) {
      console.log(`❌ New data validation failed: ${error.message}`);
    }

    // Example 5: Custom validation with Zod transforms
    console.log('\n📝 Example 5: Custom Validation and Transforms');
    
    const CustomPersonSchema = z.object({
      id: z.string().url(),
      name: z.string().transform(name => name.trim().toLowerCase()),
      age: z.number().transform(age => Math.max(0, Math.min(150, age))),
      email: z.string().email().transform(email => email.toLowerCase()),
      tags: z.array(z.string()).transform(tags => [...new Set(tags)]), // Remove duplicates
      metadata: z.record(z.string(), z.any()).optional()
    });

    const customPersonData = {
      id: 'http://example.org/custom',
      name: '  CUSTOM PERSON  ',
      age: 200, // Will be clamped to 150
      email: 'CUSTOM@EXAMPLE.COM',
      tags: ['developer', 'engineer', 'developer', 'senior'], // Duplicates will be removed
      metadata: {
        source: 'api',
        version: '1.0'
      }
    };

    const transformedPerson = CustomPersonSchema.parse(customPersonData);
    console.log('✅ Custom validation and transformation applied:');
    console.log(`  - Name: "${transformedPerson.name}"`);
    console.log(`  - Age: ${transformedPerson.age}`);
    console.log(`  - Email: ${transformedPerson.email}`);
    console.log(`  - Tags: [${transformedPerson.tags.join(', ')}]`);

    // Example 6: Error handling and reporting
    console.log('\n📝 Example 6: Error Handling');
    
    const invalidData = {
      id: 'not-a-url',
      name: '',
      age: -5,
      email: 'invalid-email',
      tags: 'not-an-array'
    };

    try {
      PersonSchema.parse(invalidData);
    } catch (error) {
      console.log('✅ Caught validation errors:');
      if (error instanceof z.ZodError) {
        for (const issue of error.issues) {
          console.log(`  - ${issue.path.join('.')}: ${issue.message}`);
        }
      }
    }

    // Show final statistics
    const allQuads = graph.getQuads();
    console.log(`\n📊 Final Statistics:`);
    console.log(`  - Total RDF triples: ${allQuads.length}`);
    console.log(`  - Schemas created: 5`);
    console.log(`  - Data validations: 8`);
    console.log(`  - Successful conversions: 6`);

  } catch (error) {
    console.error('❌ Error:', error.message);
    console.error(error.stack);
  }
}

main();
