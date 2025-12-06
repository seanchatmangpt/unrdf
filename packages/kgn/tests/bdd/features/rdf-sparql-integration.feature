Feature: RDF and SPARQL Integration
  As a semantic web developer
  I want templates to integrate with RDF graphs
  So that I can generate semantically-aware code artifacts

  Background:
    Given an RDF graph store is available
    And SPARQL query engine is initialized
    And I have ontology data loaded

  @rdf @sparql @critical
  Scenario: Generate code from RDF class definitions
    Given I have an RDF ontology with class definitions:
      """
      @prefix ex: <http://example.org/> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .

      ex:Person a owl:Class ;
          rdfs:label "Person" ;
          rdfs:comment "A person entity" .

      ex:name a owl:DatatypeProperty ;
          rdfs:domain ex:Person ;
          rdfs:range xsd:string .

      ex:age a owl:DatatypeProperty ;
          rdfs:domain ex:Person ;
          rdfs:range xsd:integer .
      """
    And I have a template that queries RDF for class definitions
    When I execute the SPARQL query to get class properties
    And I render the template with the RDF data
    Then a complete class definition should be generated
    And all properties should be included
    And the output should match RDF golden files
    And semantic relationships should be preserved

  @sparql @query @critical
  Scenario: Execute SPARQL queries within templates
    Given I have a template with embedded SPARQL queries:
      """
      // Generated from ontology
      {% set classes = sparql("SELECT ?class ?label WHERE { ?class a owl:Class . ?class rdfs:label ?label }") %}

      export const ontologyClasses = {
      {% for result in classes %}
        {{ result.class|localName|camelCase }}: {
          uri: "{{ result.class }}",
          label: "{{ result.label }}"
        }{{ "," if not loop.last }}
      {% endfor %}
      };

      {% set properties = sparql("SELECT ?prop ?domain ?range WHERE { ?prop a owl:DatatypeProperty . ?prop rdfs:domain ?domain . ?prop rdfs:range ?range }") %}

      export const propertyMappings = {
      {% for prop in properties %}
        {{ prop.prop|localName|camelCase }}: {
          uri: "{{ prop.prop }}",
          domain: "{{ prop.domain|localName }}",
          range: "{{ prop.range|localName }}"
        }{{ "," if not loop.last }}
      {% endfor %}
      };
      """
    When I render the template with SPARQL integration
    Then SPARQL queries should execute successfully
    And results should be properly formatted in the template
    And the generated JavaScript should be valid
    And ontology structure should be accurately represented

  @rdf @validation @critical
  Scenario: Validate template output against RDF constraints
    Given I have SHACL shapes for data validation:
      """
      @prefix sh: <http://www.w3.org/ns/shacl#> .
      @prefix ex: <http://example.org/> .

      ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Person ;
          sh:property [
              sh:path ex:name ;
              sh:datatype xsd:string ;
              sh:minCount 1 ;
              sh:maxCount 1 ;
          ] ;
          sh:property [
              sh:path ex:age ;
              sh:datatype xsd:integer ;
              sh:minCount 0 ;
              sh:maxCount 1 ;
              sh:minInclusive 0 ;
          ] .
      """
    And I have a template that generates RDF data
    When I render the template and validate against SHACL shapes
    Then the generated RDF should pass SHACL validation
    And constraint violations should be reported
    And the validation report should be detailed
    And conformant data should be accepted

  @rdf @inference @advanced
  Scenario: Generate code using RDF inference
    Given I have an RDF graph with inference rules
    And the reasoner is enabled
    When I query for inferred relationships in templates
    Then implicit relationships should be discovered
    And inferred data should be available for code generation
    And the generated code should reflect the complete model
    And reasoning results should be consistent

  @performance @rdf
  Scenario: Optimize SPARQL queries in template rendering
    Given I have templates with complex SPARQL queries
    And a large RDF dataset is loaded
    When I render multiple templates simultaneously
    Then query performance should be acceptable (< 100ms per query)
    And query results should be cached appropriately
    And memory usage should remain reasonable
    And concurrent queries should not interfere