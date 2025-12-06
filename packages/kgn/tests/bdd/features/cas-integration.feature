Feature: Content-Addressed Storage Integration
  As a KGEN user
  I want templates to integrate with CAS
  So that I can ensure content integrity and attestation

  Background:
    Given the CAS system is initialized
    And the template engine is configured for CAS integration
    And I have test content ready for storage

  @cas @attestation @critical
  Scenario: Store rendered templates in CAS with attestation
    Given I have a template that renders to source code
    And I have configured CAS attestation settings
    When I render and store the template output
    Then the content should be stored with a content hash
    And an attestation should be created
    And the attestation should include template metadata
    And I should be able to retrieve by content hash
    And the stored content should match exactly

  @cas @performance @critical
  Scenario: Batch template rendering with CAS storage
    Given I have 100 templates to render
    And CAS is configured for batch operations
    When I render all templates and store in CAS
    Then the throughput should exceed 50 templates per second
    And all content should be deduplicated properly
    And storage efficiency should be optimized
    And attestations should be created for each unique output

  @cas @integrity @critical
  Scenario: Verify template output integrity through CAS
    Given I have stored template output in CAS
    And the original template and data are available
    When I re-render the template with the same data
    Then the new output hash should match the stored hash
    And content integrity should be verified
    And the attestation should remain valid
    And no corruption should be detected

  @cas @provenance @critical
  Scenario: Track template provenance through CAS attestation
    Given I have a template with complex dependencies
    And the template includes imported components
    When I render and attest the template
    Then the provenance graph should include all dependencies
    And template lineage should be traceable
    And version information should be preserved
    And the complete build context should be attestable

  @cas @deduplication
  Scenario: Handle template output deduplication
    Given I have multiple templates that produce identical output
    When I render and store all templates
    Then only one copy of identical content should be stored
    And all templates should reference the same content hash
    And storage space should be optimized
    And attestations should reflect the deduplication

  @cas @garbage-collection
  Scenario: Clean up unused template outputs
    Given I have stored many template outputs in CAS
    And some outputs are no longer referenced
    When I run CAS garbage collection
    Then unreferenced template outputs should be removed
    And referenced outputs should be preserved
    And storage should be compacted
    And performance should be maintained