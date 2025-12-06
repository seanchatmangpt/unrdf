# London BDD Feature: Deterministic Template Generation
# Behavior-Driven Development specifications for guaranteed determinism

Feature: Deterministic Template Generation
  As a KGEN template system user
  I want all template outputs to be completely deterministic
  So that builds are reproducible, verifiable, and secure across all environments

  Background:
    Given the KGEN template system is initialized
    And all nondeterministic functions are replaced with deterministic alternatives
    And the system uses fixed timestamps and seeded random generation

  # ===== CORE DETERMINISM GUARANTEES =====

  Rule: Same input must always produce identical output

    Scenario: Identical template renders
      Given a template "basic-template.njk"
      And fixed input data:
        | field     | value           |
        | name      | test-project    |
        | version   | 1.0.0          |
        | buildTime | 2025-01-01T00:00:00.000Z |
      When I render the template 5 times with the same input
      Then all 5 outputs should be byte-for-byte identical
      And the output hash should be "SHA256:a1b2c3d4e5f6..."

    Scenario: Template with complex data structures
      Given a template with nested objects and arrays
      And input data with multiple levels of nesting
      When I render the template multiple times
      Then object keys should always appear in sorted order
      And array elements should maintain stable ordering
      And the complete output should be identical across renders

  Rule: Templates must be independent of system time

    Scenario: Time-independent template generation
      Given a template that previously used timestamps
      And the template now uses fixed metadata timestamps
      When I render the template at different times of day
      And I render the same template on different dates
      Then all outputs should be identical regardless of execution time
      And no system timestamp should appear in the output

    Scenario: Build metadata consistency
      Given a template that includes build information
      And build metadata comes from configuration, not system time
      When the template is rendered across multiple builds
      Then the build timestamp should be consistent
      And version information should be deterministic

  # ===== CROSS-PLATFORM CONSISTENCY =====

  Rule: Templates must produce identical output across all platforms

    Scenario Outline: Cross-platform template consistency
      Given a template "cross-platform-test.njk"
      And standardized input data
      When I render the template on <platform>
      Then the output should match the golden file
      And file paths should use forward slashes consistently
      And line endings should be normalized to LF

      Examples:
        | platform       |
        | Ubuntu Linux   |
        | macOS          |
        | Windows        |
        | Alpine Linux   |

    Scenario: Platform-specific configurations eliminated
      Given templates previously using process.platform
      And templates now using configuration-based platform targeting
      When I render templates with different target platforms in config
      Then outputs should vary only by configured differences
      And no runtime platform detection should occur

  # ===== ENVIRONMENT INDEPENDENCE =====

  Rule: Templates must not depend on environment variables

    Scenario: Environment variable elimination
      Given templates that previously accessed process.env
      And templates now use explicit configuration
      When I render templates with different environment variables set
      Then the template output should remain identical
      And no environment variable values should leak into output

    Scenario: Configuration-based environment handling
      Given templates requiring environment-specific behavior
      And explicit configuration parameters for environment settings
      When I render templates for different target environments
      Then outputs should vary only by explicit configuration
      And environment detection should be configuration-driven

  # ===== DATA STRUCTURE DETERMINISM =====

  Rule: Object iteration must be deterministic

    Scenario: Sorted object key iteration
      Given a template iterating over object properties
      And the template uses sorted key iteration
      When I render the template with objects having keys in different orders
      Then output should always show keys in alphabetical order
      And key ordering should be stable across renders

    Scenario: Deterministic collection processing
      Given templates processing Sets and Maps
      And deterministic iteration order enforced
      When I render templates with collections
      Then collection elements should appear in consistent order
      And ordering should not depend on insertion sequence

  # ===== RANDOM GENERATION DETERMINISM =====

  Rule: All randomness must be deterministic

    Scenario: Seeded random generation
      Given templates requiring random-like values
      And deterministic pseudo-random generation with fixed seeds
      When I render templates requiring "random" IDs
      Then the same input should generate the same "random" values
      And randomness should be derived from input content hash

    Scenario: UUID generation determinism
      Given templates requiring unique identifiers
      And content-hash-based ID generation
      When I render templates needing UUIDs
      Then IDs should be deterministic based on content
      And identical content should produce identical IDs

  # ===== FILE SYSTEM DETERMINISM =====

  Rule: File system operations must be deterministic

    Scenario: Sorted directory reading
      Given templates that list directory contents
      And directory reading with consistent sorting
      When I render templates that include file lists
      Then files should be listed in alphabetical order
      And ordering should be independent of filesystem

    Scenario: Glob pattern determinism
      Given templates using glob patterns
      And sorted results from glob operations
      When I render templates with file globbing
      Then matched files should appear in sorted order
      And results should be consistent across filesystems

  # ===== NETWORK INDEPENDENCE =====

  Rule: Templates must not make network calls

    Scenario: Network call elimination
      Given templates previously making HTTP requests
      And templates now using local data sources only
      When I render templates in network-isolated environments
      Then rendering should succeed without network access
      And all required data should be provided locally

  # ===== FLOATING POINT DETERMINISM =====

  Rule: Numeric operations must be deterministic

    Scenario: Consistent floating point operations
      Given templates performing mathematical calculations
      And explicit precision controls for floating point operations
      When I render templates with numeric computations
      Then results should be identical across different systems
      And floating point precision should be explicitly controlled

  # ===== PERFORMANCE WITH DETERMINISM =====

  Rule: Deterministic templates should maintain acceptable performance

    Scenario: Performance impact assessment
      Given deterministic template implementations
      When I measure template rendering performance
      Then rendering time should be within 20% of non-deterministic versions
      And memory usage should not increase significantly
      And throughput should remain acceptable for production use

  # ===== REGRESSION PREVENTION =====

  Rule: Nondeterministic patterns must be prevented

    Scenario: Lint rule enforcement
      Given ESLint configuration with determinism rules
      When developers write template code
      Then nondeterministic patterns should be detected at development time
      And CI/CD should reject code with determinism violations
      And code review should include determinism checks

    Scenario: Golden file validation
      Given golden files for all template outputs
      When templates are modified
      Then output changes should be explicitly reviewed
      And golden files should be updated only with approval
      And unexpected output changes should fail CI

  # ===== AUDIT AND COMPLIANCE =====

  Rule: Determinism must be continuously verified

    Scenario: Determinism audit trail
      Given template rendering with audit logging
      When templates are rendered in production
      Then audit logs should capture deterministic parameters
      And template inputs and outputs should be traceable
      And determinism compliance should be verifiable

    Scenario: Compliance validation
      Given regulatory requirements for reproducible builds
      When templates are used in compliance-sensitive contexts
      Then all outputs should be reproducible and verifiable
      And determinism should be attestable through cryptographic proofs
      And build authenticity should be cryptographically guaranteed

  # ===== ERROR HANDLING =====

  Rule: Error conditions must be deterministic

    Scenario: Consistent error messages
      Given templates that may encounter errors
      When error conditions occur during rendering
      Then error messages should be deterministic and not include timestamps
      And error handling should not introduce nondeterminism
      And stack traces should be normalized for consistency

    Scenario: Graceful degradation
      Given templates with optional features
      When optional features are unavailable
      Then fallback behavior should be deterministic
      And degraded output should be reproducible
      And feature availability should not affect determinism of core output