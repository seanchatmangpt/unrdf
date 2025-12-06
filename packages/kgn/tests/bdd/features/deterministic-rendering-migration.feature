Feature: Deterministic Template Rendering (Migration from Unjucks)
  As a developer migrating from unjucks
  I want template rendering to be completely deterministic with mocked dependencies
  So that I can trust the output for CI/CD and reproducible builds

  Background:
    Given a mocked template engine with deterministic environment
    And all external dependencies are stubbed with test doubles
    And time is frozen at "2024-01-01T00:00:00.000Z"
    And random seed is set to deterministic value 0.42
    And UUID generator produces predictable IDs
    And hash functions return deterministic values

  @P0 @deterministic @critical @migration
  Scenario: Template renders identically across multiple executions
    Given a mock template renderer
    And a template "component.njk" with variable substitution:
      """
      import React from 'react';

      export const {{ componentName|pascalCase }} = ({ {{ props.join(', ') }} }) => {
        const id = "{{ uuid() }}";
        const timestamp = "{{ now() }}";
        const hash = "{{ hash(componentName) }}";

        return (
          <div id={id} className="{{classPrefix}}-{{componentName|lower}}" data-created={timestamp}>
            <h1>{{ title }}</h1>
            <p>Hash: {hash}</p>
            {% for prop in props %}
            <span>{{ prop }}: {{{ prop }}}</span>{{ "," if not loop.last }}
            {% endfor %}
          </div>
        );
      };
      """
    And rendering data:
      | componentName | UserProfile         |
      | title        | User Profile Widget |
      | classPrefix  | widget              |
      | props        | ["name", "email"]   |
    When I render the template 100 times in parallel
    Then all outputs should be byte-identical
    And the mock renderer was called exactly 100 times
    And no external system interactions occurred
    And the output contains deterministic UUID "deterministic-uuid-0"
    And the output contains frozen timestamp "2024-01-01T00:00:00.000Z"
    And the output contains deterministic hash "sha256-11-deterministic"

  @P0 @deterministic @performance @migration
  Scenario: Deterministic rendering under high load (Unjucks performance requirement)
    Given a mocked complex template with nested structures and loops
    And stubbed data providers return consistent large datasets
    And memory monitoring is enabled
    When I render 1000 templates concurrently
    Then all outputs match their deterministic signatures
    And rendering completes within 3 seconds
    And memory usage remains constant (< 10MB increase)
    And CPU usage stays under 80%
    And no race conditions occur

  @P0 @deterministic @frontmatter @migration
  Scenario: YAML frontmatter processing with mocked file system
    Given a mocked file system with template "config.njk"
    And stubbed frontmatter parser
    And template content with complex frontmatter:
      """
      ---
      title: "{{ projectName }} Configuration"
      version: "{{ version }}"
      generated: "{{ now() }}"
      checksum: "{{ hash(content) }}"
      defaults:
        environment: "{{ env.NODE_ENV || 'production' }}"
        debug: false
        features:
          - authentication
          - caching
          - monitoring
      computed:
        apiUrl: "https://api.{{ domain }}/v{{ version.split('.')[0] }}"
        timeout: 5000
        retries: 3
      ---
      export const config = {
        title: "{{ title }}",
        version: "{{ version }}",
        generated: "{{ generated }}",
        checksum: "{{ checksum }}",
        apiUrl: "{{ computed.apiUrl }}",
        timeout: {{ computed.timeout }},
        environment: "{{ defaults.environment }}",
        debug: {{ defaults.debug }},
        features: [
        {% for feature in defaults.features %}
          "{{ feature }}"{{ "," if not loop.last }}
        {% endfor %}
        ],
        retryPolicy: {
          maxRetries: {{ computed.retries }},
          backoff: "exponential"
        }
      };
      """
    And template variables:
      | projectName | KGEN-Migration |
      | version     | 2.0.0         |
      | domain      | kgen.dev      |
    When I parse frontmatter and render with mocked dependencies
    Then frontmatter should be extracted deterministically
    And computed values should resolve consistently
    And environment variables should be mocked
    And the final output should match golden file "config-migration-golden.js"
    And the JSON structure should be valid
    And all timestamps should be frozen
    And all hashes should be deterministic

  @P0 @deterministic @cross-platform @migration
  Scenario: Cross-platform deterministic rendering (Linux/Mac/Windows)
    Given mocked file system with platform-agnostic paths
    And stubbed OS-specific operations
    And templates with path references:
      """
      const paths = {
        config: "{{ configPath|normalize }}",
        templates: "{{ templatesDir|normalize }}",
        output: "{{ outputDir|normalize }}"
      };

      export const platformConfig = {
        platform: "{{ os.platform() }}",
        separator: "{{ path.sep }}",
        homedir: "{{ os.homedir()|normalize }}",
        paths
      };
      """
    And cross-platform test data with different path styles
    When I render templates on mocked Linux, macOS, and Windows environments
    Then core content should be identical across all platforms
    And only path separators should be normalized appropriately
    And OS-specific values should be mocked consistently
    And line endings should be normalized to LF
    And the golden test should pass on all mocked platforms

  @P0 @deterministic @error-handling @migration
  Scenario: Deterministic error handling with mocked failures
    Given a template with potential error conditions
    And mocked dependencies that can simulate failures
    And error tracking is enabled
    When template processing encounters mocked errors:
      | error_type          | mock_response           | expected_behavior        |
      | missing_variable    | undefined               | default_value_used       |
      | file_not_found      | ENOENT                  | graceful_fallback        |
      | invalid_json        | SyntaxError             | validation_error_thrown  |
      | network_timeout     | timeout                 | retry_with_backoff       |
      | permission_denied   | EACCES                  | alternative_path_used    |
    Then error handling should be deterministic
    And the same error should produce the same response
    And error messages should be consistent
    And no actual external systems should be accessed
    And recovery mechanisms should work predictably

  @P0 @deterministic @filters @migration
  Scenario: All 47 core filters work deterministically with mocks
    Given mocked filter registry with all unjucks filters ported
    And stubbed external filter dependencies (date libs, formatters, etc.)
    And deterministic test data for each filter category
    When I apply each of the 47 filters in isolation
    Then each filter produces consistent output
    And no filter accesses external systems
    And all filters complete within 1ms each
    And complex filter chains work deterministically
    And filter output matches expected golden values

  @P0 @deterministic @memory-management @migration
  Scenario: Memory-efficient deterministic rendering
    Given memory monitoring with mock objects
    And large template datasets (1000+ templates)
    And stubbed garbage collection triggers
    When I render templates in batches
    Then memory usage should be predictable
    And mock objects should be properly cleaned up
    And no memory leaks should occur
    And peak memory usage should stay under 100MB
    And GC should be called at predictable intervals