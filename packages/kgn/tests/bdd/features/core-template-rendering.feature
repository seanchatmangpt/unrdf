Feature: Core Template Rendering Engine
  As a software developer
  I want reliable deterministic template rendering
  So that I can generate consistent code artifacts

  Background:
    Given the KGEN template engine is initialized
    And I have a clean test environment
    And the golden test fixtures are loaded

  @golden @critical
  Scenario: Render Nunjucks template with variable substitution
    Given I have a template "component.njk" with content:
      """
      import React from 'react';

      export const {{ componentName }} = ({ {{ props.join(', ') }} }) => {
        return (
          <div className="{{classPrefix}}-{{componentName|lower}}">
            <h1>{{ title }}</h1>
            {% for prop in props %}
            <p>{{ prop }}: {{{ prop }}}</p>
            {% endfor %}
          </div>
        );
      };
      """
    And I have rendering data:
      | componentName | UserProfile         |
      | title        | User Profile Widget |
      | classPrefix  | widget              |
      | props        | ["name", "email"]   |
    When I render the template with the data
    Then the output should match the golden file "component-golden.js"
    And the rendering should be byte-identical across multiple runs
    And the output should contain valid React component syntax

  @golden @critical
  Scenario: Process YAML frontmatter with variable inheritance
    Given I have a template "config.njk" with frontmatter:
      """
      ---
      title: "{{ projectName }} Configuration"
      version: "{{ version }}"
      author: "{{ author }}"
      defaults:
        environment: production
        debug: false
        features:
          - authentication
          - caching
      variables:
        apiUrl: "https://api.{{ domain }}"
        timeout: 5000
      ---
      export const config = {
        title: "{{ title }}",
        version: "{{ version }}",
        apiUrl: "{{ apiUrl }}",
        timeout: {{ timeout }},
        environment: "{{ defaults.environment }}",
        debug: {{ defaults.debug|lower }},
        features: [
        {% for feature in defaults.features %}
          "{{ feature }}"{{ "," if not loop.last }}
        {% endfor %}
        ]
      };
      """
    And I provide template data:
      | projectName | KGen Project |
      | version     | 1.0.0       |
      | author      | DfLLSS Team |
      | domain      | example.com |
    When I parse the frontmatter and render the template
    Then the frontmatter should be correctly extracted
    And variables should inherit from frontmatter defaults
    And the final output should match the golden file "config-golden.js"
    And the JSON structure should be valid

  @filters @critical
  Scenario: Apply custom filters for code generation
    Given I have registered custom filters:
      | filter        | description                    |
      | camelCase     | Convert to camelCase           |
      | snakeCase     | Convert to snake_case          |
      | kebabCase     | Convert to kebab-case          |
      | pascalCase    | Convert to PascalCase          |
      | pluralize     | Make word plural               |
      | singularize   | Make word singular             |
      | indent        | Indent text by specified level |
    And I have a template using multiple filters:
      """
      class {{ entityName|pascalCase }} {
        constructor() {
          this.{{ entityName|camelCase }}Id = null;
          this.{{ entityName|snakeCase }}_data = {};
        }

        get{{ entityName|pascalCase|pluralize }}() {
          return this.api.get('/{{ entityName|kebabCase|pluralize }}');
        }

      {{ methods|indent(2) }}
      }
      """
    When I render the template with entity "user_profile" and methods data
    Then custom filters should be applied correctly
    And the output should use proper naming conventions
    And the generated class should be syntactically valid
    And the result should match the golden output

  @deterministic @performance
  Scenario: Ensure deterministic rendering under high load
    Given I have a complex template with nested loops and conditionals
    And I have a dataset with 100 items
    When I render the template 50 times in parallel
    Then all outputs should be byte-identical
    And rendering should complete within 1 second per run
    And memory usage should remain stable
    And no race conditions should occur

  @cross-platform
  Scenario: Generate consistent output across operating systems
    Given I have templates with file path references
    And I have cross-platform test data
    When I render templates on Linux, macOS, and Windows
    Then the core content should be identical across platforms
    And only platform-specific paths should differ
    And line endings should be normalized
    And the golden test should pass on all platforms