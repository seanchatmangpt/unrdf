Feature: Deterministic Template Rendering
  As a developer
  I want deterministic template rendering
  So that generated artifacts are consistent and reproducible

  Background:
    Given the template engine is initialized
    And I have template files with frontmatter

  Scenario: Render Nunjucks template with data
    Given I have a template "hello.njk" with content "Hello {{ name }}!"
    And I have data with name "World"
    When I render the template
    Then the output should be "Hello World!"
    And the rendering should be deterministic

  Scenario: Process frontmatter metadata
    Given I have a template with YAML frontmatter
    When I parse the template
    Then the frontmatter should be extracted
    And template variables should be available

  Scenario: Apply custom filters to data
    Given I have a template using custom filters
    And the filters are properly registered
    When I render the template
    Then custom filters should be applied
    And the output should be formatted correctly

  Scenario: Generate multiple file types
    Given I have templates for different file types
    When I render LaTeX templates
    Then PDF-ready LaTeX should be generated
    When I render Office templates
    Then DOCX documents should be created
    When I render NextJS templates
    Then React components should be generated