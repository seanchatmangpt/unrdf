Feature: Advanced Template Features
  As a code generator developer
  I want advanced templating capabilities
  So that I can create sophisticated code artifacts

  Background:
    Given the template engine supports advanced features
    And I have comprehensive test fixtures loaded

  @latex @office @critical
  Scenario: Generate LaTeX documents with mathematical notation
    Given I have a LaTeX template "research-paper.tex" with content:
      """
      \documentclass{article}
      \usepackage{amsmath}
      \title{{ "{"}}{{ title }}{{ "}" }}
      \author{{ "{"}}{{ authors|join(' \\and ') }}{{ "}" }}

      \begin{document}
      \maketitle

      \section{Abstract}
      {{ abstract }}

      \section{Mathematical Model}
      {% for equation in equations %}
      \begin{equation}
      {{ equation.latex }}
      \end{equation}
      {% endfor %}

      \section{Results}
      \begin{tabular}{|l|c|c|}
      \hline
      Parameter & Value & Unit \\
      \hline
      {% for result in results %}
      {{ result.name }} & {{ result.value }} & {{ result.unit }} \\
      {% endfor %}
      \hline
      \end{tabular}
      \end{document}
      """
    And I provide LaTeX document data with equations and results
    When I render the LaTeX template
    Then the output should be valid LaTeX syntax
    And mathematical notation should be properly formatted
    And the result should match the golden LaTeX file
    And the document should compile without errors

  @office @docx @critical
  Scenario: Generate Office documents with structured content
    Given I have an Office template for Word documents
    And the template includes tables, headers, and formatting
    When I render with business report data
    Then a valid DOCX structure should be generated
    And formatting should be preserved
    And tables should contain correct data
    And the output should match the golden DOCX structure

  @nextjs @react @critical
  Scenario: Generate NextJS components with TypeScript
    Given I have NextJS templates for React components:
      """
      // {{ componentName }}.tsx
      import React, { useState, useEffect } from 'react';
      {% if hasProps %}
      interface {{ componentName }}Props {
      {% for prop in props %}
        {{ prop.name }}{{ "?" if prop.optional }}: {{ prop.type }};
      {% endfor %}
      }
      {% endif %}

      export const {{ componentName }}: React.FC{% if hasProps %}<{{ componentName }}Props>{% endif %} = ({% if hasProps %}{ {{ props|map(attribute='name')|join(', ') }} }{% endif %}) => {
      {% for hook in hooks %}
        const [{{ hook.state }}, set{{ hook.state|capitalize }}] = useState<{{ hook.type }}>({{ hook.initial }});
      {% endfor %}

      {% if hasEffects %}
        useEffect(() => {
      {% for effect in effects %}
          {{ effect.code }}
      {% endfor %}
        }, [{{ effects|map(attribute='deps')|join(', ') }}]);
      {% endif %}

        return (
          <div className="{{ componentName|kebabCase }}">
      {% for element in elements %}
            <{{ element.tag }}{{ ' className="' + element.className + '"' if element.className }}>
              {{ element.content }}
            </{{ element.tag }}>
      {% endfor %}
          </div>
        );
      };
      """
    When I render with TypeScript component specifications
    Then valid TypeScript React components should be generated
    And proper imports and interfaces should be included
    And the components should be type-safe
    And the output should match golden TypeScript files

  @error-handling @validation
  Scenario: Handle template errors gracefully
    Given I have a template with syntax errors
    When I attempt to render the template
    Then a descriptive error should be thrown
    And the error should include line numbers and context
    And the template engine should remain stable
    And subsequent renders should work correctly

  @performance @memory
  Scenario: Handle large template datasets efficiently
    Given I have a template that processes 10,000 data items
    And the template includes complex nested structures
    When I render the template
    Then memory usage should not exceed 100MB
    And rendering should complete within 5 seconds
    And the output should be correct and complete
    And garbage collection should clean up properly