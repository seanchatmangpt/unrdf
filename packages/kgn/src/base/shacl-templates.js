/**
 * KGEN SHACL Templates - RDF validation shape templates
 * 
 * Provides templates for creating SHACL (Shapes Constraint Language) shapes
 * for validating RDF knowledge graphs used in KGEN template generation
 */

export class KGenSHACLTemplates {
  constructor(options = {}) {
    this.options = {
      deterministicMode: options.deterministicMode !== false,
      staticBuildTime: options.staticBuildTime || '2024-01-01T00:00:00.000Z',
      namespace: options.namespace || 'http://kgen.ai/shacl#',
      baseIRI: options.baseIRI || 'http://kgen.ai/',
      ...options
    };
    
    this.shapeTemplates = new Map();
    this.initializeSHACLTemplates();
  }

  /**
   * Initialize built-in SHACL shape templates
   */
  initializeSHACLTemplates() {
    // Basic node shape template
    this.registerShapeTemplate('basic_node_shape', {
      name: '{{ shapeName }}Shape',
      description: 'Basic node shape for {{ targetClass | default("Resource") }}',
      template: `@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix kgen: <${this.options.namespace}> .
@prefix ex: <${this.options.baseIRI}> .

ex:{{ shapeName }}Shape
    a sh:NodeShape ;
    sh:targetClass ex:{{ targetClass | default(shapeName) }} ;
    {% if description -%}
    rdfs:comment "{{ description }}" ;
    {% endif -%}
    {% for property in properties | default([]) -%}
    sh:property [
        sh:path ex:{{ property.path }} ;
        {% if property.datatype -%}
        sh:datatype {{ property.datatype }} ;
        {% endif -%}
        {% if property.minCount -%}
        sh:minCount {{ property.minCount }} ;
        {% endif -%}
        {% if property.maxCount -%}
        sh:maxCount {{ property.maxCount }} ;
        {% endif -%}
        {% if property.pattern -%}
        sh:pattern "{{ property.pattern }}" ;
        {% endif -%}
        {% if property.message -%}
        sh:message "{{ property.message }}" ;
        {% endif -%}
    ] ;
    {% endfor -%}
.`
    });

    // Property shape template
    this.registerShapeTemplate('property_shape', {
      name: '{{ shapeName }}PropertyShape',
      description: 'Property shape for {{ propertyPath }}',
      template: `@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix kgen: <${this.options.namespace}> .
@prefix ex: <${this.options.baseIRI}> .

ex:{{ shapeName }}PropertyShape
    a sh:PropertyShape ;
    sh:path ex:{{ propertyPath }} ;
    {% if datatype -%}
    sh:datatype {{ datatype }} ;
    {% endif -%}
    {% if nodeKind -%}
    sh:nodeKind {{ nodeKind }} ;
    {% endif -%}
    {% if minCount !== undefined -%}
    sh:minCount {{ minCount }} ;
    {% endif -%}
    {% if maxCount !== undefined -%}
    sh:maxCount {{ maxCount }} ;
    {% endif -%}
    {% if minLength -%}
    sh:minLength {{ minLength }} ;
    {% endif -%}
    {% if maxLength -%}
    sh:maxLength {{ maxLength }} ;
    {% endif -%}
    {% if pattern -%}
    sh:pattern "{{ pattern }}" ;
    {% endif -%}
    {% if in -%}
    sh:in ( {{ in | join(' ') }} ) ;
    {% endif -%}
    {% if message -%}
    sh:message "{{ message }}" ;
    {% endif -%}
    {% if severity -%}
    sh:severity {{ severity }} ;
    {% endif -%}
.`
    });

    // Template validation shape
    this.registerShapeTemplate('template_shape', {
      name: 'TemplateShape',
      description: 'Validation shape for KGEN templates',
      template: `@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix kgen: <${this.options.namespace}> .
@prefix ex: <${this.options.baseIRI}> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

kgen:TemplateShape
    a sh:NodeShape ;
    sh:targetClass kgen:Template ;
    rdfs:comment "Validation shape for KGEN templates" ;
    
    # Template must have a name
    sh:property [
        sh:path kgen:templateName ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:pattern "^[a-zA-Z][a-zA-Z0-9_-]*$" ;
        sh:message "Template name must be a valid identifier" ;
    ] ;
    
    # Template must have content
    sh:property [
        sh:path kgen:templateContent ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:minLength 1 ;
        sh:message "Template must have content" ;
    ] ;
    
    # Template version
    sh:property [
        sh:path kgen:version ;
        sh:datatype xsd:string ;
        sh:pattern "^\\d+\\.\\d+\\.\\d+$" ;
        sh:message "Version must follow semantic versioning" ;
    ] ;
    
    # Template category
    sh:property [
        sh:path kgen:category ;
        sh:datatype xsd:string ;
        sh:in ( "component" "layout" "module" "utility" "custom" ) ;
        sh:message "Category must be one of: component, layout, module, utility, custom" ;
    ] ;
    
    # Deterministic flag
    sh:property [
        sh:path kgen:isDeterministic ;
        sh:datatype xsd:boolean ;
        sh:maxCount 1 ;
    ] ;
.`
    });

    // Filter validation shape
    this.registerShapeTemplate('filter_shape', {
      name: 'FilterShape',
      description: 'Validation shape for KGEN filters',
      template: `@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix kgen: <${this.options.namespace}> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

kgen:FilterShape
    a sh:NodeShape ;
    sh:targetClass kgen:Filter ;
    rdfs:comment "Validation shape for KGEN filters" ;
    
    # Filter must have a name
    sh:property [
        sh:path kgen:filterName ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:pattern "^[a-zA-Z][a-zA-Z0-9_]*$" ;
        sh:message "Filter name must be a valid JavaScript identifier" ;
    ] ;
    
    # Filter function code
    sh:property [
        sh:path kgen:filterCode ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:message "Filter must have implementation code" ;
    ] ;
    
    # Filter must be deterministic
    sh:property [
        sh:path kgen:isDeterministic ;
        sh:datatype xsd:boolean ;
        sh:hasValue true ;
        sh:message "All KGEN filters must be deterministic" ;
    ] ;
.`
    });

    // Knowledge graph shape
    this.registerShapeTemplate('knowledge_graph_shape', {
      name: 'KnowledgeGraphShape',
      description: 'Validation shape for KGEN knowledge graphs',
      template: `@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix kgen: <${this.options.namespace}> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

kgen:KnowledgeGraphShape
    a sh:NodeShape ;
    sh:targetClass kgen:KnowledgeGraph ;
    rdfs:comment "Validation shape for KGEN knowledge graphs" ;
    
    # Graph must have an identifier
    sh:property [
        sh:path kgen:graphId ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:pattern "^[a-zA-Z][a-zA-Z0-9_-]*$" ;
        sh:message "Graph ID must be a valid identifier" ;
    ] ;
    
    # Graph version
    sh:property [
        sh:path kgen:graphVersion ;
        sh:datatype xsd:string ;
        sh:pattern "^\\d+\\.\\d+\\.\\d+$" ;
        sh:message "Graph version must follow semantic versioning" ;
    ] ;
    
    # Creation timestamp
    sh:property [
        sh:path kgen:createdAt ;
        sh:datatype xsd:dateTime ;
        sh:maxCount 1 ;
    ] ;
    
    # Modification timestamp  
    sh:property [
        sh:path kgen:modifiedAt ;
        sh:datatype xsd:dateTime ;
        sh:maxCount 1 ;
    ] ;
.`
    });

    // Component shape template
    this.registerShapeTemplate('component_shape', {
      name: 'ComponentShape',
      description: 'Validation shape for template components',
      template: `@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix kgen: <${this.options.namespace}> .
@prefix ex: <${this.options.baseIRI}> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:{{ shapeName | default('Component') }}Shape
    a sh:NodeShape ;
    sh:targetClass ex:{{ targetClass | default('Component') }} ;
    rdfs:comment "{{ description | default('Validation shape for components') }}" ;
    
    {% for property in requiredProperties | default([]) -%}
    # Required property: {{ property.name }}
    sh:property [
        sh:path ex:{{ property.name }} ;
        {% if property.datatype -%}
        sh:datatype {{ property.datatype }} ;
        {% endif -%}
        sh:minCount 1 ;
        {% if property.maxCount -%}
        sh:maxCount {{ property.maxCount }} ;
        {% endif -%}
        {% if property.pattern -%}
        sh:pattern "{{ property.pattern }}" ;
        {% endif -%}
        sh:message "{{ property.message | default('Property ' + property.name + ' is required') }}" ;
    ] ;
    
    {% endfor -%}
    {% for property in optionalProperties | default([]) -%}
    # Optional property: {{ property.name }}
    sh:property [
        sh:path ex:{{ property.name }} ;
        {% if property.datatype -%}
        sh:datatype {{ property.datatype }} ;
        {% endif -%}
        {% if property.maxCount -%}
        sh:maxCount {{ property.maxCount }} ;
        {% endif -%}
        {% if property.pattern -%}
        sh:pattern "{{ property.pattern }}" ;
        {% endif -%}
    ] ;
    
    {% endfor -%}
.`
    });

    // Validation rule template
    this.registerShapeTemplate('validation_rule', {
      name: 'ValidationRuleShape',
      description: 'Template for custom validation rules',
      template: `@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix kgen: <${this.options.namespace}> .
@prefix ex: <${this.options.baseIRI}> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:{{ ruleName }}Rule
    a sh:NodeShape ;
    {% if targetClass -%}
    sh:targetClass ex:{{ targetClass }} ;
    {% endif -%}
    {% if targetNode -%}
    sh:targetNode ex:{{ targetNode }} ;
    {% endif -%}
    rdfs:comment "{{ description | default('Custom validation rule') }}" ;
    
    {% if sparqlConstraint -%}
    sh:sparql [
        sh:message "{{ sparqlMessage | default('SPARQL constraint violation') }}" ;
        sh:prefixes [
            sh:declare [
                sh:prefix "kgen" ;
                sh:namespace "${this.options.namespace}" ;
            ] ;
            sh:declare [
                sh:prefix "ex" ;
                sh:namespace "${this.options.baseIRI}" ;
            ] ;
        ] ;
        sh:select """
            {{ sparqlConstraint }}
        """ ;
    ] ;
    {% endif -%}
.`
    });
  }

  /**
   * Register a SHACL shape template
   */
  registerShapeTemplate(name, template) {
    this.shapeTemplates.set(name, {
      ...template,
      registered: this.options.staticBuildTime,
      namespace: this.options.namespace
    });
  }

  /**
   * Generate SHACL shape from template
   */
  generateShape(templateName, context = {}) {
    const template = this.shapeTemplates.get(templateName);
    if (!template) {
      throw new Error(`SHACL template '${templateName}' not found`);
    }
    
    // Simple template substitution for SHACL
    let shapeContent = template.template;
    
    // Add standard prefixes if not present
    if (!shapeContent.includes('@prefix sh:')) {
      const prefixes = [
        '@prefix sh: <http://www.w3.org/ns/shacl#> .',
        `@prefix kgen: <${this.options.namespace}> .`,
        `@prefix ex: <${this.options.baseIRI}> .`,
        '@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .',
        '@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .',
        ''
      ].join('\n');
      
      shapeContent = prefixes + shapeContent;
    }
    
    // Replace template variables
    for (const [key, value] of Object.entries(context)) {
      const regex = new RegExp(`{{\\s*${key}\\s*(?:\\|[^}]*)?\\s*}}`, 'g');
      
      const matches = shapeContent.match(regex);
      if (matches) {
        for (const match of matches) {
          const defaultMatch = match.match(/\|\s*default\('([^']*)'\)/);
          const replacement = value !== undefined ? value : (defaultMatch ? defaultMatch[1] : match);
          shapeContent = shapeContent.replace(match, String(replacement));
        }
      }
    }
    
    // Process simple loops and conditions
    shapeContent = this.processTemplateLogic(shapeContent, context);
    
    return {
      name: this.interpolate(template.name, context),
      description: this.interpolate(template.description, context),
      content: shapeContent,
      metadata: {
        template: templateName,
        generated: this.options.staticBuildTime,
        namespace: this.options.namespace,
        context: Object.keys(context)
      }
    };
  }

  /**
   * Process template logic (loops and conditions)
   */
  processTemplateLogic(content, context) {
    let result = content;
    
    // Process {% for %} loops
    const forLoopRegex = /{%\s*for\s+(\w+)\s+in\s+([\w.\[\]|()]+)\s*%}([\s\S]*?){%\s*endfor\s*%}/g;
    
    result = result.replace(forLoopRegex, (match, itemVar, listExpr, loopContent) => {
      const listValue = this.evaluateExpression(listExpr, context);
      if (!Array.isArray(listValue)) {
        return '';
      }
      
      return listValue.map((item, index) => {
        const loopContext = {
          ...context,
          [itemVar]: item,
          loop: {
            index: index,
            first: index === 0,
            last: index === listValue.length - 1
          }
        };
        
        return this.interpolateWithContext(loopContent, loopContext);
      }).join('');
    });
    
    // Process {% if %} conditions
    const ifRegex = /{%\s*if\s+([\w.\[\]|()\s!]+)\s*%}([\s\S]*?)(?:{%\s*else\s*%}([\s\S]*?))?{%\s*endif\s*%}/g;
    
    result = result.replace(ifRegex, (match, condition, ifContent, elseContent) => {
      const conditionValue = this.evaluateExpression(condition, context);
      return conditionValue ? ifContent : (elseContent || '');
    });
    
    return result;
  }

  /**
   * Evaluate simple expressions
   */
  evaluateExpression(expr, context) {
    // Handle default expressions
    const defaultMatch = expr.match(/(\w+)\s*\|\s*default\('([^']*)'\)/);
    if (defaultMatch) {
      const value = this.getNestedValue(context, defaultMatch[1]);
      return value !== undefined ? value : defaultMatch[2];
    }
    
    // Handle simple property access
    return this.getNestedValue(context, expr.trim());
  }

  /**
   * Get nested value from object
   */
  getNestedValue(obj, path) {
    return path.split('.').reduce((current, key) => {
      return current && current[key] !== undefined ? current[key] : undefined;
    }, obj);
  }

  /**
   * Interpolate template with context
   */
  interpolateWithContext(template, context) {
    let result = template;
    
    for (const [key, value] of Object.entries(context)) {
      if (typeof value === 'object' && value !== null) {
        // Handle nested objects
        for (const [nestedKey, nestedValue] of Object.entries(value)) {
          const regex = new RegExp(`{{\\s*${key}\\.${nestedKey}\\s*}}`, 'g');
          result = result.replace(regex, String(nestedValue));
        }
      } else {
        const regex = new RegExp(`{{\\s*${key}\\s*}}`, 'g');
        result = result.replace(regex, String(value || ''));
      }
    }
    
    return result;
  }

  /**
   * Simple template interpolation
   */
  interpolate(template, context) {
    let result = template;
    
    for (const [key, value] of Object.entries(context)) {
      const regex = new RegExp(`{{\\s*${key}\\s*(?:\\|[^}]*)?\\s*}}`, 'g');
      
      const matches = result.match(regex);
      if (matches) {
        for (const match of matches) {
          const defaultMatch = match.match(/\|\s*default\('([^']*)'\)/);
          const replacement = value !== undefined ? value : (defaultMatch ? defaultMatch[1] : match);
          result = result.replace(match, replacement);
        }
      }
    }
    
    return result;
  }

  /**
   * Generate complete SHACL validation file
   */
  generateValidationFile(shapes = [], filename = 'validation.ttl') {
    const header = [
      '# KGEN SHACL Validation Shapes',
      `# Generated: ${this.options.staticBuildTime}`,
      `# Namespace: ${this.options.namespace}`,
      '',
      '@prefix sh: <http://www.w3.org/ns/shacl#> .',
      `@prefix kgen: <${this.options.namespace}> .`,
      `@prefix ex: <${this.options.baseIRI}> .`,
      '@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .',
      '@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .',
      ''
    ];
    
    const generatedShapes = shapes.map(shape => 
      this.generateShape(shape.template, shape.context)
    );
    
    const content = [
      ...header,
      ...generatedShapes.map(shape => [
        `# ${shape.description}`,
        shape.content,
        ''
      ]).flat()
    ];
    
    return {
      filename,
      content: content.join('\n'),
      shapes: generatedShapes,
      metadata: {
        shapeCount: generatedShapes.length,
        generated: this.options.staticBuildTime,
        namespace: this.options.namespace
      }
    };
  }

  /**
   * Get available shape template names
   */
  getTemplateNames() {
    return Array.from(this.shapeTemplates.keys()).sort();
  }

  /**
   * Get shape template by name
   */
  getTemplate(name) {
    return this.shapeTemplates.get(name);
  }

  /**
   * Export SHACL templates
   */
  exportTemplates(templateNames = []) {
    const templatesToExport = templateNames.length > 0 
      ? templateNames.filter(name => this.shapeTemplates.has(name))
      : Array.from(this.shapeTemplates.keys());
    
    const exported = {
      format: 'kgen-shacl-templates',
      version: '1.0.0',
      generated: this.options.staticBuildTime,
      namespace: this.options.namespace,
      baseIRI: this.options.baseIRI,
      templates: {}
    };
    
    for (const name of templatesToExport) {
      exported.templates[name] = this.shapeTemplates.get(name);
    }
    
    return exported;
  }

  /**
   * Get template statistics
   */
  getStats() {
    return {
      totalTemplates: this.shapeTemplates.size,
      namespace: this.options.namespace,
      baseIRI: this.options.baseIRI,
      templates: Array.from(this.shapeTemplates.keys()).sort()
    };
  }
}

export default KGenSHACLTemplates;