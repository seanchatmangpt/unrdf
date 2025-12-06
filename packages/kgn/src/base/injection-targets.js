/**
 * KGEN Injection Target Templates - Code injection patterns
 * 
 * Provides templates for creating injection targets in existing codebases
 * Supports marker-based, semantic-based, and position-based injection strategies
 */

export class KGenInjectionTargets {
  constructor(options = {}) {
    this.options = {
      deterministicMode: options.deterministicMode !== false,
      staticBuildTime: options.staticBuildTime || '2024-01-01T00:00:00.000Z',
      namespace: options.namespace || 'kgen',
      ...options
    };
    
    this.targetTemplates = new Map();
    this.initializeTargetTemplates();
  }

  /**
   * Initialize built-in injection target templates
   */
  initializeTargetTemplates() {
    // Marker-based injection target
    this.registerTargetTemplate('marker_target', {
      name: 'MarkerBasedTarget',
      description: 'Injection target using comment markers',
      pattern: {
        startMarker: '{{ startComment | default("// KGEN:START") }} {{ markerName }}',
        endMarker: '{{ endComment | default("// KGEN:END") }} {{ markerName }}',
        content: '{{ injectionContent }}'
      },
      template: `{{ startComment | default("// KGEN:START") }} {{ markerName }}
{{ injectionContent | indent(indentLevel | default(0)) }}
{{ endComment | default("// KGEN:END") }} {{ markerName }}`,
      detection: {
        regex: '{{ startComment | escape_regex }} {{ markerName | escape_regex }}[\\s\\S]*?{{ endComment | escape_regex }} {{ markerName | escape_regex }}',
        flags: 'g'
      }
    });

    // Function injection target
    this.registerTargetTemplate('function_target', {
      name: 'FunctionInjectionTarget', 
      description: 'Inject code at specific points in functions',
      pattern: {
        functionStart: 'function {{ functionName }}({{ parameters | default("") }}) {',
        functionEnd: '}',
        injectionPoint: '{{ injectionPoint | default("start") }}' // start, end, before-return
      },
      template: `{% if injectionPoint == "start" -%}
function {{ functionName }}({{ parameters | default("") }}) {
{{ injectionContent | indent(2) }}
{% elif injectionPoint == "end" -%}
{{ injectionContent | indent(2) }}
}
{% elif injectionPoint == "before-return" -%}
{{ injectionContent | indent(2) }}
  return
{% endif -%}`,
      detection: {
        regex: 'function\\s+{{ functionName | escape_regex }}\\s*\\([^)]*\\)\\s*{',
        flags: 'g'
      }
    });

    // Class injection target
    this.registerTargetTemplate('class_target', {
      name: 'ClassInjectionTarget',
      description: 'Inject code into class definitions',
      pattern: {
        className: '{{ className }}',
        injectionType: '{{ injectionType | default("method") }}', // method, property, constructor
        visibility: '{{ visibility | default("public") }}'
      },
      template: `{% if injectionType == "method" -%}
  {{ visibility }} {{ methodName }}({{ parameters | default("") }}) {
{{ methodBody | indent(4) }}
  }
{% elif injectionType == "property" -%}
  {{ visibility }} {{ propertyName }}{{ propertyValue ? " = " + propertyValue : "" }};
{% elif injectionType == "constructor" -%}
  constructor({{ parameters | default("") }}) {
{{ constructorBody | indent(4) }}
  }
{% endif -%}`,
      detection: {
        regex: 'class\\s+{{ className | escape_regex }}\\s*(?:extends\\s+\\w+)?\\s*{',
        flags: 'g'
      }
    });

    // Import injection target
    this.registerTargetTemplate('import_target', {
      name: 'ImportInjectionTarget',
      description: 'Inject import statements',
      pattern: {
        importType: '{{ importType | default("named") }}', // named, default, namespace
        modulePath: '{{ modulePath }}'
      },
      template: `{% if importType == "named" -%}
import { {{ importNames | join(", ") }} } from '{{ modulePath }}';
{% elif importType == "default" -%}
import {{ defaultImport }} from '{{ modulePath }}';
{% elif importType == "namespace" -%}
import * as {{ namespaceImport }} from '{{ modulePath }}';
{% elif importType == "side-effect" -%}
import '{{ modulePath }}';
{% endif -%}`,
      detection: {
        regex: '^\\s*import\\s+.*from\\s+["\']{{ modulePath | escape_regex }}["\'];?\\s*$',
        flags: 'gm'
      }
    });

    // Configuration injection target
    this.registerTargetTemplate('config_target', {
      name: 'ConfigurationTarget',
      description: 'Inject configuration values',
      pattern: {
        configFormat: '{{ configFormat | default("json") }}', // json, yaml, js, env
        configKey: '{{ configKey }}'
      },
      template: `{% if configFormat == "json" -%}
  "{{ configKey }}": {{ configValue | json }}
{% elif configFormat == "yaml" -%}
{{ configKey }}: {{ configValue | yaml }}
{% elif configFormat == "js" -%}
{{ configKey }}: {{ configValue | js_value }},
{% elif configFormat == "env" -%}
{{ configKey }}={{ configValue }}
{% endif -%}`,
      detection: {
        regex: '"{{ configKey | escape_regex }}"\\s*:',
        flags: 'g'
      }
    });

    // HTML injection target
    this.registerTargetTemplate('html_target', {
      name: 'HTMLInjectionTarget',
      description: 'Inject HTML content at specific points',
      pattern: {
        targetElement: '{{ targetElement | default("body") }}',
        position: '{{ position | default("append") }}' // append, prepend, before, after
      },
      template: `{% if position == "append" -%}
<{{ targetElement }}{{ attributes ? " " + attributes : "" }}>
{{ existingContent }}
{{ injectionContent | indent(2) }}
</{{ targetElement }}>
{% elif position == "prepend" -%}
<{{ targetElement }}{{ attributes ? " " + attributes : "" }}>
{{ injectionContent | indent(2) }}
{{ existingContent }}
</{{ targetElement }}>
{% elif position == "before" -%}
{{ injectionContent }}
<{{ targetElement }}{{ attributes ? " " + attributes : "" }}>{{ existingContent }}</{{ targetElement }}>
{% elif position == "after" -%}
<{{ targetElement }}{{ attributes ? " " + attributes : "" }}>{{ existingContent }}</{{ targetElement }}>
{{ injectionContent }}
{% endif -%}`,
      detection: {
        regex: '<{{ targetElement | escape_regex }}(?:\\s[^>]*)?>',
        flags: 'gi'
      }
    });

    // CSS injection target
    this.registerTargetTemplate('css_target', {
      name: 'CSSInjectionTarget',
      description: 'Inject CSS rules and selectors',
      pattern: {
        selector: '{{ cssSelector }}',
        injectionType: '{{ injectionType | default("rule") }}' // rule, property, media
      },
      template: `{% if injectionType == "rule" -%}
{{ cssSelector }} {
{{ cssProperties | indent(2) }}
}
{% elif injectionType == "property" -%}
  {{ propertyName }}: {{ propertyValue }};
{% elif injectionType == "media" -%}
@media {{ mediaQuery }} {
{{ mediaRules | indent(2) }}
}
{% endif -%}`,
      detection: {
        regex: '{{ cssSelector | escape_regex }}\\s*{',
        flags: 'g'
      }
    });

    // JSON injection target
    this.registerTargetTemplate('json_target', {
      name: 'JSONInjectionTarget',
      description: 'Inject properties into JSON objects',
      pattern: {
        targetPath: '{{ jsonPath }}',
        operation: '{{ operation | default("merge") }}' // merge, replace, append
      },
      template: `{% if operation == "merge" -%}
{{ jsonKey }}: {{ jsonValue | json }}
{% elif operation == "replace" -%}
{{ jsonValue | json }}
{% elif operation == "append" and jsonValue is iterable -%}
{{ existingValue | concat(jsonValue) | json }}
{% endif -%}`,
      detection: {
        regex: '"{{ jsonKey | escape_regex }}"\\s*:\\s*',
        flags: 'g'
      }
    });

    // Package.json target
    this.registerTargetTemplate('package_json_target', {
      name: 'PackageJSONTarget',
      description: 'Inject dependencies and scripts into package.json',
      pattern: {
        section: '{{ section | default("dependencies") }}', // dependencies, devDependencies, scripts, etc.
        operation: '{{ operation | default("add") }}' // add, update, remove
      },
      template: `{% if section == "dependencies" or section == "devDependencies" -%}
    "{{ packageName }}": "{{ packageVersion }}"
{% elif section == "scripts" -%}
    "{{ scriptName }}": "{{ scriptCommand }}"
{% elif section == "keywords" -%}
    "{{ keyword }}"
{% elif section == "exports" -%}
    "{{ exportPath }}": "{{ exportTarget }}"
{% endif -%}`,
      detection: {
        regex: '"{{ section | escape_regex }}"\\s*:\\s*{',
        flags: 'g'
      }
    });

    // Docker injection target
    this.registerTargetTemplate('dockerfile_target', {
      name: 'DockerfileTarget',
      description: 'Inject instructions into Dockerfile',
      pattern: {
        instruction: '{{ instruction | upper }}',
        position: '{{ position | default("append") }}' // append, prepend, after-instruction
      },
      template: `{{ instruction | upper }} {{ instructionArgs }}`,
      detection: {
        regex: '^{{ instruction | upper | escape_regex }}\\s+',
        flags: 'gm'
      }
    });

    // YAML injection target
    this.registerTargetTemplate('yaml_target', {
      name: 'YAMLInjectionTarget',
      description: 'Inject properties into YAML files',
      pattern: {
        yamlPath: '{{ yamlPath }}',
        indentLevel: '{{ indentLevel | default(0) }}'
      },
      template: `{{ ' '.repeat(indentLevel * 2) }}{{ yamlKey }}: {{ yamlValue | yaml_value }}`,
      detection: {
        regex: '^\\s*{{ yamlKey | escape_regex }}\\s*:',
        flags: 'gm'
      }
    });
  }

  /**
   * Register an injection target template
   */
  registerTargetTemplate(name, template) {
    this.targetTemplates.set(name, {
      ...template,
      registered: this.options.staticBuildTime,
      namespace: this.options.namespace
    });
  }

  /**
   * Generate injection target from template
   */
  generateTarget(templateName, context = {}) {
    const template = this.targetTemplates.get(templateName);
    if (!template) {
      throw new Error(`Injection target template '${templateName}' not found`);
    }
    
    // Process template with context
    let targetContent = template.template;
    
    // Replace template variables
    for (const [key, value] of Object.entries(context)) {
      const regex = new RegExp(`{{\\s*${key}\\s*(?:\\|[^}]*)?\\s*}}`, 'g');
      
      const matches = targetContent.match(regex);
      if (matches) {
        for (const match of matches) {
          const defaultMatch = match.match(/\|\s*default\('([^']*)'\)/);
          const replacement = value !== undefined ? value : (defaultMatch ? defaultMatch[1] : match);
          targetContent = targetContent.replace(match, String(replacement));
        }
      }
    }
    
    // Process template logic
    targetContent = this.processTemplateLogic(targetContent, context);
    
    // Generate detection pattern
    const detectionPattern = this.generateDetectionPattern(template.detection, context);
    
    return {
      name: this.interpolate(template.name, context),
      description: this.interpolate(template.description, context),
      content: targetContent,
      pattern: template.pattern ? this.processPattern(template.pattern, context) : null,
      detection: detectionPattern,
      metadata: {
        template: templateName,
        generated: this.options.staticBuildTime,
        namespace: this.options.namespace,
        context: Object.keys(context)
      }
    };
  }

  /**
   * Generate detection pattern for finding injection points
   */
  generateDetectionPattern(detection, context) {
    if (!detection) return null;
    
    let regex = detection.regex;
    
    // Replace template variables in regex
    for (const [key, value] of Object.entries(context)) {
      const regexVar = new RegExp(`{{\\s*${key}\\s*\\|\\s*escape_regex\\s*}}`, 'g');
      regex = regex.replace(regexVar, this.escapeRegex(String(value || '')));
      
      const simpleVar = new RegExp(`{{\\s*${key}\\s*}}`, 'g');
      regex = regex.replace(simpleVar, String(value || ''));
    }
    
    return {
      regex: new RegExp(regex, detection.flags || 'g'),
      source: regex,
      flags: detection.flags || 'g'
    };
  }

  /**
   * Process pattern object with context
   */
  processPattern(pattern, context) {
    const processed = {};
    
    for (const [key, value] of Object.entries(pattern)) {
      if (typeof value === 'string') {
        processed[key] = this.interpolate(value, context);
      } else {
        processed[key] = value;
      }
    }
    
    return processed;
  }

  /**
   * Process template logic (loops and conditions)
   */
  processTemplateLogic(content, context) {
    let result = content;
    
    // Process {% if %} conditions
    const ifRegex = /{%\s*if\s+([\w.\[\]|()\s!="']+)\s*%}([\s\S]*?)(?:{%\s*elif\s+([\w.\[\]|()\s!="']+)\s*%}([\s\S]*?))*(?:{%\s*else\s*%}([\s\S]*?))?{%\s*endif\s*%}/g;
    
    result = result.replace(ifRegex, (match, condition, ifContent, elifCondition, elifContent, elseContent) => {
      const conditionValue = this.evaluateCondition(condition, context);
      if (conditionValue) {
        return ifContent;
      } else if (elifCondition && this.evaluateCondition(elifCondition, context)) {
        return elifContent;
      } else {
        return elseContent || '';
      }
    });
    
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
    
    return result;
  }

  /**
   * Evaluate conditional expressions
   */
  evaluateCondition(condition, context) {
    // Handle simple conditions
    const trimmed = condition.trim();
    
    // Handle == comparisons
    const eqMatch = trimmed.match(/(\w+)\s*==\s*"([^"]*)"/); 
    if (eqMatch) {
      const value = this.getNestedValue(context, eqMatch[1]);
      return value === eqMatch[2];
    }
    
    // Handle simple property access
    const value = this.evaluateExpression(trimmed, context);
    return Boolean(value);
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
   * Escape string for regex
   */
  escapeRegex(string) {
    return string.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  }

  /**
   * Generate injection operation specification
   */
  generateInjectionSpec(targetName, context = {}, options = {}) {
    const target = this.generateTarget(targetName, context);
    
    return {
      target: target.name,
      operation: options.operation || 'inject',
      content: target.content,
      detection: target.detection,
      position: options.position || 'replace',
      backup: options.backup !== false,
      validate: options.validate !== false,
      metadata: {
        ...target.metadata,
        operation: options.operation || 'inject',
        position: options.position || 'replace'
      }
    };
  }

  /**
   * Get available target template names
   */
  getTemplateNames() {
    return Array.from(this.targetTemplates.keys()).sort();
  }

  /**
   * Get target template by name
   */
  getTemplate(name) {
    return this.targetTemplates.get(name);
  }

  /**
   * Export injection target templates
   */
  exportTemplates(templateNames = []) {
    const templatesToExport = templateNames.length > 0 
      ? templateNames.filter(name => this.targetTemplates.has(name))
      : Array.from(this.targetTemplates.keys());
    
    const exported = {
      format: 'kgen-injection-targets',
      version: '1.0.0',
      generated: this.options.staticBuildTime,
      namespace: this.options.namespace,
      templates: {}
    };
    
    for (const name of templatesToExport) {
      exported.templates[name] = this.targetTemplates.get(name);
    }
    
    return exported;
  }

  /**
   * Get template statistics
   */
  getStats() {
    return {
      totalTemplates: this.targetTemplates.size,
      namespace: this.options.namespace,
      deterministicMode: this.options.deterministicMode,
      templates: Array.from(this.targetTemplates.keys()).sort()
    };
  }
}

export default KGenInjectionTargets;