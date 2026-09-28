/**
 * Test Data Factory for BDD Tests
 * Provides consistent, realistic test data for all scenarios
 */

export async function createTestDataFactory() {
  const factory = {
    // React Component Data
    reactComponents: {
      simple: {
        componentName: 'UserProfile',
        title: 'User Profile Widget',
        classPrefix: 'widget',
        props: ['name', 'email', 'avatar']
      },

      complex: {
        componentName: 'DataTable',
        title: 'Advanced Data Table',
        classPrefix: 'data',
        props: [
          { name: 'data', type: 'Array<Record<string, any>>', optional: false },
          { name: 'columns', type: 'ColumnDef[]', optional: false },
          { name: 'loading', type: 'boolean', optional: true },
          { name: 'onRowClick', type: '(row: any) => void', optional: true }
        ],
        hooks: [
          { state: 'sortColumn', type: 'string | null', initial: 'null' },
          { state: 'sortDirection', type: '"asc" | "desc"', initial: '"asc"' }
        ],
        effects: [
          {
            code: 'console.log("Data updated:", data);',
            deps: ['data']
          }
        ],
        elements: [
          { tag: 'div', className: 'table-container', content: 'Table content here' },
          { tag: 'button', className: 'reload-btn', content: 'Reload Data' }
        ]
      }
    },

    // LaTeX Document Data
    latexDocuments: {
      researchPaper: {
        title: 'Advanced Template Rendering Techniques',
        authors: ['Dr. Jane Smith', 'Prof. John Doe'],
        abstract: 'This paper presents novel approaches to deterministic template rendering in code generation systems.',
        equations: [
          {
            latex: 'E = mc^2',
            description: 'Energy-mass equivalence'
          },
          {
            latex: '\\int_{-\\infty}^{\\infty} e^{-x^2} dx = \\sqrt{\\pi}',
            description: 'Gaussian integral'
          }
        ],
        results: [
          { name: 'Rendering Speed', value: '1000', unit: 'templates/sec' },
          { name: 'Memory Usage', value: '50', unit: 'MB' },
          { name: 'Accuracy', value: '99.9', unit: '%' }
        ]
      }
    },

    // API Definition Data
    apiDefinitions: {
      userAPI: {
        name: 'UserAPI',
        version: '2.1.0',
        description: 'User management API',
        endpoints: [
          { path: '/users', method: 'GET', description: 'List all users' },
          { path: '/users', method: 'POST', description: 'Create new user' },
          { path: '/users/{id}', method: 'GET', description: 'Get user by ID' },
          { path: '/users/{id}', method: 'PUT', description: 'Update user' },
          { path: '/users/{id}', method: 'DELETE', description: 'Delete user' }
        ]
      },

      productAPI: {
        name: 'ProductAPI',
        version: '1.5.2',
        description: 'Product catalog API',
        endpoints: [
          { path: '/products', method: 'GET', description: 'List products' },
          { path: '/products/search', method: 'POST', description: 'Search products' },
          { path: '/categories', method: 'GET', description: 'List categories' }
        ]
      }
    },

    // Database Schema Data
    databaseSchemas: {
      ecommerce: {
        name: 'ecommerce_db',
        tables: [
          {
            name: 'users',
            primaryKey: 'id',
            columns: [
              { name: 'id', type: 'UUID', nullable: false },
              { name: 'email', type: 'VARCHAR(255)', nullable: false, unique: true },
              { name: 'password_hash', type: 'VARCHAR(255)', nullable: false },
              { name: 'created_at', type: 'TIMESTAMP', nullable: false, default: 'NOW()' }
            ]
          },
          {
            name: 'products',
            primaryKey: 'id',
            columns: [
              { name: 'id', type: 'UUID', nullable: false },
              { name: 'name', type: 'VARCHAR(255)', nullable: false },
              { name: 'price', type: 'DECIMAL(10,2)', nullable: false },
              { name: 'category_id', type: 'UUID', nullable: true }
            ]
          }
        ]
      }
    },

    // RDF/SPARQL Test Data
    rdfData: {
      ontologyClasses: [
        {
          uri: 'http://example.org/Person',
          label: 'Person',
          comment: 'Represents a person entity',
          properties: [
            { uri: 'http://example.org/name', range: 'xsd:string' },
            { uri: 'http://example.org/age', range: 'xsd:integer' },
            { uri: 'http://example.org/email', range: 'xsd:string' }
          ]
        },
        {
          uri: 'http://example.org/Organization',
          label: 'Organization',
          comment: 'Represents an organizational entity',
          properties: [
            { uri: 'http://example.org/name', range: 'xsd:string' },
            { uri: 'http://example.org/website', range: 'xsd:anyURI' }
          ]
        }
      ],

      instances: [
        {
          uri: 'http://example.org/persons/john',
          type: 'http://example.org/Person',
          properties: {
            'http://example.org/name': 'John Doe',
            'http://example.org/age': 30,
            'http://example.org/email': 'john@example.com'
          }
        }
      ]
    },

    // SHACL Shapes Data
    shaclShapes: {
      personValidation: {
        targetClass: 'http://example.org/Person',
        properties: [
          {
            path: 'http://example.org/name',
            datatype: 'xsd:string',
            minCount: 1,
            maxCount: 1
          },
          {
            path: 'http://example.org/age',
            datatype: 'xsd:integer',
            minInclusive: 0,
            maxInclusive: 120
          },
          {
            path: 'http://example.org/email',
            datatype: 'xsd:string',
            pattern: '^[\\w\\.-]+@[\\w\\.-]+\\.[a-zA-Z]{2,}$'
          }
        ]
      }
    },

    // Performance Test Data
    performanceData: {
      largeDataset: Array.from({ length: 1000 }, (_, i) => ({
        id: `item-${i}`,
        name: `Test Item ${i}`,
        value: Math.random() * 1000,
        category: ['A', 'B', 'C'][i % 3],
        tags: [`tag-${i % 10}`, `category-${i % 5}`]
      })),

      complexNesting: {
        level1: {
          level2: {
            level3: {
              level4: {
                data: Array.from({ length: 100 }, (_, i) => ({ index: i, value: `nested-${i}` }))
              }
            }
          }
        }
      }
    },

    // Cross-platform test data
    crossPlatformData: {
      filePaths: {
        unix: ['/usr/local/bin/kgen', '/home/user/project/src/main.js'],
        windows: ['C:\\Program Files\\KGen\\bin\\kgen.exe', 'C:\\Users\\User\\Project\\src\\main.js'],
        relative: ['./src/components', '../lib/utils', '~/config/settings.json']
      },

      lineEndings: {
        unix: 'Line 1\nLine 2\nLine 3\n',
        windows: 'Line 1\r\nLine 2\r\nLine 3\r\n',
        mixed: 'Line 1\nLine 2\r\nLine 3\n'
      }
    },

    // Template configurations
    templateConfigs: {
      deterministic: {
        projectName: 'KGen Project',
        version: '1.0.0',
        author: 'DfLLSS Team',
        domain: 'example.com',
        timestamp: '2024-01-01T00:00:00Z', // Fixed for determinism
        features: ['authentication', 'caching', 'validation']
      },

      variable: {
        projectName: 'Dynamic Project',
        version: '{{ version }}',
        author: '{{ author }}',
        timestamp: '{{ now }}',
        features: '{{ features }}'
      }
    }
  };

  // Factory methods
  factory.generateLargeDataset = (size) => {
    return Array.from({ length: size }, (_, i) => ({
      id: `large-item-${i}`,
      data: `Content for item ${i}`,
      metadata: {
        index: i,
        category: `category-${i % 10}`,
        timestamp: new Date(Date.now() + i * 1000).toISOString()
      }
    }));
  };

  factory.createTemplateVariations = (baseTemplate, variations) => {
    const results = {};

    Object.entries(variations).forEach(([name, data]) => {
      results[name] = {
        template: baseTemplate,
        data: { ...factory.templateConfigs.deterministic, ...data }
      };
    });

    return results;
  };

  factory.generatePermutations = (baseData, permutationKeys) => {
    const permutations = [];

    function generateCombinations(keys, index, current) {
      if (index === keys.length) {
        permutations.push({ ...baseData, ...current });
        return;
      }

      const key = keys[index];
      const values = Array.isArray(baseData[key]) ? baseData[key] : [baseData[key]];

      values.forEach(value => {
        generateCombinations(keys, index + 1, { ...current, [key]: value });
      });
    }

    generateCombinations(permutationKeys, 0, {});
    return permutations;
  };

  return factory;
}