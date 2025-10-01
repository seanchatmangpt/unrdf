import unjs from "eslint-config-unjs";

export default unjs({
  ignores: [
    // ignore paths
  ],
  rules: {
    // rule overrides
  },
  markdown: {
    rules: {
      // markdown rule overrides
    },
  },
  plugins: {
    jsdoc: (await import('eslint-plugin-jsdoc')).default
  },
  rules: {
    // JSDoc enforcement rules
    'jsdoc/require-jsdoc': ['error', { 
      publicOnly: true,
      require: {
        FunctionDeclaration: true,
        MethodDefinition: true,
        ClassDeclaration: true,
        ArrowFunctionExpression: false,
        FunctionExpression: false
      }
    }],
    'jsdoc/require-param-type': 'off',
    'jsdoc/require-returns-type': 'off',
    'jsdoc/check-types': 'off',
    'jsdoc/valid-types': 'off'
  }
});
