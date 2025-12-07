# Lint Rule Suggestions

## Magic Number Detection

### Problem

Magic numbers (hardcoded numeric literals) reduce code clarity and maintainability. They should be extracted to named constants.

### Suggested Rule

Add ESLint rule to detect magic numbers in timeout/delay contexts:

```javascript
// eslint.config.mjs
import noMagicNumbers from 'eslint-plugin-no-magic-numbers';

export default [
  {
    plugins: {
      'no-magic-numbers': noMagicNumbers
    },
    rules: {
      'no-magic-numbers/no-magic-numbers': [
        'warn',
        {
          ignore: [
            -1, 0, 1, 2, // Common simple values
            Math.PI,
            Math.E
          ],
          ignoreArrayIndexes: true,
          ignoreDefaultValues: true,
          detectObjects: false,
          enforceConst: true,
          ignoreNumericLiteralTypes: false,
          ignoreReadonlyClassProperties: false,
          ignoreTypeIndexes: false
        }
      ]
    }
  }
];
```

### Alternative: Custom Rule

For more specific detection of timeout/delay magic numbers:

```javascript
// Custom rule: detect magic numbers in setTimeout/setInterval
const timeoutMagicNumberPattern = /setTimeout|setInterval/;

module.exports = {
  meta: {
    type: 'suggestion',
    docs: {
      description: 'Disallow magic numbers in timeout/delay contexts',
      category: 'Best Practices',
      recommended: true
    },
    fixable: null,
    schema: []
  },
  create(context) {
    return {
      CallExpression(node) {
        if (
          node.callee.name === 'setTimeout' ||
          node.callee.name === 'setInterval'
        ) {
          const delayArg = node.arguments[1];
          if (
            delayArg &&
            delayArg.type === 'Literal' &&
            typeof delayArg.value === 'number' &&
            delayArg.value > 10 // Only flag numbers > 10 (small values are often OK)
          ) {
            context.report({
              node: delayArg,
              message: 'Magic number in timeout/delay. Extract to named constant (e.g., TIMEOUT_MS).'
            });
          }
        }
      }
    };
  }
};
```

### Implementation Notes

1. **Start with warnings**: Use `'warn'` instead of `'error'` initially to allow gradual migration
2. **Ignore common values**: Allow `0`, `1`, `-1` which are often legitimate
3. **Focus on timeouts**: Prioritize detecting magic numbers in `setTimeout`/`setInterval` calls
4. **Gradual adoption**: Apply rule to new code first, then gradually fix existing code

### Benefits

- **Automated detection**: Catches magic numbers during development
- **Consistent enforcement**: Same rule applied across all files
- **Early feedback**: Developers see warnings before committing
- **Documentation**: Rule name and message explain the pattern

### Related

- See [Coding Patterns](./coding-patterns.md#named-constants-pattern) for the pattern
- See [Code Review Checklist](./code-review-checklist.md#magic-numbers) for manual checks

