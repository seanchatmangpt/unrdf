module.exports = {
  extends: ['../../.eslintrc.cjs'],
  parserOptions: {
    ecmaVersion: 2022,
    sourceType: 'module',
  },
  rules: {
    'no-console': 'off', // Allow console in demo/examples
  },
};
