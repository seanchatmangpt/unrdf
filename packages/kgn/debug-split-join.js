import { TemplateEngine } from './src/engine/template-engine.js';

async function debugSplitJoin() {
  const engine = new TemplateEngine({ deterministicMode: true });

  // Test our custom filters directly
  const template = '{{ text | split " " | join "-" }}';
  const context = { text: 'hello world kgen' };

  console.log('Template:', template);
  console.log('Context:', context);

  try {
    const result = await engine.renderString(template, context);
    console.log('Result:', result);

    if (result.success) {
      console.log('Content:', JSON.stringify(result.content));
    } else {
      console.log('Error:', result.error);
    }

    // Test individual filters
    console.log('\n=== Testing individual filters ===');
    const splitResult = await engine.renderString('{{ text | split " " | json }}', context);
    if (splitResult.success) {
      console.log('Split result:', splitResult.content);
    }

  } catch (error) {
    console.error('Debug error:', error);
  }
}

debugSplitJoin();