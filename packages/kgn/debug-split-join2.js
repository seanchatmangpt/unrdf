import { TemplateEngine } from './src/engine/template-engine.js';

async function debugSplitJoin() {
  const engine = new TemplateEngine({ deterministicMode: true });

  // Test various filter syntaxes
  const tests = [
    { template: '{{ text | split(" ") | join("-") }}', context: { text: 'hello world kgen' }, desc: 'Parentheses syntax' },
    { template: '{{ text | split: " " | join: "-" }}', context: { text: 'hello world kgen' }, desc: 'Colon syntax' },
    { template: '{{ text | split }} ', context: { text: 'hello world kgen' }, desc: 'Default split' },
    { template: '{{ text | upper }}', context: { text: 'hello world kgen' }, desc: 'Simple upper filter' }
  ];

  for (const test of tests) {
    console.log(`\n=== ${test.desc} ===`);
    console.log('Template:', test.template);

    try {
      const result = await engine.renderString(test.template, test.context);
      if (result.success) {
        console.log('Success:', JSON.stringify(result.content));
      } else {
        console.log('Error:', result.error);
      }
    } catch (error) {
      console.error('Exception:', error.message);
    }
  }
}

debugSplitJoin();