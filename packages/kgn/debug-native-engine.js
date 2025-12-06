import { KGenTemplateEngine } from './src/core/kgen-engine.js';

async function debugNativeEngine() {
  const engine = new KGenTemplateEngine({
    deterministicMode: true,
    strictMode: true
  });

  // Test basic functionality
  const tests = [
    { template: '{{ text | upper }}', context: { text: 'hello' }, desc: 'upper filter' },
    { template: '{{ text | split(" ") | join("-") }}', context: { text: 'hello world' }, desc: 'split/join chain' },
    { template: '{{ missing | default("fallback") }}', context: {}, desc: 'default filter' }
  ];

  for (const test of tests) {
    console.log(`\n=== ${test.desc} ===`);
    console.log('Template:', test.template);
    console.log('Context:', test.context);

    try {
      // Test each phase
      const planResult = await engine.plan(test.template, test.context);
      console.log('Plan success:', planResult.success);
      if (!planResult.success) {
        console.log('Plan error:', planResult.error);
        continue;
      }

      const renderResult = await engine.render(planResult, test.context);
      console.log('Render success:', renderResult.success);
      if (renderResult.success) {
        console.log('Render content:', JSON.stringify(renderResult.content));
      } else {
        console.log('Render error:', renderResult.error);
        continue;
      }

      // Test full renderTemplate method
      const fullResult = await engine.renderTemplate(test.template, test.context);
      console.log('Full result:', JSON.stringify(fullResult));

    } catch (error) {
      console.error('Exception:', error.message);
    }
  }
}

debugNativeEngine();