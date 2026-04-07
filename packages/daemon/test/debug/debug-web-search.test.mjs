#!/usr/bin/env node
/**
 * @file Debug DuckDuckGo with different approaches
 */

async function testSearch(url, headers, description) {
  console.log(`\n${description}`);
  console.log(`URL: ${url}`);

  try {
    const response = await fetch(url, { headers });

    console.log(`Status: ${response.status}`);
    console.log(`Content-Type: ${response.headers.get('content-type')}`);
    console.log(`Content-Length: ${response.headers.get('content-length') || 'unknown'}`);

    const html = await response.text();
    console.log(`HTML length: ${html.length}`);

    if (html.length > 0) {
      console.log(`\nFirst 500 chars:`);
      console.log(html.substring(0, 500));

      // Count result links
      const linkCount = (html.match(/<a/g) || []).length;
      console.log(`Total <a> tags: ${linkCount}`);
    }

    return html.length > 0;

  } catch (error) {
    console.log(`❌ Error: ${error.message}`);
    return false;
  }
}

async function main() {
  const query = 'RDF semantic web';
  const encodedQuery = encodeURIComponent(query);

  console.log('🔍 Testing different DuckDuckGo search approaches\n');

  // Test 1: HTML version with basic user agent
  await testSearch(
    `https://html.duckduckgo.com/html/?q=${encodedQuery}`,
    {
      'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36'
    },
    'Test 1: html.duckduckgo.com with realistic User-Agent'
  );

  // Test 2: Regular DuckDuckGo
  await testSearch(
    `https://duckduckgo.com/?q=${encodedQuery}`,
    {
      'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36'
    },
    'Test 2: duckduckgo.com'
  );

  // Test 3: With accept headers
  await testSearch(
    `https://html.duckduckgo.com/html/?q=${encodedQuery}`,
    {
      'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36',
      'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
      'Accept-Language': 'en-US,en;q=0.5'
    },
    'Test 3: With Accept headers'
  );
}

main().catch(console.error);
