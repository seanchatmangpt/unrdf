#!/usr/bin/env node
/**
 * @file Parse DuckDuckGo HTML results correctly
 */

async function fetchDuckDuckGoResults(query) {
  const searchUrl = `https://html.duckduckgo.com/html/?q=${encodeURIComponent(query)}`;

  const response = await fetch(searchUrl, {
    headers: {
      'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36',
      'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
    }
  });

  if (!response.ok) {
    throw new Error(`HTTP ${response.status}`);
  }

  return await response.text();
}

async function main() {
  const query = 'RDF semantic web';
  console.log(`🔍 Searching: ${query}\n`);

  const html = await fetchDuckDuckGoResults(query);

  // Look for all anchor tags with their surrounding context
  const lines = html.split('\n');
  const results = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Look for result links (they usually have specific patterns)
    if (line.includes('result__url') || line.includes('result__a') ||
        (line.includes('<a ') && (line.includes('http') && line.includes('class=')))) {

      // Extract URL from href
      const urlMatch = line.match(/href="([^"]+)"/);
      if (urlMatch && urlMatch[1] && !urlMatch[1].includes('duckduckgo')) {
        let url = urlMatch[1];

        // Clean DuckDuckGo redirect URLs
        url = url.replace(/^\/l\/\?uddg=/, '');

        if (url.startsWith('http')) {
          results.push({
            url,
            line: line.trim().substring(0, 200)
          });
        }
      }
    }
  }

  console.log(`Found ${results.length} potential results:\n`);

  results.slice(0, 10).forEach((result, i) => {
    console.log(`${i + 1}. ${result.url}`);
  });

  // Alternative: Look for regular result patterns
  console.log('\n' + '='.repeat(80));
  console.log('Looking for all external HTTP/HTTPS URLs:');
  console.log('='.repeat(80));

  const urlRegex = /https?:\/\/(?!duckduckgo)[a-zA-Z0-9][^\s<>"']+/g;
  const allUrls = [];
  let match;

  while ((match = urlRegex.exec(html)) !== null) {
    const url = match[0];
    if (!allUrls.includes(url) && url.length > 10 && url.length < 200) {
      allUrls.push(url);
    }
  }

  console.log(`Found ${allUrls.length} unique URLs`);
  console.log('\nFirst 20:');
  allUrls.slice(0, 20).forEach((url, i) => {
    console.log(`  ${i + 1}. ${url}`);
  });
}

main().catch(console.error);
