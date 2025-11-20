// scripts/debug-pages.mjs
import { spawnSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';

/**
 * Run a command synchronously and return { code, stdout, stderr }.
 * @param {string} cmd
 * @param {string[]} args
 * @param {{cwd?: string, env?: Record<string,string>}} [opts]
 */
function run(cmd, args, opts = {}) {
  const res = spawnSync(cmd, args, {
    stdio: ['ignore', 'pipe', 'pipe'],
    encoding: 'utf8',
    ...opts
  });
  return { code: res.status ?? 0, stdout: res.stdout || '', stderr: res.stderr || '' };
}

function logSection(title) {
  console.log(`\n=== ${title} ===`);
}

function fail(message) {
  console.error(`\n✖ ${message}`);
  process.exitCode = 1;
}

function ok(message) {
  console.log(`✔ ${message}`);
}

// Config
const BOOK_DIR = resolve(process.cwd(), 'book');
const OUTPUT_DIR = resolve(process.cwd(), '_site');

(async () => {
  logSection('Environment checks');
  // Ensure cargo bin is on PATH for mdbook
  process.env.PATH = `${process.env.HOME}/.cargo/bin:${process.env.PATH}`;

  const mdv = run('mdbook', ['--version']);
  if (mdv.code !== 0) {
    console.log(mdv.stdout.trim());
    console.error(mdv.stderr.trim());
    return fail('mdbook not found. Install with: cargo install mdbook');
  }
  ok(`mdBook detected: ${mdv.stdout.trim()}`);

  // Validate book structure
  logSection('Book structure');
  const summaryPath = resolve(BOOK_DIR, 'src', 'SUMMARY.md');
  if (!existsSync(summaryPath)) {
    return fail(`Missing SUMMARY.md at ${summaryPath}`);
  }
  ok('SUMMARY.md found');

  const bookTomlPath = resolve(BOOK_DIR, 'book.toml');
  if (!existsSync(bookTomlPath)) {
    return fail(`Missing book.toml at ${bookTomlPath}`);
  }
  ok('book.toml found');

  // Inspect site-url in book.toml
  const bookToml = readFileSync(bookTomlPath, 'utf8');
  const siteUrlMatch = bookToml.match(/site-url\s*=\s*"([^"]*)"/);
  if (siteUrlMatch) {
    console.log(`site-url: ${siteUrlMatch[1]}`);
  } else {
    console.log('site-url: (not set)');
  }

  // Build
  logSection('Build');
  const build = run('mdbook', ['build', 'book', '-d', OUTPUT_DIR]);
  process.stdout.write(build.stdout);
  process.stderr.write(build.stderr);
  if (build.code !== 0) {
    return fail('mdbook build failed');
  }
  ok(`Built to ${OUTPUT_DIR}`);

  // Verify output paths
  logSection('Output verification');
  const indexPath = resolve(OUTPUT_DIR, 'index.html');
  if (!existsSync(indexPath)) {
    return fail(`index.html not found at ${indexPath}`);
  }
  ok('index.html exists');

  // Common mistaken paths (helpful hints)
  const oldPath = resolve(process.cwd(), 'book', 'book', 'html', 'index.html');
  if (existsSync(oldPath)) {
    console.log(`Note: legacy path exists: ${oldPath}`);
  }

  // Print small tree (top-level only)
  logSection('Output listing');
  const ls = run('ls', ['-la', OUTPUT_DIR]);
  process.stdout.write(ls.stdout);

  logSection('Conclusion');
  ok('Local build OK. In CI, upload _site with actions/upload-pages-artifact and deploy-pages.');
})();

