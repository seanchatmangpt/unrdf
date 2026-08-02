/** Documentation proof generation bound to actual file content. */
import { createHash } from 'node:crypto';
import { readdir, readFile, stat } from 'node:fs/promises';
import { resolve, relative, join } from 'node:path';
import { generateReceipt } from '@unrdf/kgc-runtime';

async function walk(root, current = root) {
  const entries = await readdir(current, { withFileTypes: true });
  const files = [];
  for (const entry of entries.sort((a, b) => a.name.localeCompare(b.name))) {
    const path = join(current, entry.name);
    if (entry.isDirectory()) files.push(...await walk(root, path));
    else if (entry.isFile()) files.push(path);
  }
  return files;
}

export async function proveDocs(docsPath = 'docs') {
  const root = resolve(docsPath);
  const rootStat = await stat(root);
  if (!rootStat.isDirectory()) throw new Error(`DOCS_PATH_NOT_DIRECTORY:${root}`);
  const files = await walk(root);
  const manifest = [];
  for (const file of files) {
    const content = await readFile(file);
    manifest.push({
      path: relative(root, file).replaceAll('\\', '/'),
      bytes: content.length,
      sha256: createHash('sha256').update(content).digest('hex'),
    });
  }
  const proof = createHash('sha256').update(JSON.stringify(manifest)).digest('hex');
  const output = {
    proof,
    algorithm: 'sha256',
    fileCount: manifest.length,
    totalBytes: manifest.reduce((total, file) => total + file.bytes, 0),
    manifest,
  };
  const receipt = await generateReceipt('prove-docs', { docsPath: root }, output);
  return { ...output, receipt };
}
