import { defineCommand } from 'citty';
import { createWriteStream, existsSync } from 'node:fs';
import { readFile, writeFile } from 'node:fs/promises';
import { resolve, basename, dirname } from 'node:path';
import archiver from 'archiver';
import { parseConfig } from './sync/config-parser.mjs';

export const packCommand = defineCommand({
  meta: {
    name: 'pack',
    description: 'Bundle RDF assets into a certified artifact',
  },
  args: {
    config: {
      type: 'string',
      default: 'unrdf.toml',
      description: 'Path to unrdf.toml config',
    },
    output: {
      type: 'string',
      alias: 'o',
      description: 'Output path for the .unrdf bundle',
    },
  },
  async run({ args }) {
    const configPath = resolve(args.config);
    if (!existsSync(configPath)) {
      console.error(`Config file not found: ${configPath}`);
      process.exit(1);
    }

    const config = await parseConfig(configPath);
    const projectDir = dirname(configPath);
    const outputPath = args.output || resolve(projectDir, `${config.project?.name || 'artifact'}.unrdf`);
    
    const output = createWriteStream(outputPath);
    const archive = archiver('tar', {
      gzip: true,
      gzipOptions: { level: 9 }
    });

    archive.on('error', (err) => { throw err; });
    archive.pipe(output);

    const manifest = {
      pack_id: `unrdf:${config.project?.name || 'anonymous'}`,
      version: config.project?.version || '1.0.0',
      timestamp: new Date().toISOString(),
      assets: []
    };

    // Bundle ontology source
    if (config.ontology?.source) {
      const sourcePath = resolve(projectDir, config.ontology.source);
      if (existsSync(sourcePath)) {
        const assetName = `assets/${basename(sourcePath)}`;
        archive.file(sourcePath, { name: assetName });
        manifest.assets.push(assetName);

        // Include companion receipt if exists
        const receiptPath = `${sourcePath}.receipt.json`;
        if (existsSync(receiptPath)) {
          archive.file(receiptPath, { name: 'receipt.json' });
        }
      }
    }

    archive.append(JSON.stringify(manifest, null, 2), { name: 'manifest.json' });
    await archive.finalize();

    console.log(`✅ Successfully packed artifact to ${outputPath}`);
  },
});
