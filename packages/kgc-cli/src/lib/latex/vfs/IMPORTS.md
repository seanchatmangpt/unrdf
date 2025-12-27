# How to Import VFS Modules - Quick Reference

## For Other Agents in the Pipeline

### Agent 3 (Cache Manager)
```javascript
import { hashVfs, getVfsHashMetadata } from './vfs/index.mjs';

const cacheKey = hashVfs(vfs); // Deterministic cache key
const stats = getVfsHashMetadata(vfs); // Get hash + stats
```

### Agent 4 (Dependency Resolver)
```javascript
import { getVfsText, filterVFSByExtension } from './vfs/index.mjs';

const mainTex = getVfsText(vfs, 'work/main.tex');
const texFiles = filterVFSByExtension(vfs, ['.tex']);
```

### Agent 5+ (Compilation)
```javascript
import { packDirectory } from './vfs/index.mjs';

const vfs = await packDirectory('/path/to/project');
// Pass vfs to SwiftLaTeX or other engine
```

## Import Paths

### From within `/packages/kgc-cli/src/lib/latex/`
```javascript
import { hashVfs, packDirectory } from './vfs/index.mjs';
```

### From project root
```javascript
import { hashVfs, packDirectory } from '@unrdf/kgc-cli/lib/latex/vfs/index.mjs';
```

### Backward compatible (legacy)
```javascript
import { collectProjectFiles } from './vfs.mjs';
// collectProjectFiles is aliased to packDirectory
```

## All Exports from `vfs/index.mjs`

### Hashing Functions
- `hashFile(content: Uint8Array) → string`
- `hashVfs(vfs: Map) → string`
- `hashVfsByExtension(vfs: Map, extensions: string[]) → string`
- `areVfsEqual(vfs1: Map, vfs2: Map) → boolean`
- `getVfsHashMetadata(vfs: Map) → { hash, fileCount, totalBytes, paths }`

### Path Normalization
- `normalizePath(path: string) → string`
- `normalizeToVFS(absolutePath: string, projectRoot: string) → string`
- `vfsToRelative(vfsPath: string) → string`
- `isValidVfsPath(path: string) → boolean`
- `isValidVFSPath(path: string) → boolean`
- `isRelativePath(path: string) → boolean`
- `sortVFSPaths(paths: string[]) → string[]`

### Packing Functions
- `packDirectory(dirPath: string, options?: object) → Promise<Map>`
- `packDirectoryClean(dirPath: string, options?: object) → Promise<Map>`
- `collectProjectFiles(projectRoot: string, options?: object) → Promise<Map>`
- `listProjectFilesSorted(vfs: Map) → string[]`
- `getVFSStats(vfs: Map) → { fileCount, totalBytes, byExtension }`
- `filterVFSByExtension(vfs: Map, extensions: string[]) → Map`

### Utility Functions
- `createVfs() → Map`
- `cloneVfs(vfs: Map) → Map`
- `mergeVfs(...vfsList: Map[]) → Map`
- `getVfsText(vfs: Map, path: string) → string|null`
- `setVfsText(vfs: Map, path: string, text: string) → void`

## Quick Examples

### Pack and Hash (Most Common)
```javascript
import { packDirectory, hashVfs } from './vfs/index.mjs';

const vfs = await packDirectory('/home/user/thesis');
const cacheKey = hashVfs(vfs);
console.log(`Cache key: ${cacheKey}`);
console.log(`Files: ${vfs.size}`);
```

### Read LaTeX Source
```javascript
import { getVfsText } from './vfs/index.mjs';

const mainTex = getVfsText(vfs, 'work/main.tex');
if (mainTex) {
  console.log('Document class:', mainTex.match(/\\documentclass{([^}]+)}/)?.[1]);
}
```

### Modify VFS
```javascript
import { setVfsText } from './vfs/index.mjs';

setVfsText(vfs, 'work/generated.tex', '\\section{Auto-generated}');
```

### Filter by File Type
```javascript
import { filterVFSByExtension } from './vfs/index.mjs';

const images = filterVFSByExtension(vfs, ['.png', '.jpg']);
console.log(`Images: ${images.size}`);
```
