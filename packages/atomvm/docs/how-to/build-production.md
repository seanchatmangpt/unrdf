# How-To: Build for Production

## Problem

You need to create an optimized production build of the AtomVM browser runtime for deployment.

## Solution

Use Vite's production build to create optimized bundles with minification and tree-shaking.

## Quick Build

```bash
cd packages/atomvm
pnpm build
```

This creates optimized files in `dist/` directory.

## Step 1: Production Build

Run the build command:

```bash
pnpm build
```

**Output:**
```
vite v5.4.21 building for production...
transforming...
✓ 9 modules transformed.
rendering chunks...
computing gzip size...
dist/index.html                            5.13 kB │ gzip: 1.62 kB
dist/assets/coi-serviceworker-BWZFP-8q.js  2.18 kB │ gzip: 0.96 kB
dist/assets/main-Cu5uTnfK.js               8.93 kB │ gzip: 3.01 kB
✓ built in 71ms
```

**What's created:**
- `dist/index.html` - Production HTML
- `dist/assets/main-*.js` - Minified JavaScript bundle
- `dist/assets/coi-serviceworker-*.js` - Service worker bundle

## Step 2: Verify Build Output

Check the build output:

```bash
ls -lh dist/
ls -lh dist/assets/
```

**Expected files:**
- `index.html` (~5KB)
- `assets/main-*.js` (~9KB, minified)
- `assets/coi-serviceworker-*.js` (~2KB, minified)

## Step 3: Test Production Build

Preview the production build:

```bash
pnpm preview
```

Or serve with a simple HTTP server:

```bash
pnpm serve
# Or
python3 -m http.server 8080 --directory dist
```

**Verify:**
1. Open `http://localhost:8080` (or preview port)
2. Check service worker registers
3. Verify COI is enabled
4. Test runtime initialization

## Step 4: Optimize Build (Optional)

### Customize Vite Config

Edit `vite.config.js` for additional optimizations:

```javascript
import { defineConfig } from 'vite';

export default defineConfig({
  build: {
    outDir: 'dist',
    emptyOutDir: true,
    minify: 'terser', // Use terser for better minification
    terserOptions: {
      compress: {
        drop_console: true, // Remove console.log in production
      },
    },
    rollupOptions: {
      output: {
        manualChunks: {
          // Split vendor code
          'coi-serviceworker': ['coi-serviceworker'],
        },
      },
    },
  },
});
```

### Bundle Analysis

Analyze bundle size:

```bash
# Install bundle analyzer
pnpm add -D rollup-plugin-visualizer

# Update vite.config.js to include analyzer
# Build and check dist/stats.html
```

## Step 5: Deploy

### Static Hosting

Deploy `dist/` directory to any static host:

- **Netlify:** Drag and drop `dist/` folder
- **Vercel:** Deploy `dist/` directory
- **GitHub Pages:** Push `dist/` to `gh-pages` branch
- **AWS S3:** Upload `dist/` contents to S3 bucket

### Important Notes

1. **HTTPS Required:**
   - Service workers require HTTPS (or localhost)
   - Ensure your hosting provides HTTPS

2. **Headers:**
   - Service worker will add COOP/COEP headers
   - No need to configure server headers manually

3. **Cache Headers:**
   - Set appropriate cache headers for assets
   - Service worker handles updates automatically

### Example Deployment Script

```bash
#!/bin/bash
# deploy.sh

# Build
pnpm build

# Deploy to S3 (example)
aws s3 sync dist/ s3://your-bucket/ \
  --delete \
  --cache-control "public, max-age=31536000" \
  --exclude "index.html" \
  --exclude "service-worker.js"

# Deploy index.html with no cache
aws s3 cp dist/index.html s3://your-bucket/index.html \
  --cache-control "no-cache"
```

## Step 6: Verify Production Deployment

After deployment, verify:

1. **Service worker registers:**
   - DevTools → Application → Service Workers
   - Should see active service worker

2. **COI enabled:**
   ```javascript
   console.log('COI:', crossOriginIsolated);
   console.log('SAB:', typeof SharedArrayBuffer !== 'undefined');
   ```

3. **Assets load:**
   - Network tab shows all assets loading
   - No 404 errors

4. **Runtime works:**
   - Initialize AtomVM
   - Run example code
   - Check for errors

## Troubleshooting

### Build Fails

**Check:**
- All dependencies installed: `pnpm install`
- No syntax errors in source files
- Vite config is valid

### Assets Not Loading

**Check:**
- Base path in `vite.config.js` (if deploying to subdirectory)
- Asset paths in `index.html`
- CORS headers if loading from different origin

### Service Worker Not Working

**Check:**
- HTTPS enabled (or localhost)
- Service worker file is accessible
- No mixed content warnings

## Related Documentation

- [Tutorial: Getting Started](../tutorials/01-getting-started.md) - Learn the basics
- [How-To: Enable SharedArrayBuffer Support](./enable-sharedarraybuffer.md) - Fix COI
- [Reference: API Documentation](../reference/api.md) - Complete API


