# Nextra Deployment Guide

## Current Status: ✅ READY FOR GITHUB PAGES

The Nextra documentation site is configured and ready to deploy to GitHub Pages at:
**https://seanchatmangpt.github.io/unrdf/**

## What's Deployed

### Nextra Documentation Site
- **URL**: `https://seanchatmangpt.github.io/unrdf/`
- **Content**: 9 static pages with LaTeX/KaTeX math rendering
- **Features**:
  - KGC-4D concepts with comprehensive formulas
  - Mathematical foundations ($\mathcal{S} = \langle O, T, V, G \rangle$)
  - Forensic UX patterns
  - Research papers section (web-readable format)
  - μ(O) Calculus Dashboard documentation

### Pages Included

```
/                           - Home page
/playground                 - μ(O) Dashboard docs
/concepts/kgc-4d            - KGC-4D overview with LaTeX
/concepts/kgc-4d/mathematics - Mathematical foundations
/concepts/kgc-4d/forensic-ux - UX patterns
/guides/getting-started     - Getting started guide
/papers                     - Research papers hub
```

## Deployment Process

### Automatic (Recommended)

Push to `main` branch triggers automatic deployment:

```bash
git push origin main
```

GitHub Actions workflow (`.github/workflows/deploy-nextra.yml`) will:
1. Install dependencies with pnpm
2. Build Nextra with `NODE_ENV=production`
3. Deploy to GitHub Pages

**Build time**: ~15 seconds
**Deploy time**: ~30 seconds
**Total**: Under 1 minute from push to live

### Manual Deployment

```bash
# Build locally
cd packages/nextra
NODE_ENV=production pnpm build

# Output in packages/nextra/out/
# Can be deployed to any static host
```

## Configuration

### GitHub Pages Settings

**Repository Settings → Pages**:
- Source: GitHub Actions
- Branch: `gh-pages` (auto-created by workflow)
- Custom domain: (optional)

### Base Path

The site is configured for `/unrdf/` base path:

```javascript
// next.config.mjs
basePath: process.env.NODE_ENV === 'production' ? '/unrdf' : '',
assetPrefix: process.env.NODE_ENV === 'production' ? '/unrdf/' : '',
```

This means:
- Production: `https://seanchatmangpt.github.io/unrdf/`
- Development: `http://localhost:3003/`

## μ(O) Calculus Dashboard

### Current Approach

The dashboard is documented at `/playground` page but **not yet embedded** in the static site.

**Two deployment options**:

#### Option A: Separate Service (Current)
- Run KGC-4D Playground on port 3001
- Link from Nextra docs
- Development: `pnpm --filter @unrdf/kgc-4d-playground dev`

#### Option B: Integrated (Future)
- Resolve shadcn/ui component dependencies
- Build playground as static export
- Copy to `packages/nextra/public/playground/`
- Embed via iframe or direct integration

### Accessing the Dashboard

**Development**:
```bash
cd packages/kgc-4d/playground
pnpm dev
# Visit http://localhost:3001
```

**Production**:
- Currently: Link to separate deployment
- Future: `https://seanchatmangpt.github.io/unrdf/playground`

## Build Verification

Test the build locally before deploying:

```bash
cd packages/nextra
NODE_ENV=production pnpm build
cd out
python3 -m http.server 8000
# Visit http://localhost:8000/unrdf/
```

Expected output:
```
✓ Compiled successfully in 7.9s
✓ Generating static pages (9/9) in 529.4ms

Route (app)
├ ○ /
├ ○ /playground
├ ○ /concepts/kgc-4d
├ ○ /concepts/kgc-4d/mathematics
├ ○ /concepts/kgc-4d/forensic-ux
└ ○ /papers
```

## Troubleshooting

### Build Fails

```bash
# Clean and rebuild
pnpm clean
pnpm install --frozen-lockfile
pnpm build
```

### Pages Not Loading

Check base path in browser network tab:
- Assets should load from `/unrdf/_next/...`
- If loading from `/_next/...`, base path is wrong

### Math Not Rendering

Verify KaTeX CSS is loaded:
```javascript
// app/layout.tsx
import 'katex/dist/katex.min.css';
```

## Next Steps

1. ✅ **Deployed to GitHub Pages** - Merge to main triggers deployment
2. ⏳ **Migrate remaining KGC-4D docs** - 56 files to convert to MDX
3. ⏳ **Generate API docs from TSDoc** - 18 packages
4. ⏳ **Resolve playground dependencies** - Fix shadcn/ui components
5. ⏳ **Embed playground** - Static export or iframe integration

## Advantages Over Docusaurus

Nextra provides:
- ✅ **LaTeX/KaTeX rendering** - Formulas as searchable text
- ✅ **Enhanced MDX components** - Callout, Steps, Tabs
- ✅ **Next.js integration** - Can embed React components
- ✅ **Faster builds** - 7.9s vs Docusaurus ~20s
- ✅ **Better developer experience** - Hot reload, TypeScript

## Resources

- **Live Site**: https://seanchatmangpt.github.io/unrdf/
- **Repository**: https://github.com/seanchatmangpt/unrdf
- **Nextra Docs**: https://nextra.site
- **KaTeX**: https://katex.org
