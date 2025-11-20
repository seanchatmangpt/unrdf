# Deploying the UNRDF mdBook

This guide covers deploying the mdBook to various platforms.

## 🚀 GitHub Pages (Recommended)

**Automatic Deployment** (already configured):

1. **Enable GitHub Pages in repository settings:**
   - Go to repository Settings → Pages
   - Set Source to "GitHub Actions"
   - Save changes

2. **Deployment happens automatically:**
   - On every push to `main` that modifies `book/**`
   - Via workflow dispatch (manual trigger)
   - Takes ~2 minutes to deploy

3. **Access your book:**
   - URL: `https://seanchatmangpt.github.io/unrdf/`
   - Wait 2-3 minutes after first deployment

**Manual Deployment:**

```bash
# Build locally
cd book
mdbook build

# The book/ directory contains the static site
# Deploy book/ to any static host
```

## 📦 npm Package

Publish the mdBook as an npm package:

```bash
# Create package.json for book
cat > book/package.json <<'EOF'
{
  "name": "@unrdf/book",
  "version": "4.0.0",
  "description": "UNRDF for Enterprise Next.js - The Complete Guide",
  "main": "book/index.html",
  "files": ["book"],
  "scripts": {
    "serve": "http-server book -p 3000"
  },
  "keywords": ["rdf", "nextjs", "unrdf", "knowledge-graph", "enterprise"],
  "author": "UNRDF Team",
  "license": "MIT"
}
EOF

# Publish to npm
cd book
npm publish --access public
```

## 🌐 Vercel

Deploy to Vercel for global CDN:

```bash
# Install Vercel CLI
pnpm add -g vercel

# Deploy
cd book
vercel --prod
```

**vercel.json configuration:**

```json
{
  "outputDirectory": "book",
  "buildCommand": "mdbook build",
  "installCommand": "curl -L https://github.com/rust-lang/mdBook/releases/download/v0.4.37/mdbook-v0.4.37-x86_64-unknown-linux-gnu.tar.gz | tar xz"
}
```

## 🔥 Netlify

Deploy to Netlify:

```bash
# Install Netlify CLI
pnpm add -g netlify-cli

# Deploy
cd book
netlify deploy --prod --dir=book
```

**netlify.toml configuration:**

```toml
[build]
  command = "mdbook build"
  publish = "book"

[build.environment]
  MDBOOK_VERSION = "0.4.37"

[[plugins]]
  package = "@netlify/plugin-mdbook"
```

## 📊 Deployment Status

Check deployment status:

- **GitHub Pages**: Check Actions tab in repository
- **npm**: `npm view @unrdf/book`
- **Vercel**: Visit Vercel dashboard
- **Netlify**: Visit Netlify dashboard

## 🔧 Troubleshooting

**mdBook not found:**
```bash
cargo install mdbook
```

**GitHub Pages 404:**
- Ensure `.nojekyll` file exists in `book/` directory
- Check GitHub Pages source is set to "GitHub Actions"
- Wait 2-3 minutes for DNS propagation

**Build failures:**
- Verify all markdown files are valid
- Check SUMMARY.md references existing files
- Run `mdbook build` locally to test

## 📝 Update Workflow

1. Edit markdown files in `book/src/`
2. Test locally: `mdbook serve`
3. Commit and push to main
4. GitHub Actions deploys automatically

---

**Live URL after deployment:**
- GitHub Pages: https://seanchatmangpt.github.io/unrdf/
- Custom domain: Configure in repository settings

Built with ❤️ using mdBook v0.4.37
