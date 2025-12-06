# UNRDF v5 Video Scripts

Professional video scripts for UNRDF v5 tutorial and marketing content.

---

## Overview

This directory contains complete, production-ready video scripts for creating UNRDF v5 educational content. Each script includes:

- **Detailed narration** (word-for-word)
- **Screen directions** (what to show when)
- **Code examples** (syntax-highlighted, tested)
- **Timing markers** (for editing/pacing)
- **Production notes** (B-roll, graphics, animations)
- **Downloadable resources** (code samples, cheat sheets)

---

## Videos

### 1. Migration Guide (8-10 minutes)
**File:** [`01-migration-guide.md`](./01-migration-guide.md)
**Audience:** v4.x users upgrading to v5
**Difficulty:** Intermediate

**Topics:**
- Why upgrade to v5?
- Breaking changes overview
- Automated migration tool usage
- Manual migration steps
- Testing & verification
- Common issues & troubleshooting

**Key Takeaways:**
- Learn how to migrate from N3.js to Oxigraph
- Use automated migration tool (90% automation)
- Update DataFactory and streaming imports
- Replace autonomic CLI command with programmatic API

---

### 2. What's New in v5 (12-15 minutes)
**File:** [`02-whats-new-v5.md`](./02-whats-new-v5.md)
**Audience:** New and existing users
**Difficulty:** Beginner to Intermediate

**Topics:**
- Oxigraph Integration (40% faster queries)
- Zero-copy Architecture (60% memory reduction)
- Synchronous SPARQL API
- OpenTelemetry Observability
- Enhanced CLI Tools
- Knowledge Hooks API
- Production Readiness (FMEA)
- Comprehensive Documentation

**Key Takeaways:**
- Understand major performance improvements
- Learn new synchronous API patterns
- Explore production monitoring with OTEL
- Discover Knowledge Hooks for validation

---

### 3. Getting Started (10-12 minutes)
**File:** [`03-getting-started.md`](./03-getting-started.md)
**Audience:** New users, beginners
**Difficulty:** Beginner

**Topics:**
- Installation (Node.js 18+, npm/pnpm)
- Creating your first RDF store
- Adding triples (subjects, predicates, objects)
- Querying with SPARQL
- Saving/loading Turtle files
- Using the CLI tools

**Key Takeaways:**
- Build a contacts knowledge graph from scratch
- Learn core RDF concepts (triples, quads, graphs)
- Write basic SPARQL queries
- Use CLI for quick operations

**Project:** Build a personal contacts knowledge graph with names, emails, relationships, and SPARQL queries.

---

### 4. Performance Benchmarks (15-18 minutes)
**File:** [`04-performance-benchmarks.md`](./04-performance-benchmarks.md)
**Audience:** Intermediate to advanced users
**Difficulty:** Intermediate

**Topics:**
- Benchmark methodology
- Query performance analysis
- Memory usage deep dive
- Load/save performance
- Hook system overhead
- Scaling characteristics
- Comparison with other libraries
- Production sizing recommendations

**Key Takeaways:**
- Verify 40% query performance improvement
- Understand 60% memory reduction mechanics
- Learn hook overhead characteristics
- Size production deployments correctly

**Evidence:**
- 100+ iterations per test
- 5 dataset sizes (1K → 10M triples)
- Real-world query patterns
- Production deployment guidance

---

## Production Workflow

### Pre-Production

1. **Script Review**
   - [ ] Technical accuracy verified
   - [ ] Code examples tested
   - [ ] Timing validated
   - [ ] Transitions polished

2. **Resource Preparation**
   - [ ] Code samples extracted
   - [ ] Test datasets created
   - [ ] Graphics designed
   - [ ] B-roll list finalized

3. **Environment Setup**
   - [ ] VSCode configured (theme, font size)
   - [ ] Terminal configured (colors, prompt)
   - [ ] Screen recording software tested
   - [ ] Microphone levels checked

### Production

1. **Recording Sessions**
   - Record in segments (follow timing markers)
   - Capture screen + audio simultaneously
   - Use live coding (not pasted code)
   - Show natural workflow (typos OK)

2. **Code Demos**
   - Use syntax highlighting
   - Show autocomplete/IntelliSense
   - Display terminal output
   - Highlight key changes

3. **Visual Elements**
   - Title cards at section breaks
   - Animated graphs/charts
   - Code comparison split-screens
   - Overlay checklists/summaries

### Post-Production

1. **Editing**
   - Cut dead air (keep natural pauses)
   - Add chapter markers at timestamps
   - Sync B-roll with narration
   - Color grade for consistency

2. **Graphics & Animation**
   - Add title overlays
   - Animate performance charts
   - Highlight code changes
   - Create lower-thirds for resources

3. **Audio**
   - Normalize volume levels
   - Remove background noise
   - Add music (intro/outro only)
   - Sync voiceover timing

4. **Export**
   - 1080p or 4K resolution
   - 60fps for screen recordings
   - H.264 codec (YouTube optimal)
   - Include closed captions (SRT)

---

## Style Guide

### Visual Style

**Screen Layout:**
- 16:9 aspect ratio
- VSCode: 60% screen width (left)
- Terminal: 40% screen width (right)
- Font size: 16-18pt (readable on mobile)

**Color Palette:**
- Background: Dark theme (#1e1e1e)
- Accent: UNRDF blue (#0066cc)
- Success: Green (#22c55e)
- Warning: Yellow (#f59e0b)
- Error: Red (#ef4444)

**Typography:**
- Code: JetBrains Mono, 16pt
- UI: Inter, 14-16pt
- Titles: Inter Bold, 24-32pt

### Narration Style

**Voice:**
- Conversational, friendly
- Clear pronunciation
- Moderate pace (140-160 wpm)
- Enthusiasm without overselling

**Tone:**
- Educational, not preachy
- Precise, not verbose
- Encouraging, not condescending
- Professional, not stuffy

**Language:**
- Active voice ("We'll create..." not "A store will be created...")
- Present tense ("This query returns..." not "This query will return...")
- Inclusive pronouns ("Let's..." not "You should...")
- Avoid jargon (or explain when necessary)

### Code Style

**Formatting:**
- Prettier with default settings
- 2-space indentation
- Semicolons required
- ES6+ syntax (const/let, arrow functions, destructuring)

**Comments:**
- Explain "why" not "what"
- Add context for complex logic
- Keep comments concise (<80 chars)
- Use JSDoc for functions

**Examples:**
- Use realistic variable names (no `foo`, `bar`)
- Show complete, runnable code
- Include error handling
- Add console.log for visibility

---

## Asset Checklist

### Required Graphics

- [ ] Title cards (4 videos)
- [ ] Performance comparison charts
- [ ] Memory usage graphs
- [ ] Architecture diagrams
- [ ] Feature comparison tables
- [ ] Checklists (migration, production)
- [ ] Resource links overlay

### Required B-Roll

- [ ] Terminal commands executing
- [ ] Code editor with autocomplete
- [ ] Test suite running
- [ ] Documentation site navigation
- [ ] Graph visualizations
- [ ] Monitoring dashboards

### Required Code Samples

- [ ] Migration examples (before/after)
- [ ] Getting started project (complete)
- [ ] Benchmark harness
- [ ] Production configuration

### Required Datasets

- [ ] Tiny (100 triples) - testing
- [ ] Small (10K triples) - demos
- [ ] Medium (100K triples) - benchmarks
- [ ] contacts.ttl - getting started example

---

## Publishing Checklist

### YouTube Upload

- [ ] Title: "UNRDF v5: [Video Title]"
- [ ] Description: Include timestamps, resources, links
- [ ] Tags: rdf, sparql, knowledge-graph, semantic-web, tutorial
- [ ] Thumbnail: Custom designed (1280x720)
- [ ] Playlist: Add to "UNRDF v5 Tutorial Series"
- [ ] End screen: Link to next video + subscribe
- [ ] Cards: Add at key moments

### Documentation Site

- [ ] Embed YouTube player
- [ ] Add transcript (searchable)
- [ ] Link to code samples (GitHub)
- [ ] Include downloadable resources
- [ ] Add related articles

### Social Media

- [ ] Twitter: Thread with key takeaways + video link
- [ ] LinkedIn: Professional post with highlights
- [ ] Discord: Announcement in #videos channel
- [ ] Reddit: r/semanticweb, r/programming (follow rules)

### GitHub

- [ ] Add video links to README.md
- [ ] Update CHANGELOG.md
- [ ] Tag code samples with video reference
- [ ] Create discussion thread for feedback

---

## Timeline

### Week 1: Pre-Production
- Script review & finalization
- Code samples testing
- Graphics design
- Dataset creation

### Week 2-3: Production
- Record all 4 videos
- Capture B-roll footage
- Multiple takes for best quality

### Week 4: Post-Production
- Video editing
- Graphics & animation
- Audio mixing
- Closed captioning

### Week 5: Publishing
- YouTube uploads
- Documentation updates
- Social media campaign
- Community engagement

---

## Resources

**Tools:**
- Screen recording: OBS Studio, Screenflow
- Video editing: DaVinci Resolve, Final Cut Pro
- Graphics: Figma, Adobe Illustrator
- Animation: After Effects, Keynote

**References:**
- [YouTube Creator Academy](https://creatoracademy.youtube.com/)
- [Diataxis Documentation Framework](https://diataxis.fr/)
- [Technical Video Best Practices](https://techwriting.io/video-tutorials/)

**Templates:**
- YouTube description template: `./templates/youtube-description.md`
- Social media posts: `./templates/social-media.md`
- Thumbnail designs: `./templates/thumbnails/`

---

## Feedback & Iteration

**Metrics to Track:**
- View count & watch time
- Audience retention graph
- Comments sentiment
- Like/dislike ratio
- Click-through rate (CTR)
- Subscription conversions

**Iteration Process:**
1. Publish video
2. Monitor metrics for 7 days
3. Read all comments
4. Identify improvement areas
5. Update script for future videos
6. Re-record if major issues

**Common Feedback:**
- "Too fast" → Slow down, add pauses
- "Can't see code" → Increase font size
- "Want downloadable code" → Add GitHub gist
- "Need more examples" → Expand use cases

---

## Contact

**Questions or suggestions?**
- GitHub Issues: https://github.com/unrdf/unrdf/issues
- Discord: https://discord.gg/unrdf
- Email: video-feedback@unrdf.org

---

**Document Version:** 1.0.0
**Created:** 2025-12-06
**Last Updated:** 2025-12-06
**Status:** Ready for Production
