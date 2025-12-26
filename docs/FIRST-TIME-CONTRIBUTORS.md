# First-Time Contributors Guide

Welcome to UNRDF! This guide is specifically designed for developers making their very first open-source contribution.

**Never contributed to open source before? Perfect! You're in the right place.**

## What You'll Learn

By the end of this guide, you'll:
- Understand how to fork, clone, and contribute to an open-source project
- Make your first pull request to UNRDF
- Understand the basics of RDF and knowledge graphs
- Know where to find help when you get stuck

**Time required:** 1-2 hours total

---

## Prerequisites

Don't worry if you're new to some of these tools. We'll walk you through everything.

### Required Knowledge
- ✅ Basic JavaScript (variables, functions, async/await)
- ✅ Command line basics (cd, ls, running commands)
- ✅ Basic Git (or willing to learn)

### Required Tools
- [ ] **Node.js 18+** - [Download here](https://nodejs.org)
- [ ] **Git** - [Download here](https://git-scm.com)
- [ ] **GitHub account** - [Sign up here](https://github.com)
- [ ] **Code editor** - VS Code recommended ([Download](https://code.visualstudio.com))

### Recommended (but optional)
- GitHub Desktop (easier than command-line Git for beginners)
- Basic understanding of what RDF is (we'll explain as we go)

---

## The Complete Beginner's Path

### Step 1: Understanding UNRDF (10 minutes)

**What is UNRDF?**

UNRDF is a library that helps you work with "knowledge graphs" - a way to store information about how things relate to each other.

**Example:**
Instead of storing data in tables like this:
```
Name    | Knows
Alice   | Bob
Bob     | Charlie
```

Knowledge graphs store relationships like this:
```
Alice → knows → Bob
Bob → knows → Charlie
Alice → knows → Charlie
```

This makes it easy to ask questions like:
- "Who does Alice know?"
- "Who are Bob's friends?"
- "Who knows who knows Alice?" (friends of friends)

**Why contribute to UNRDF?**
- Learn about semantic web technologies
- Build your open-source portfolio
- Help improve a real project used by developers
- Join a welcoming community

---

### Step 2: Set Up Your Development Environment (20 minutes)

#### 2.1 Install Node.js

1. Go to https://nodejs.org
2. Download the LTS (Long Term Support) version
3. Install it (default options are fine)
4. Verify installation:
   ```bash
   node --version  # Should show v18.x.x or higher
   ```

#### 2.2 Install Git

**Mac:**
```bash
# If you have Homebrew
brew install git

# Or download from https://git-scm.com
```

**Windows:**
Download from https://git-scm.com and install.

**Linux:**
```bash
sudo apt-get install git  # Ubuntu/Debian
sudo yum install git      # CentOS/RHEL
```

Verify:
```bash
git --version
```

#### 2.3 Configure Git

Tell Git who you are:
```bash
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"
```

#### 2.4 Set Up GitHub

1. Create account at https://github.com
2. Set up SSH keys (recommended):
   - Follow: https://docs.github.com/en/authentication/connecting-to-github-with-ssh
   - Or use HTTPS (GitHub will prompt for password)

#### 2.5 Install pnpm

UNRDF uses pnpm instead of npm:
```bash
npm install -g pnpm
pnpm --version  # Should show 8.x.x or higher
```

---

### Step 3: Get the Code (15 minutes)

#### 3.1 Fork the Repository

1. Go to https://github.com/unrdf/unrdf
2. Click the "Fork" button in top right
3. Wait for GitHub to create your copy

**What is forking?**
Forking creates your own copy of the project where you can make changes without affecting the original.

#### 3.2 Clone Your Fork

**Using command line:**
```bash
# Replace YOUR-USERNAME with your GitHub username
git clone https://github.com/YOUR-USERNAME/unrdf.git
cd unrdf
```

**Using GitHub Desktop:**
1. File → Clone Repository
2. Find your fork (YOUR-USERNAME/unrdf)
3. Choose a local path
4. Click "Clone"

#### 3.3 Set Up Remotes

This connects your local copy to both your fork and the original project:

```bash
git remote add upstream https://github.com/unrdf/unrdf.git
git remote -v  # Verify you see 'origin' and 'upstream'
```

**What are remotes?**
- `origin` = your fork (where you push changes)
- `upstream` = original project (where you pull updates)

---

### Step 4: Install and Build (10 minutes)

```bash
# Install dependencies (may take 3-5 minutes)
pnpm install

# Build the project (may take 2-3 minutes)
pnpm run build
```

**What's happening?**
- `pnpm install` downloads all the code libraries UNRDF needs
- `pnpm run build` compiles the code into a runnable form

**If you see errors:** Check [docs/TROUBLESHOOTING.md](TROUBLESHOOTING.md)

---

### Step 5: Make Your First Contribution (30 minutes)

Let's make a simple documentation improvement - perfect for first-timers!

#### 5.1 Find Something to Fix

Easy options:
- Fix a typo in README.md or docs/
- Add a clarifying example
- Improve an error message
- Add yourself to a contributors list

**For practice, let's add an example to the README:**

#### 5.2 Create a Branch

```bash
git checkout -b docs/add-example
```

**What is a branch?**
A branch is like a parallel universe where you can make changes without affecting the main code.

#### 5.3 Make Your Change

Open `README.md` in your editor and add a simple example:

```markdown
### Example: Query for People's Names

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();
const store = core.parseRdf(`
  @prefix ex: <http://example.org/> .
  ex:Alice ex:name "Alice Johnson" .
  ex:Bob ex:name "Bob Smith" .
`);

const results = await core.query(store, `
  SELECT ?name WHERE { ?person ex:name ?name }
`);

for (const row of results) {
  console.log(row.get('name').value);
}
// Output:
// Alice Johnson
// Bob Smith
```
```

#### 5.4 Test Your Change

```bash
# Make sure nothing broke
pnpm test

# If tests fail, don't worry - ask for help!
```

#### 5.5 Commit Your Change

```bash
git add README.md
git commit -m "docs: add query example to README"
```

**Commit message format:**
```
type(scope): description

Examples:
docs: fix typo in installation guide
feat: add new SPARQL function
fix: resolve parsing error
```

#### 5.6 Push to Your Fork

```bash
git push -u origin docs/add-example
```

#### 5.7 Create a Pull Request

1. Go to https://github.com/YOUR-USERNAME/unrdf
2. You'll see a banner: "Compare & pull request" - click it
3. Fill in the template:

```markdown
## Description
Added a simple query example to help new users understand how to query RDF data.

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [x] Documentation update

## Checklist
- [x] Follows style guidelines
- [x] Tested locally
- [x] Clear commit message
```

4. Click "Create pull request"

**Congratulations!** You've made your first open-source contribution!

---

### Step 6: Respond to Feedback (Variable)

A maintainer will review your PR. They might:
- ✅ Approve and merge it immediately
- 💬 Ask questions or request changes
- 🔄 Suggest improvements

**If changes are requested:**

1. Make the changes locally
2. Commit them:
   ```bash
   git add .
   git commit -m "docs: address review feedback"
   ```
3. Push to the same branch:
   ```bash
   git push
   ```
4. The PR updates automatically!

**Be patient** - reviews may take a few days. Maintainers are volunteers.

---

## Common First-Timer Questions

### "What should I contribute?"

**Great first contributions:**
1. **Documentation** - Fix typos, add examples, clarify confusing parts
2. **Tests** - Add tests for existing features
3. **Issues** - Look for `good first issue` label
4. **Examples** - Add real-world usage examples

**Avoid for first PR:**
- Large refactors
- Breaking changes
- New major features

### "What if I make a mistake?"

**Don't worry!** Everyone makes mistakes. Worst case:
- Your PR isn't merged (you learned something!)
- Maintainer asks you to fix something (you learn more!)
- You accidentally break something (Git can undo anything!)

### "How do I understand the code?"

**Start small:**
1. Read the docs: [docs/START-HERE.md](START-HERE.md)
2. Look at examples: `examples/` directory
3. Read tests: `packages/*/test/` - tests show how to use code
4. Ask questions: GitHub Discussions

**Don't try to understand everything at once!**

### "What if my English isn't perfect?"

**No problem!** We value contributions from everyone, regardless of language skills. Focus on:
- Clear, simple sentences
- Code examples (code is universal!)
- Diagrams or screenshots

Maintainers will help with language if needed.

### "I'm stuck. Where do I get help?"

**Ask for help - we're friendly!**

1. **Comment on your PR** - "@maintainers I'm stuck with X"
2. **GitHub Discussions** - https://github.com/unrdf/unrdf/discussions
3. **Open an issue** - Tag it `question`
4. **Check troubleshooting** - [docs/TROUBLESHOOTING.md](TROUBLESHOOTING.md)

**No question is too basic!** We all started somewhere.

---

## Next Steps After Your First PR

### Learn More About UNRDF
- [docs/ARCHITECTURE.md](ARCHITECTURE.md) - How the system works
- [docs/WALKTHROUGHS.md](WALKTHROUGHS.md) - Step-by-step tutorials
- [docs/PACKAGES.md](PACKAGES.md) - What each package does

### Take On Bigger Challenges
- Find a `good first issue` with code changes
- Add a new example to `examples/`
- Write a tutorial
- Fix a bug

### Help Other First-Timers
- Review PRs from other contributors
- Answer questions in Discussions
- Improve this guide!

### Build Your Portfolio
- Add UNRDF to your GitHub profile
- Blog about your contribution
- Present at a meetup
- List it on your resume

---

## Terminology Guide

**Quick reference for terms you'll see:**

- **Fork** - Your copy of the project on GitHub
- **Clone** - Download code to your computer
- **Branch** - Isolated workspace for changes
- **Commit** - Save changes with a message
- **Push** - Upload commits to GitHub
- **Pull Request (PR)** - Ask to merge your changes
- **Merge** - Combine your changes into main project
- **Remote** - GitHub repository (origin, upstream)
- **Repository (Repo)** - Project folder with Git history

**RDF/UNRDF specific:**
- **Triple** - Subject-Predicate-Object statement
- **Store** - Database of triples
- **SPARQL** - Query language for RDF (like SQL)
- **Turtle** - RDF file format (`.ttl`)
- **Knowledge Graph** - Network of interconnected data

---

## Resources for Learning

### Git & GitHub
- [GitHub's Git Guides](https://github.com/git-guides)
- [First Timers Only](https://www.firsttimersonly.com/)
- [How to Contribute to Open Source](https://opensource.guide/how-to-contribute/)

### RDF & Semantic Web
- [RDF Primer](https://www.w3.org/TR/rdf-primer/)
- [SPARQL Tutorial](https://www.w3.org/TR/sparql11-query/)
- [Linked Data Book](https://linkeddatabook.com/editions/1.0/)

### JavaScript
- [MDN JavaScript Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)
- [JavaScript.info](https://javascript.info/)

---

## Success Stories

*Share your first contribution experience!*

**Add your story here by submitting a PR:**

```markdown
### [Your Name] - [Month Year]
**My first contribution:** [Brief description]
**What I learned:** [Key takeaway]
**Advice for others:** [One tip]
```

---

## Final Encouragement

Making your first open-source contribution is a big step - and you're doing great!

Remember:
- Everyone was a beginner once
- Mistakes are how we learn
- The community is here to help
- Your contribution matters, no matter how small

**You've got this!** 🚀

---

**Ready to contribute?**

1. Follow the steps above
2. Create your first PR
3. Add your name to the success stories
4. Help the next first-timer!

**Questions?** Open an issue or ask in [Discussions](https://github.com/unrdf/unrdf/discussions).

Welcome to the UNRDF community!
