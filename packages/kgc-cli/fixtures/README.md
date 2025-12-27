# Test Fixtures

**Purpose**: Sample LaTeX projects for testing and validation.

## Directory Structure

```
fixtures/
├── minimal/               # Minimal working example
│   └── main.tex
├── article/              # Simple article with references
│   ├── main.tex
│   ├── references.bib
│   └── figures/
├── thesis/               # Complex document (chapters, ToC, bibliography)
│   ├── main.tex
│   ├── chapters/
│   ├── bibliography.bib
│   └── figures/
└── errors/               # Intentional error cases (for diagnostics testing)
    ├── missing-package.tex
    ├── undefined-command.tex
    └── syntax-error.tex
```

## Usage

Fixtures are used by:
- Integration tests (`test/latex-build.test.mjs`)
- VFS validation tests (`test/latex-vfs.test.mjs`)
- Diagnostics tests (`test/latex-diagnostics.test.mjs`)
- CLI manual testing

## Minimal Example

```latex
% fixtures/minimal/main.tex
\documentclass{article}
\begin{document}
Hello, World!
\end{document}
```

This should compile successfully with zero dependencies on all engines.

## Creating New Fixtures

1. Add directory under `fixtures/`
2. Include `main.tex` as entry point
3. Document expected behavior (success/failure)
4. Add test case in `test/` directory
