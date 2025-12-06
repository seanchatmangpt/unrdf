# End-to-End Testing Suite

## Overview

This E2E test suite validates the Nuxt UI Docs template from the perspective of **7 user avatars**, each with specific **Jobs-to-be-Done (JTBD)**.

## User Avatars & Test Coverage

### 1. Alex the API Developer
**File**: `avatars/alex-api-developer.spec.ts`

**Jobs to be Done**:
- ✅ Document API endpoints clearly
- ✅ Get AI assistance for code
- ✅ Test API responses without dependencies

**Test Coverage**:
- 11 tests covering documentation navigation, AI chat integration, and API testing
- Validates syntax highlighting, search functionality, and mock testing

---

### 2. Sofia the Technical Writer
**File**: `avatars/sofia-technical-writer.spec.ts`

**Jobs to be Done**:
- ✅ Organize content hierarchically
- ✅ Generate content with AI
- ✅ Preview markdown rendering

**Test Coverage**:
- 11 tests covering content structure, AI content generation, and markdown rendering
- Validates navigation, TOC, code blocks, and prose components

---

### 3. Marcus the DevOps Engineer
**File**: `avatars/marcus-devops.spec.ts`

**Jobs to be Done**:
- ✅ Use embedded database without external services
- ✅ Configure environment variables
- ✅ Test database queries locally

**Test Coverage**:
- 10 tests covering PGlite configuration, environment setup, and database operations
- Validates zero-configuration deployment and CRUD functionality

---

### 4. Priya the Product Manager
**File**: `avatars/priya-product-manager.spec.ts`

**Jobs to be Done**:
- ✅ Chat with AI about technical feasibility
- ✅ View visual examples and component demos
- ✅ Understand authentication flows

**Test Coverage**:
- 10 tests covering AI chat, component documentation, and OAuth flows
- Validates product research capabilities and visual examples

---

### 5. Chen the Full-Stack Developer
**File**: `avatars/chen-fullstack.spec.ts`

**Jobs to be Done**:
- ✅ Use modern UI components
- ✅ Implement streaming AI chat
- ✅ Perform database CRUD operations

**Test Coverage**:
- 12 tests covering Nuxt UI components, streaming chat, and database CRUD
- Validates ChatPalette integration and full API functionality

---

### 6. Jasmine the QA Engineer
**File**: `avatars/jasmine-qa.spec.ts`

**Jobs to be Done**:
- ✅ Run comprehensive unit tests
- ✅ Mock external AI services
- ✅ Generate coverage reports

**Test Coverage**:
- 10 tests covering testing infrastructure, mocking, and coverage
- Validates vitest setup, mock APIs, and critical user paths

---

### 7. Raj the Open Source Contributor
**File**: `avatars/raj-oss-contributor.spec.ts`

**Jobs to be Done**:
- ✅ Understand codebase from documentation
- ✅ Run project locally with zero configuration
- ✅ Validate changes with tests before PR

**Test Coverage**:
- 11 tests covering documentation accessibility, zero-config setup, and pre-PR validation
- Validates contributor experience and local development

---

## Test Architecture

```
e2e/
├── avatars/           # User persona test suites (7 files)
│   ├── alex-api-developer.spec.ts
│   ├── sofia-technical-writer.spec.ts
│   ├── marcus-devops.spec.ts
│   ├── priya-product-manager.spec.ts
│   ├── chen-fullstack.spec.ts
│   ├── jasmine-qa.spec.ts
│   └── raj-oss-contributor.spec.ts
├── fixtures/          # Test data and mock responses
│   └── test-data.ts
├── helpers/           # Page objects and utilities
│   └── page-objects.ts
└── README.md          # This file
```

## Running Tests

### All Tests
```bash
pnpm test:e2e
```

### Specific Avatar
```bash
pnpm test:e2e avatars/alex-api-developer.spec.ts
```

### Interactive Mode (UI)
```bash
pnpm test:e2e:ui
```

### Debug Mode
```bash
pnpm test:e2e:debug
```

### View Report
```bash
pnpm test:e2e:report
```

## Test Environments

Tests run across multiple browsers:
- ✅ Chromium (Desktop)
- ✅ Firefox (Desktop)
- ✅ WebKit (Safari)
- ✅ Mobile Chrome (Pixel 5)
- ✅ Mobile Safari (iPhone 12)

## Success Metrics

### Total Coverage
- **7 User Avatars** with distinct personas
- **75 E2E Tests** covering all JTBD scenarios
- **21 Jobs-to-be-Done** validated end-to-end
- **5 Browser Environments** tested

### Feature Validation
- ✅ Documentation navigation and search
- ✅ AI chat integration and streaming
- ✅ Database CRUD operations
- ✅ Authentication flows
- ✅ UI component rendering
- ✅ Mock API responses
- ✅ Zero-configuration deployment
- ✅ Testing infrastructure

## Test Patterns

### Page Object Model
Reusable page interactions in `helpers/page-objects.ts`:
- `HomePage`: Main landing page
- `DocsPage`: Documentation pages
- `ChatModal`: AI chat interface
- `APITestHelper`: API mocking and interception

### Test Data
Centralized fixtures in `fixtures/test-data.ts`:
- User avatars
- Mock chat messages
- Database records
- API responses
- Documentation structure

### JTBD Organization
Each avatar test file follows the pattern:
```typescript
test.describe('Avatar Name - JTBD Journey', () => {
  test.describe('Job 1: [Job Description]', () => {
    // Tests for this job
  })

  test.describe('Success Criteria Validation', () => {
    // Validate all jobs completed
  })
})
```

## CI/CD Integration

Tests are configured for CI environments:
- Retries: 2 (in CI)
- Workers: 1 (in CI)
- Screenshots: On failure
- Videos: On failure
- Traces: On first retry

## Maintenance

### Adding New Tests
1. Choose appropriate avatar persona
2. Add test to relevant JTBD section
3. Use page objects for interactions
4. Update success criteria

### Adding New Avatar
1. Create persona in `USER_AVATARS.md`
2. Create test file in `avatars/`
3. Add fixture data in `fixtures/test-data.ts`
4. Update this README

## Resources

- [Playwright Docs](https://playwright.dev)
- [JTBD Framework](https://jtbd.info)
- [Page Object Pattern](https://playwright.dev/docs/pom)
- [User Avatars](../USER_AVATARS.md)
