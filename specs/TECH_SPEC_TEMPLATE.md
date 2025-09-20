# [Feature Name] - Technical Requirements

User Sign off: [REQUIRED - DO NOT BUILD WITHOUT THIS]

## Executive Summary

[Brief overview of technical approach]

## Architecture Overview

### System Context

[How this fits in the larger system - include diagram if helpful]

### Component Architecture

[Detailed component breakdown with responsibilities]

### Data Architecture

[Data models, storage strategy, caching approach]

## Technology Decisions

### Framework/Library Selection

| Requirement | Option 1    | Option 2    | Recommendation |
| ----------- | ----------- | ----------- | -------------- |
| [Need]      | [Pros/Cons] | [Pros/Cons] | [Choice + Why] |

### Integration Patterns

[How components communicate - REST, GraphQL, Events, etc.]

## Implementation Tasks

### Component: [Component Name]

Location: `relative-to-cwd/path/to/component/directory`

- [ ] **COMP-1**: [Task description] (delivers FR-X, NFR-Y) [TESTABLE]
  - Specific implementation details
  - Interfaces with: `relative-to-cwd/to/file.ext:line:col`
  - Creates: `relative-to-cwd/to/new/file.ext:1:1`

- [ ] **COMP-2**: [Task description] (delivers FR-X) [TESTABLE]
  - Specific implementation details
  - Updates: `relative-to-cwd/to/existing/file.ext:line:col`
  - Dependencies: COMP-1 must be complete

### Component: [Another Component] [TEST AFTER COMPONENT]

Location: `relative-to-cwd/to/another/component`

- [ ] **AUTH-1**: [Authentication task] (delivers FR-1, NFR-3)
  - Implementation specifics
  - File: `relative-to-cwd/to/auth/service.ts:1:1`

- [ ] **AUTH-2**: [Session management] (delivers FR-2)
  - Implementation specifics
  - Updates: `relative-to-cwd/to/session/manager.ts:45:12`
  - Note: AUTH-1 must be complete before AUTH-2

Note: Test AUTH-1 and AUTH-2 together after both are implemented

## Technical Specifications

### API Design

[Detailed endpoint specifications with request/response schemas]

### Database Schema [optional]

[Table definitions, indexes, relationships]

## Testing Strategy

### Unit Testing

[Coverage targets, mocking strategy]

### Integration Testing [optional]

[Test scenarios, environment setup]

## Operational Considerations

### Logging

[logging strategy]

## Technical Debt Considerations

[Known compromises and future refactoring needs]

## Dependencies and Prerequisites

[External services, libraries, infrastructure requirements]

## Regressions or missed requirements

None found

<!-- section for the future to document missed requirements and their resolution, with the intention being to prevent future mistakes -->
