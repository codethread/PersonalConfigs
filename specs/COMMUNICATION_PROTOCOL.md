# Agent Communication Protocol for Claude Code Workflows

This protocol defines the communication standards for multi-agent workflows in Claude Code, specifically designed for the stateless nature of subagent interactions.

## Why This Protocol Exists

1. **Stateless Architecture**: Subagents operate in single request/response cycles with no ability for follow-up clarification
2. **Context Isolation**: Each agent invocation starts fresh, requiring complete context transfer upfront
3. **Precision Requirements**: Ambiguity cannot be resolved through discussion, making initial clarity critical
4. **Traceability**: Multiple agents working on interconnected tasks need clear reference points for coordination
5. **Boundary Definition**: Agents must understand both their responsibilities AND explicit non-responsibilities to prevent scope creep

## File Reference Standard

All file references MUST use the vimgrep format:

```
/full/path/from/cwd/file.ext:line:column
```

Examples:

- `/Users/codethread/PersonalConfigs/src/auth.ts:45:12`
- `/Users/codethread/PersonalConfigs/config/settings.json:102:3`

When referencing ranges, use:

```
/path/to/file.ext:startLine:startCol-endLine:endCol
```

## Specification Structure Requirements

### Feature Specification (SPEC)

Feature specifications MUST use numbered sections for reference:

```markdown
## Functional Requirements

### FR-1: User Authentication

The system SHALL provide email-based authentication

### FR-2: Session Management

The system SHALL maintain user sessions for 24 hours

## Non-Functional Requirements

### NFR-1: Performance

Authentication SHALL complete within 2 seconds

### NFR-2: Security

Passwords SHALL be hashed using bcrypt with minimum 10 rounds
```

### Technical Specification (TECH_SPEC)

Technical specifications MUST contain implementation checklists that reference feature requirements:

```markdown
## Implementation Tasks

### Component: Authentication Service

Location: `/Users/codethread/PersonalConfigs/src/services/auth.ts`

- [ ] **AUTH-1**: Implement email validation (delivers FR-1)
  - Validates email format per RFC 5322
  - Returns error for invalid formats
  - File: Create at `/Users/codethread/PersonalConfigs/src/validators/email.ts`

- [ ] **AUTH-2**: Implement password hashing (delivers NFR-2)
  - Uses bcrypt library with 12 rounds
  - Interfaces with: `/Users/codethread/PersonalConfigs/src/lib/crypto.ts:15:1`

- [ ] **AUTH-3**: Create login endpoint (delivers FR-1, NFR-1)
  - POST /api/auth/login
  - Response time < 2000ms
  - Updates: `/Users/codethread/PersonalConfigs/src/routes/auth.ts:28:1`
```

## Agent Briefing Protocol

When delegating to any agent, provide this structured context:

```yaml
Context:
  Phase: [specification|design|implementation|verification]
  Role: "You are working on [phase] of [feature]"
  Workflow_Position: "Previous phase: [x] | Your phase: [y] | Next phase: [z]"

Inputs:
  Primary_Spec: /full/path/to/spec.md
  Technical_Spec: /full/path/to/spec.tech.md # if exists
  Related_Docs:
    - /full/path/to/related.md

Your_Responsibilities:
  - [Specific task 1]
  - [Specific task 2]

NOT_Your_Responsibilities:
  - [Explicitly excluded task 1]
  - [Explicitly excluded task 2]

Deliverables:
  Format: [Description of expected output format]
  References: "Use pattern: file:line:col for all code references"
  Checklist_Items: [List specific items to complete, e.g., "AUTH-1, AUTH-2, AUTH-3"]
```

## Handover Requirements

### From Specification to Design

The specification phase MUST provide:

- Complete feature specification with numbered requirements (FR-X, NFR-X)
- Technical notes if spike work was performed
- Clear success criteria for each requirement

### From Design to Implementation

The design phase MUST provide:

- Technical specification with numbered tasks (COMPONENT-N)
- Each task explicitly linked to feature requirements
- File paths for all components to be created/modified
- Interface definitions with exact file:line:col references

### From Implementation to Verification

The implementation phase MUST provide:

- Completed checklist items with file:line:col references to changes
- List of any deviations from technical specification
- Known limitations or incomplete items

### Verification Reporting

The verification phase MUST provide:

- Status for each numbered requirement (FR-X, NFR-X)
- Status for each implementation task (COMPONENT-N)
- Specific file:line:col references for any issues found
- Clear PASS/FAIL status with evidence

## Example Agent Invocation

```markdown
Context:
Phase: implementation
Role: "You are implementing the authentication service for a user management feature"
Workflow_Position: "Previous phase: technical design | Your phase: implementation | Next phase: QA verification"

Inputs:
Primary_Spec: /Users/codethread/PersonalConfigs/specs/001-user-auth.md
Technical_Spec: /Users/codethread/PersonalConfigs/specs/001-user-auth.tech.md
Related_Docs: - /Users/codethread/PersonalConfigs/specs/SPEC_PATTERNS.md

Your_Responsibilities:

- Implement tasks AUTH-1, AUTH-2, and AUTH-3 from the technical spec
- Ensure all code follows project conventions in CLAUDE.md
- Create unit tests for each component

NOT_Your_Responsibilities:

- Do not implement tasks AUTH-4 through AUTH-6 (assigned to parallel stream)
- Do not modify database schema (completed in previous sprint)
- Do not deploy or configure production environment

Deliverables:
Format: Working code with all specified tasks completed
References: "Use pattern: /full/path/file.ts:line:col for all code references"
Checklist_Items: "Complete and mark done: AUTH-1, AUTH-2, AUTH-3"
```

## Validation Rules

1. **Never use relative paths** - Always use full paths from project root
2. **Always include line numbers** - Even for new files (use :1:1)
3. **Reference specific items** - Use requirement IDs (FR-1) not descriptions
4. **Maintain checklist state** - Mark items complete immediately upon finishing
5. **Document deviations** - Any variance from spec must be explicitly noted with rationale

## Protocol Versioning

Protocol Version: 1.0.0
Last Updated: 2025-01-19
Compatibility: Claude Code with stateless subagents

Changes to this protocol require updating all prime-\* commands that reference it.
