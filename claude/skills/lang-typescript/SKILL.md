---
name: typescript
description: Write clean, type-safe TypeScript code using modern patterns, strict configuration, and best practices. Use when writing TypeScript code, configuring projects, or solving type-related challenges.
---

# TypeScript Expert

Write clean, type-safe TypeScript code that leverages the full power of the type system to catch bugs at compile time.

## When to Use This Skill

Use this skill when:
- Writing or refactoring TypeScript code
- Configuring TypeScript projects (tsconfig.json)
- Solving complex type-related challenges
- Choosing between type system patterns
- Validating external data with types

## Core Workflow

### 1. Type Decision Tree

**Choose the right construct:**

| Use Case | Use | Not |
|----------|-----|-----|
| Object shapes | `interface` | `type` |
| Unions/primitives | `type` | `interface` |
| Dynamic data | `unknown` | `any` |
| State machines | Discriminated unions | Complex conditionals |
| Domain types | Branded types | Plain primitives |

**Example:**
```typescript
// ✅ Correct choices
interface User { id: number; name: string }  // Object shape
type Status = 'idle' | 'loading' | 'success' // Union
type USD = number & { readonly __brand: 'USD' } // Branded type

// ❌ Wrong choices
type User = { id: number }  // Use interface
interface Status { /* ... */ }  // Can't do unions
```

### 2. State Modeling Pattern

For finite states, always use discriminated unions to eliminate impossible states:

```typescript
type ApiState =
  | { status: 'idle' }
  | { status: 'loading' }
  | { status: 'success'; data: string }
  | { status: 'error'; message: string };

// Exhaustiveness checking
function handle(state: ApiState) {
  switch (state.status) {
    case 'success': return state.data;
    case 'error': return state.message;
    case 'idle': return 'Not started';
    case 'loading': return 'Loading...';
    default:
      const _exhaustive: never = state; // Compiler error if cases missing
      throw new Error('Unhandled state');
  }
}
```

### 3. Validation Workflow

**Always validate external data with runtime checks:**

1. **Simple validation:** Type guards
2. **Complex validation:** Schema libraries (Zod, Yup)

```typescript
// Type guard pattern
function isUser(data: unknown): data is User {
  return (
    typeof data === 'object' &&
    data !== null &&
    'id' in data &&
    typeof (data as any).id === 'number'
  );
}

// Schema validation (preferred for APIs)
import { z } from 'zod';

const UserSchema = z.object({
  id: z.number(),
  name: z.string(),
  email: z.string().email(),
});

type User = z.infer<typeof UserSchema>;
const user = UserSchema.parse(apiData); // Runtime validation + types
```

**Never use type assertions without validation:**
```typescript
// ❌ BAD: No runtime check
const user = data as User;

// ✅ GOOD: Validated first
if (isUser(data)) {
  // data is User here
}
```

### 4. Configuration Checklist

For new projects, enable strict mode plus:

```json
{
  "compilerOptions": {
    "strict": true,
    "noUncheckedIndexedAccess": true,
    "exactOptionalPropertyTypes": true,
    "noPropertyAccessFromIndexSignature": true,
    "moduleResolution": "bundler"
  }
}
```

**Why these matter:**
- `noUncheckedIndexedAccess` - Prevents undefined access bugs
- `exactOptionalPropertyTypes` - Distinguishes missing from undefined
- `moduleResolution: "bundler"` - Optimized for Vite/esbuild

### 5. Code Organization Rules

**Barrel files:** Avoid for internal code (75% faster builds)

```typescript
// ❌ BAD: Internal barrel
// src/components/index.ts
export * from './Button';

// ✅ GOOD: Direct imports + path aliases
// tsconfig.json: "paths": { "@/components/*": ["src/components/*"] }
import { Button } from '@/components/Button';
```

**Only use barrel files for:**
- Library entry points
- Public APIs
- Small modules (<10 exports)

## Quick Reference Patterns

### Utility Types

```typescript
type UserPreview = Pick<User, 'id' | 'name'>;  // Extract subset
type PublicUser = Omit<User, 'email'>;         // Remove fields
type UpdateDto = Partial<User>;                 // Make optional
type CompleteUser = Required<User>;             // Make required
type ImmutableUser = Readonly<User>;            // Make readonly
type UserType = ReturnType<typeof getUser>;    // Extract return type
```

### Error Handling

```typescript
// Catch with unknown
try { } catch (err: unknown) {
  if (err instanceof Error) { /* ... */ }
}

// Result types for expected failures
type Result<T, E = Error> =
  | { success: true; value: T }
  | { success: false; error: E };
```

### Readonly Patterns

```typescript
const config: Readonly<Config> = { /* ... */ };
const numbers: readonly number[] = [1, 2, 3];
const ROUTES = { home: '/' } as const;  // Deep readonly + literal types
```

## When to Consult Detailed References

For comprehensive information on advanced patterns, configuration options, or specific features, read:
- `references/best-practices-2025.md` - Full TypeScript best practices guide

The reference includes:
- Advanced type patterns (conditional types, mapped types, branded types)
- Complete tsconfig.json options
- Modern features (decorators, const type parameters)
- Common anti-patterns and solutions

## Quality Checklist

Before completing TypeScript code:
- [ ] External data validated (not just typed)
- [ ] No `any` types (or explicitly justified)
- [ ] State machines use discriminated unions
- [ ] Utility types used where applicable
- [ ] Readonly applied to prevent mutations
- [ ] Exhaustiveness checks with `never`
