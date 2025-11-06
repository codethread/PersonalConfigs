# TypeScript Best Practices for 2025

*Comprehensive guide for AI agents and developers writing TypeScript code*

**Last Updated:** 2025-11-04  
**Sources:** TypeScript official docs, DEV Community, LogRocket, 2ality.com, and multiple 2025 technical articles

---

## Table of Contents

1. [Type System Best Practices](#1-type-system-best-practices)
2. [Configuration & Tooling](#2-configuration--tooling)
3. [Code Organization](#3-code-organization)
4. [Common Patterns & Anti-patterns](#4-common-patterns--anti-patterns)
5. [Modern Features (2024-2025)](#5-modern-features-2024-2025)

---

## 1. Type System Best Practices

### 1.1 Type vs Interface

**Current Consensus (2025):**
Both `type` and `interface` can define object shapes, but they have different characteristics:

**Key Differences:**

| Feature | Interface | Type |
|---------|-----------|------|
| Object shapes | ✅ | ✅ |
| Primitives/unions | ❌ | ✅ |
| Declaration merging | ✅ | ❌ |
| Performance | Cached internally | Recomputed each use |
| Composition | `extends` (faster) | `&` (slower) |

**Official Guidance:**
- TypeScript docs suggest defaulting to `interface` when possible
- For composition, prefer `interface extends` over `type &` (faster and more predictable)

**Practical Recommendation (2025):**
```typescript
// Use interface for:
// 1. Object shapes that may be extended
interface User {
  id: number;
  name: string;
}

interface Admin extends User {
  permissions: string[];
}

// 2. Public API definitions (allows consumers to extend via declaration merging)
interface ButtonProps {
  label: string;
  onClick: () => void;
}

// Use type for:
// 1. Unions and primitives
type Status = 'loading' | 'success' | 'error';
type ID = string | number;

// 2. Complex mapped/conditional types
type Readonly<T> = {
  readonly [K in keyof T]: T[K];
};

// 3. Tuples
type Point = [number, number];

// 4. When declaration merging would be problematic
type Config = {
  apiKey: string;
}; // Cannot be accidentally extended
```

### 1.2 Advanced Type Patterns

#### Discriminated Unions

**Purpose:** Model finite states, eliminate impossible states, enable exhaustive checking

```typescript
// State machine pattern
type ApiState =
  | { status: 'idle' }
  | { status: 'loading' }
  | { status: 'success'; data: string }
  | { status: 'error'; message: string };

function handleState(state: ApiState) {
  switch (state.status) {
    case 'idle':
      return 'Not started';
    case 'loading':
      return 'Loading...';
    case 'success':
      return `Data: ${state.data}`; // TypeScript knows data exists
    case 'error':
      return `Error: ${state.message}`; // TypeScript knows message exists
    default:
      // Exhaustiveness check - will fail if new status added
      const _exhaustive: never = state;
      return _exhaustive;
  }
}

// Multi-shaped API responses
type ApiResponse =
  | { kind: 'user'; id: number; name: string }
  | { kind: 'post'; id: number; title: string; content: string }
  | { kind: 'comment'; id: number; text: string; postId: number };

function displayResponse(response: ApiResponse) {
  // TypeScript narrows automatically based on 'kind'
  if (response.kind === 'user') {
    console.log(response.name); // OK - knows it's a user
  }
}
```

**Benefits:**
- Compile-time guarantees of handling all cases
- Eliminates invalid state combinations
- Self-documenting state transitions

#### Branded Types (Nominal Typing)

**Purpose:** Prevent accidental mixing of structurally similar types

```typescript
// Problem: TypeScript uses structural typing
function transferMoney(from: number, to: number, amount: number) {
  // Easy to accidentally swap parameters
}

// Solution: Branded types simulate nominal typing
type USD = number & { readonly __brand: 'USD' };
type EUR = number & { readonly __brand: 'EUR' };
type AccountId = string & { readonly __brand: 'AccountId' };

// Constructor functions enforce branding
function usd(amount: number): USD {
  return amount as USD;
}

function eur(amount: number): EUR {
  return amount as EUR;
}

function accountId(id: string): AccountId {
  return id as AccountId;
}

// Now this won't compile:
function transferUSD(from: AccountId, to: AccountId, amount: USD) {
  // Implementation
}

const euros = eur(100);
// transferUSD(accountId('123'), accountId('456'), euros); // ❌ Type error!

// Correct usage:
transferUSD(accountId('123'), accountId('456'), usd(100)); // ✅
```

**Use Cases:**
- Financial calculations (currencies, precision)
- Units of measurement (meters, kilometers)
- IDs (userId vs orderId vs productId)
- Validated strings (email, URL, UUID)

#### Conditional Types

**Purpose:** Create types based on compile-time logic

```typescript
// Basic conditional type
type IsString<T> = T extends string ? 'yes' : 'no';

type A = IsString<string>;  // 'yes'
type B = IsString<number>;  // 'no'

// Distributive conditional types (over unions)
type ToArray<T> = T extends any ? T[] : never;
type StrOrNumArray = ToArray<string | number>;
// Result: string[] | number[]

// Extract function return types
type ReturnType<T> = T extends (...args: any[]) => infer R ? R : never;

type Func = (a: number) => string;
type FuncReturn = ReturnType<Func>; // string

// Filter union types
type NonNullable<T> = T extends null | undefined ? never : T;
type Clean = NonNullable<string | null | undefined>; // string

// Advanced: Extract promise type
type Awaited<T> = T extends Promise<infer U> ? U : T;
type PromiseValue = Awaited<Promise<string>>; // string
```

**Common Patterns:**
- Type extraction with `infer`
- Union filtering with `never`
- Recursive type operations
- Library API type transformations

#### Mapped Types

**Purpose:** Transform existing types systematically

```typescript
// Make all properties optional
type Partial<T> = {
  [K in keyof T]?: T[K];
};

// Make all properties readonly
type Readonly<T> = {
  readonly [K in keyof T]: T[K];
};

// Custom transformations
type Nullable<T> = {
  [K in keyof T]: T[K] | null;
};

interface User {
  id: number;
  name: string;
  email: string;
}

type NullableUser = Nullable<User>;
// Result: { id: number | null; name: string | null; email: string | null }

// Conditional property transformation
type Getters<T> = {
  [K in keyof T as `get${Capitalize<string & K>}`]: () => T[K];
};

type UserGetters = Getters<User>;
// Result: {
//   getId: () => number;
//   getName: () => string;
//   getEmail: () => string;
// }
```

### 1.3 Generic Type Constraints and Inference

```typescript
// Basic constraint
function getProperty<T, K extends keyof T>(obj: T, key: K): T[K] {
  return obj[key];
}

const user = { id: 1, name: 'Alice' };
const id = getProperty(user, 'id'); // Type: number
// getProperty(user, 'age'); // ❌ Error: 'age' not in User

// Multiple constraints
function merge<T extends object, U extends object>(obj1: T, obj2: U): T & U {
  return { ...obj1, ...obj2 };
}

// Inferred generic constraints
function map<T, U>(arr: T[], fn: (item: T) => U): U[] {
  return arr.map(fn);
}

// TypeScript infers T = number, U = string
const strings = map([1, 2, 3], (n) => n.toString());

// Generic constraint with default
type ApiResponse<T = unknown> = {
  data: T;
  status: number;
};

// Recursive generic constraints
type DeepReadonly<T> = {
  readonly [K in keyof T]: T[K] extends object
    ? DeepReadonly<T[K]>
    : T[K];
};
```

### 1.4 Utility Types Usage

**Built-in utility types reduce boilerplate and improve maintainability.**

```typescript
// ========================================
// Partial<T> - Make all properties optional
// ========================================
interface User {
  id: number;
  name: string;
  email: string;
  age: number;
}

function updateUser(id: number, updates: Partial<User>) {
  // Perfect for PATCH operations where any subset can be updated
}

updateUser(1, { name: 'Bob' }); // ✅
updateUser(1, { email: 'bob@example.com', age: 30 }); // ✅

// ========================================
// Required<T> - Make all properties mandatory
// ========================================
interface Config {
  apiKey?: string;
  timeout?: number;
  retries?: number;
}

function initializeWithDefaults(config: Required<Config>) {
  // Ensures all properties exist
}

// ========================================
// Readonly<T> - Make all properties readonly
// ========================================
interface Mutable {
  value: number;
}

const immutable: Readonly<Mutable> = { value: 42 };
// immutable.value = 43; // ❌ Error: Cannot assign to 'value'

// ========================================
// Pick<T, K> - Select specific properties
// ========================================
type UserPreview = Pick<User, 'id' | 'name'>;
// Result: { id: number; name: string; }

const preview: UserPreview = { id: 1, name: 'Alice' };

// ========================================
// Omit<T, K> - Remove specific properties
// ========================================
type UserWithoutEmail = Omit<User, 'email'>;
// Result: { id: number; name: string; age: number; }

// Common pattern: Omit sensitive fields
type PublicUser = Omit<User, 'email' | 'age'>;

// ========================================
// Record<K, T> - Create object type with specific keys
// ========================================
type Role = 'admin' | 'user' | 'guest';
type Permissions = Record<Role, string[]>;

const permissions: Permissions = {
  admin: ['read', 'write', 'delete'],
  user: ['read', 'write'],
  guest: ['read'],
};

// Ensure all roles have permissions defined
type HttpMethod = 'GET' | 'POST' | 'PUT' | 'DELETE';
type Handlers = Record<HttpMethod, (req: Request) => Response>;

// ========================================
// ReturnType<T> - Extract return type
// ========================================
function getUser() {
  return { id: 1, name: 'Alice' };
}

type User2 = ReturnType<typeof getUser>;
// Result: { id: number; name: string; }

// ========================================
// Parameters<T> - Extract parameter types as tuple
// ========================================
function createUser(name: string, age: number, email: string) {
  // ...
}

type CreateUserParams = Parameters<typeof createUser>;
// Result: [string, number, string]

// ========================================
// Awaited<T> - Extract promised type
// ========================================
async function fetchUser(): Promise<User> {
  // ...
}

type FetchedUser = Awaited<ReturnType<typeof fetchUser>>;
// Result: User

// ========================================
// NonNullable<T> - Remove null and undefined
// ========================================
type MaybeString = string | null | undefined;
type DefiniteString = NonNullable<MaybeString>;
// Result: string

// ========================================
// Combining Utility Types
// ========================================
// Create update DTO without ID
type UpdateUserDto = Partial<Omit<User, 'id'>>;

// Make specific fields required in partial type
type UserRegistration = Required<Pick<User, 'name' | 'email'>> & 
                       Partial<Omit<User, 'name' | 'email'>>;

// Deep readonly
type DeepReadonly<T> = {
  readonly [K in keyof T]: T[K] extends object
    ? DeepReadonly<T[K]>
    : T[K];
};
```

### 1.5 Type Narrowing Techniques

```typescript
// ========================================
// typeof type guards
// ========================================
function process(value: string | number) {
  if (typeof value === 'string') {
    return value.toUpperCase(); // TypeScript knows it's string
  }
  return value.toFixed(2); // TypeScript knows it's number
}

// ========================================
// instanceof type guards
// ========================================
class Dog {
  bark() { console.log('Woof!'); }
}

class Cat {
  meow() { console.log('Meow!'); }
}

function handlePet(pet: Dog | Cat) {
  if (pet instanceof Dog) {
    pet.bark(); // TypeScript knows it's Dog
  } else {
    pet.meow(); // TypeScript knows it's Cat
  }
}

// ========================================
// in operator type guards
// ========================================
type Fish = { swim: () => void };
type Bird = { fly: () => void };

function move(animal: Fish | Bird) {
  if ('swim' in animal) {
    animal.swim(); // TypeScript knows it's Fish
  } else {
    animal.fly(); // TypeScript knows it's Bird
  }
}

// ========================================
// Custom type guards (predicate functions)
// ========================================
interface User {
  type: 'user';
  name: string;
}

interface Admin {
  type: 'admin';
  name: string;
  permissions: string[];
}

// Type predicate: `arg is Type`
function isAdmin(user: User | Admin): user is Admin {
  return user.type === 'admin';
}

function greet(user: User | Admin) {
  if (isAdmin(user)) {
    console.log(`Admin ${user.name} with ${user.permissions.length} permissions`);
  } else {
    console.log(`User ${user.name}`);
  }
}

// ========================================
// Assertion functions
// ========================================
function assertIsDefined<T>(value: T): asserts value is NonNullable<T> {
  if (value === null || value === undefined) {
    throw new Error('Value must be defined');
  }
}

function processUser(user: User | null) {
  assertIsDefined(user);
  // After assertion, TypeScript knows user is not null
  console.log(user.name);
}

// ========================================
// Discriminated union narrowing
// ========================================
type Shape =
  | { kind: 'circle'; radius: number }
  | { kind: 'rectangle'; width: number; height: number }
  | { kind: 'square'; size: number };

function area(shape: Shape): number {
  switch (shape.kind) {
    case 'circle':
      return Math.PI * shape.radius ** 2;
    case 'rectangle':
      return shape.width * shape.height;
    case 'square':
      return shape.size ** 2;
    default:
      const _exhaustive: never = shape;
      throw new Error('Unhandled shape');
  }
}

// ========================================
// Truthiness narrowing
// ========================================
function printLength(str: string | null | undefined) {
  // Truthy check narrows away null and undefined
  if (str) {
    console.log(str.length);
  }
}

// ========================================
// Equality narrowing
// ========================================
function example(x: string | number, y: string | boolean) {
  if (x === y) {
    // TypeScript narrows both to string (only common type)
    x.toUpperCase();
    y.toUpperCase();
  }
}

// ========================================
// Control flow analysis
// ========================================
function parseInput(input: string | null): string {
  if (!input) {
    throw new Error('Invalid input');
  }
  // TypeScript knows input is string here due to control flow
  return input.toUpperCase();
}
```

---

## 2. Configuration & Tooling

### 2.1 Modern tsconfig.json Strict Mode (2025)

**Recommended configuration for new projects:**

```json
{
  "compilerOptions": {
    // ========================================
    // Core Strict Options
    // ========================================
    "strict": true,  // Enables all strict family options below:
    // - strictNullChecks
    // - noImplicitAny
    // - noImplicitThis
    // - strictFunctionTypes
    // - strictBindCallApply
    // - strictPropertyInitialization
    // - alwaysStrict
    // - useUnknownInCatchVariables

    // ========================================
    // Additional Strictness (NOT in strict: true)
    // ========================================
    "noUncheckedIndexedAccess": true,
    "exactOptionalPropertyTypes": true,
    "noPropertyAccessFromIndexSignature": true,

    // ========================================
    // Code Quality
    // ========================================
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "noImplicitReturns": true,
    "noFallthroughCasesInSwitch": true,
    "noImplicitOverride": true,
    "allowUnusedLabels": false,
    "allowUnreachableCode": false,

    // ========================================
    // Module System (2025)
    // ========================================
    "module": "ESNext",
    "moduleResolution": "bundler",  // New in TS 5.6 for modern bundlers
    "esModuleInterop": true,
    "allowSyntheticDefaultImports": true,
    "resolveJsonModule": true,
    "isolatedModules": true,

    // ========================================
    // Type Checking
    // ========================================
    "skipLibCheck": true,  // Skip checking .d.ts files (faster builds)
    "forceConsistentCasingInFileNames": true,

    // ========================================
    // Emit
    // ========================================
    "target": "ES2022",  // Modern JavaScript
    "lib": ["ES2022", "DOM", "DOM.Iterable"],
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "removeComments": false,

    // ========================================
    // Interop
    // ========================================
    "allowJs": false,  // Pure TypeScript
    "checkJs": false
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist", "**/*.spec.ts"]
}
```

### 2.2 Important Compiler Options Explained

#### `noUncheckedIndexedAccess` ⭐

**What it does:** Adds `undefined` to any undeclared key in objects or array indices.

```typescript
// Without noUncheckedIndexedAccess
const arr = [1, 2, 3];
const item = arr[10]; // Type: number (WRONG - could be undefined!)

// With noUncheckedIndexedAccess
const arr = [1, 2, 3];
const item = arr[10]; // Type: number | undefined (CORRECT!)

if (item !== undefined) {
  console.log(item * 2); // Safe usage
}

// Object access
type Config = Record<string, string>;
const config: Config = { apiKey: 'secret' };

// Without flag: Type is string
// With flag: Type is string | undefined
const timeout = config['timeout']; // string | undefined

// Forces safe access patterns
const timeout2 = config['timeout'] ?? '5000'; // Provide default
```

**Why it matters:** Prevents runtime errors from accessing non-existent array indices or object properties.

**Recommendation:** Enable for all new projects. Existing projects may require significant refactoring.

#### `exactOptionalPropertyTypes` ⭐

**What it does:** Distinguishes between optional properties and properties that can be undefined.

```typescript
interface User {
  name: string;
  age?: number;  // Optional property
}

// Without exactOptionalPropertyTypes
const user1: User = { name: 'Alice', age: undefined }; // ✅ Allowed
const user2: User = { name: 'Bob' }; // ✅ Allowed

// With exactOptionalPropertyTypes
const user1: User = { name: 'Alice', age: undefined }; // ❌ Error!
const user2: User = { name: 'Bob' }; // ✅ Allowed

// If you want to allow undefined, be explicit:
interface User2 {
  name: string;
  age?: number | undefined;  // Now undefined is explicit
}
```

**Why it matters:** 
- Makes the distinction between "property not present" vs "property is undefined"
- Prevents bugs where `undefined` is explicitly set vs property being absent
- Useful for JSON serialization (undefined properties are omitted)

**Recommendation:** Enable for new projects. Existing projects need careful migration.

#### `noPropertyAccessFromIndexSignature`

**What it does:** Requires bracket notation for index signature properties.

```typescript
interface Config {
  apiKey: string;  // Known property
  [key: string]: string;  // Index signature
}

const config: Config = {
  apiKey: 'secret',
  timeout: '5000',
};

// Without noPropertyAccessFromIndexSignature
const key = config.apiKey;  // ✅ OK
const timeout = config.timeout;  // ✅ OK

// With noPropertyAccessFromIndexSignature
const key = config.apiKey;  // ✅ OK (known property)
const timeout = config.timeout;  // ❌ Error - use bracket notation
const timeout2 = config['timeout'];  // ✅ OK
```

**Why it matters:** Discourages overuse of index signatures, encourages defining keys explicitly.

**Recommendation:** Enable to encourage better type definitions.

### 2.3 Linters and Formatters Integration (2025)

**Recommended toolchain:**

```json
// package.json
{
  "devDependencies": {
    "typescript": "^5.7.0",
    "@typescript-eslint/parser": "^8.0.0",
    "@typescript-eslint/eslint-plugin": "^8.0.0",
    "eslint": "^9.0.0",
    "prettier": "^3.0.0",
    "eslint-config-prettier": "^9.0.0"
  }
}
```

**ESLint configuration (.eslintrc.json):**

```json
{
  "parser": "@typescript-eslint/parser",
  "parserOptions": {
    "project": "./tsconfig.json"
  },
  "plugins": ["@typescript-eslint"],
  "extends": [
    "eslint:recommended",
    "plugin:@typescript-eslint/recommended",
    "plugin:@typescript-eslint/recommended-requiring-type-checking",
    "prettier"
  ],
  "rules": {
    // Prevent 'any' usage
    "@typescript-eslint/no-explicit-any": "error",
    "@typescript-eslint/no-unsafe-assignment": "error",
    "@typescript-eslint/no-unsafe-call": "error",
    "@typescript-eslint/no-unsafe-member-access": "error",
    "@typescript-eslint/no-unsafe-return": "error",

    // Enforce type safety
    "@typescript-eslint/explicit-function-return-type": "warn",
    "@typescript-eslint/explicit-module-boundary-types": "warn",
    "@typescript-eslint/no-unused-vars": ["error", {
      "argsIgnorePattern": "^_",
      "varsIgnorePattern": "^_"
    }],

    // Code quality
    "@typescript-eslint/no-floating-promises": "error",
    "@typescript-eslint/await-thenable": "error",
    "@typescript-eslint/no-misused-promises": "error",
    
    // Style
    "@typescript-eslint/consistent-type-definitions": ["error", "interface"],
    "@typescript-eslint/consistent-type-imports": ["error", {
      "prefer": "type-imports"
    }]
  }
}
```

**Prettier configuration (.prettierrc.json):**

```json
{
  "semi": true,
  "trailingComma": "es5",
  "singleQuote": true,
  "printWidth": 100,
  "tabWidth": 2,
  "arrowParens": "always"
}
```

**VS Code Settings (settings.json):**

```json
{
  "editor.defaultFormatter": "esbenp.prettier-vscode",
  "editor.formatOnSave": true,
  "editor.codeActionsOnSave": {
    "source.fixAll.eslint": true,
    "source.organizeImports": true
  },
  "typescript.tsdk": "node_modules/typescript/lib",
  "typescript.enablePromptUseWorkspaceTsdk": true
}
```

---

## 3. Code Organization

### 3.1 Module Organization Patterns

**Feature-based structure (recommended for medium-large apps):**

```
src/
├── features/
│   ├── auth/
│   │   ├── types.ts          # Feature-specific types
│   │   ├── api.ts            # API calls
│   │   ├── hooks.ts          # React hooks (if applicable)
│   │   ├── utils.ts          # Feature utilities
│   │   └── index.ts          # Public API only
│   ├── users/
│   │   ├── types.ts
│   │   ├── api.ts
│   │   ├── components/
│   │   └── index.ts
│   └── products/
│       └── ...
├── shared/
│   ├── types/               # Shared type definitions
│   │   ├── api.ts
│   │   ├── models.ts
│   │   └── index.ts
│   ├── utils/              # Shared utilities
│   │   ├── string.ts
│   │   ├── date.ts
│   │   └── index.ts
│   └── constants/
│       └── index.ts
├── lib/                    # External library wrappers
│   ├── api-client.ts
│   └── logger.ts
└── types/                  # Global type augmentations
    └── global.d.ts
```

**Layer-based structure (recommended for smaller apps):**

```
src/
├── models/         # Domain types
│   ├── User.ts
│   ├── Product.ts
│   └── index.ts
├── services/       # Business logic
│   ├── UserService.ts
│   └── index.ts
├── repositories/   # Data access
│   ├── UserRepository.ts
│   └── index.ts
├── controllers/    # Request handlers
│   ├── UserController.ts
│   └── index.ts
└── utils/          # Helpers
    └── index.ts
```

### 3.2 Import/Export Best Practices

```typescript
// ========================================
// Named exports (preferred)
// ========================================
// user.ts
export interface User {
  id: number;
  name: string;
}

export function createUser(name: string): User {
  return { id: Date.now(), name };
}

export class UserService {
  // ...
}

// Importing
import { User, createUser, UserService } from './user';

// ========================================
// Default exports (use sparingly)
// ========================================
// Use only for single primary export
// UserService.ts
export default class UserService {
  // ...
}

// Importing
import UserService from './UserService';

// ========================================
// Type-only imports (TS 3.8+)
// ========================================
// Ensures types are erased at runtime
import type { User, Product } from './models';
import { createUser } from './models'; // Runtime import

// Re-exporting types
export type { User, Product } from './models';

// ========================================
// Namespace imports (avoid unless necessary)
// ========================================
import * as userUtils from './user-utils';
userUtils.validateEmail('test@example.com');

// Better: named imports
import { validateEmail, formatName } from './user-utils';

// ========================================
// Dynamic imports
// ========================================
// Code splitting
async function loadUserModule() {
  const { UserService } = await import('./UserService');
  return new UserService();
}

// Conditional imports
if (process.env.NODE_ENV === 'development') {
  const devTools = await import('./dev-tools');
  devTools.initialize();
}
```

### 3.3 Barrel Files - When to Use/Avoid ⚠️

**What are barrel files?**
A barrel file (typically `index.ts`) re-exports modules from a directory:

```typescript
// features/auth/index.ts (barrel file)
export { login, logout } from './api';
export { useAuth } from './hooks';
export type { User, AuthState } from './types';
```

**Benefits:**
- Clean import statements: `import { login, useAuth } from '@/features/auth'`
- Single entry point for feature
- Easier refactoring (internal paths hidden)

**Problems (2025 findings):**

1. **Build Performance Impact:**
   - Atlassian: 75% faster builds after removing barrel files
   - Next.js project: 68% reduction in loaded modules (11k → 3.5k)
   - Bundlers process entire barrel even for single import

2. **Circular Dependencies:**
   - Barrel files increase risk of circular imports
   - Can cause runtime errors or undefined imports

3. **Tree-shaking Issues:**
   - Importing through barrels can prevent dead code elimination
   - All barrel exports may be included in bundle

**2025 Recommendations:**

✅ **Use barrel files for:**
- Library entry points (required for package.json main/exports)
- Public APIs that consumers import from
- Small, cohesive feature modules (<10 exports)

```typescript
// ✅ Good: Library entry point
// my-library/index.ts
export { Button } from './components/Button';
export { Input } from './components/Input';
export type { ButtonProps, InputProps } from './types';
```

❌ **Avoid barrel files for:**
- Internal application directories
- Large directories with many exports
- Deep directory trees
- Performance-critical applications

```typescript
// ❌ Bad: Internal barrel with many exports
// src/components/index.ts
export * from './Button';
export * from './Input';
export * from './Modal';
// ... 50 more components

// Better: Direct imports
import { Button } from '@/components/Button';
import { Input } from '@/components/Input';
```

**Alternative patterns:**

```typescript
// 1. Path aliases (tsconfig.json)
{
  "compilerOptions": {
    "baseUrl": ".",
    "paths": {
      "@/components/*": ["src/components/*"],
      "@/features/*": ["src/features/*"]
    }
  }
}

// Usage: Direct imports with clean paths
import { Button } from '@/components/Button';
import { login } from '@/features/auth/api';

// 2. Explicit exports (for small modules)
// features/auth/public-api.ts
export { login, logout } from './api';
export { useAuth } from './hooks';
// Only exports what's intended to be public

// 3. No barrel files at all
// Just use direct imports throughout
```

**Detection and mitigation:**
```bash
# Find circular dependencies
npm install -g madge
madge --circular --extensions ts,tsx src/

# ESLint rule to catch cycles
# .eslintrc.json
{
  "rules": {
    "import/no-cycle": ["error", { "maxDepth": 1 }]
  }
}
```

---

## 4. Common Patterns & Anti-patterns

### 4.1 Type Assertion vs Type Guards

**Type Assertions (as / <>):**

```typescript
// Type assertion: "Trust me, I know this type"
const input = document.getElementById('myInput') as HTMLInputElement;
input.value = 'Hello'; // No runtime check!

// Angle bracket syntax (avoid in .tsx files)
const input2 = <HTMLInputElement>document.getElementById('myInput');

// ⚠️ Problem: No runtime validation
const data: any = JSON.parse(apiResponse);
const user = data as User; // Dangerous! data might not be a User
console.log(user.name.toUpperCase()); // Runtime error if data is wrong
```

**When to use assertions:**
- You have information TypeScript doesn't (DOM APIs, type-safe libraries)
- After validation (e.g., schema validation)
- Non-null assertions when you're certain

```typescript
// ✅ Acceptable: After DOM API (you know the element exists)
const button = document.querySelector('#submit-btn') as HTMLButtonElement;

// ✅ Acceptable: After validation
function parseUser(data: unknown): User {
  // Validate data structure
  if (
    typeof data === 'object' &&
    data !== null &&
    'name' in data &&
    typeof data.name === 'string'
  ) {
    return data as User; // Safe after validation
  }
  throw new Error('Invalid user data');
}

// ✅ Non-null assertion when certain
function processArray(arr: string[] | null) {
  if (!arr) return;
  const first = arr![0]; // We know arr is not null here
}
```

**Type Guards (preferred):**

```typescript
// ✅ Runtime check with type narrowing
function isUser(data: unknown): data is User {
  return (
    typeof data === 'object' &&
    data !== null &&
    'id' in data &&
    typeof (data as any).id === 'number' &&
    'name' in data &&
    typeof (data as any).name === 'string'
  );
}

const data: unknown = JSON.parse(apiResponse);
if (isUser(data)) {
  console.log(data.name.toUpperCase()); // Safe!
} else {
  console.error('Invalid user data');
}

// ✅ With libraries: Zod, Yup, io-ts
import { z } from 'zod';

const UserSchema = z.object({
  id: z.number(),
  name: z.string(),
  email: z.string().email(),
});

type User = z.infer<typeof UserSchema>;

function parseUserData(data: unknown): User {
  return UserSchema.parse(data); // Runtime validation + type safety
}
```

**Best Practice:**
- Prefer type guards over assertions
- Use assertions only after validation
- Consider schema validation libraries for API data

### 4.2 `any` vs `unknown` vs `never`

#### `any` - The Escape Hatch (avoid)

```typescript
// ❌ Disables all type checking
let data: any = { name: 'Alice' };
data.doesNotExist(); // No error, runtime crash!
data = 42;
data = 'string';
const value: string = data; // No error

// ⚠️ Only acceptable uses:
// 1. Gradual migration from JavaScript
// 2. Truly dynamic data (rare)
// 3. Prototype/POC code (temporary)
```

#### `unknown` - Type-safe any (preferred)

```typescript
// ✅ Requires type checking before use
let data: unknown = { name: 'Alice' };

// data.name; // ❌ Error: Object is of type 'unknown'

// Must narrow type first
if (typeof data === 'object' && data !== null && 'name' in data) {
  console.log((data as { name: string }).name); // ✅
}

// Common pattern: unknown → validation → typed
function processData(input: unknown): void {
  // Validate structure
  if (isValidData(input)) {
    // Now TypeScript knows the type
    console.log(input.property);
  }
}

// ✅ Use for error handling
try {
  throw new Error('Something went wrong');
} catch (err: unknown) {
  // Must check type before using
  if (err instanceof Error) {
    console.error(err.message);
  } else {
    console.error('Unknown error:', err);
  }
}
```

#### `never` - The Impossible Type

```typescript
// 1. Functions that never return
function throwError(message: string): never {
  throw new Error(message);
  // No return statement - execution never continues
}

function infiniteLoop(): never {
  while (true) {
    // ...
  }
}

// 2. Exhaustiveness checking
type Status = 'success' | 'error' | 'pending';

function handleStatus(status: Status): string {
  switch (status) {
    case 'success':
      return 'Done!';
    case 'error':
      return 'Failed!';
    case 'pending':
      return 'In progress...';
    default:
      // If all cases handled, status is type 'never' here
      const _exhaustive: never = status;
      throw new Error(`Unhandled status: ${_exhaustive}`);
  }
}

// If you add a new status but forget to handle it:
type Status2 = 'success' | 'error' | 'pending' | 'cancelled';
// TypeScript error at _exhaustive assignment! ✅

// 3. Filtering union types
type NonNullable<T> = T extends null | undefined ? never : T;
type Clean = NonNullable<string | null | undefined>; // string

// 4. Impossible intersections
type Impossible = string & number; // never (no value can be both)

// 5. Empty unions disappear
type Example = 'a' | 'b' | never; // Simplifies to 'a' | 'b'
```

**Summary Table:**

| Type | Type Safety | Use Case |
|------|-------------|----------|
| `any` | ❌ None | Avoid. Only for migration/prototypes |
| `unknown` | ✅ Full | External data, dynamic values, error handling |
| `never` | ✅ Full | Exhaustiveness, impossible states, errors |

### 4.3 Readonly Patterns

```typescript
// ========================================
// Readonly properties
// ========================================
interface User {
  readonly id: number;
  name: string;
}

const user: User = { id: 1, name: 'Alice' };
// user.id = 2; // ❌ Error: Cannot assign to 'id'
user.name = 'Bob'; // ✅ OK

// ========================================
// Readonly utility type
// ========================================
interface MutableConfig {
  apiKey: string;
  timeout: number;
}

const config: Readonly<MutableConfig> = {
  apiKey: 'secret',
  timeout: 5000,
};

// config.timeout = 10000; // ❌ Error

// ========================================
// Readonly arrays
// ========================================
const numbers: readonly number[] = [1, 2, 3];
const alsoNumbers: ReadonlyArray<number> = [1, 2, 3];

// numbers.push(4); // ❌ Error: Property 'push' does not exist
// numbers[0] = 99; // ❌ Error: Index signature is readonly

// ✅ Allowed: non-mutating methods
const doubled = numbers.map((n) => n * 2);
const firstThree = numbers.slice(0, 3);

// ========================================
// as const - Deep readonly + literal types
// ========================================
const config2 = {
  apiKey: 'secret',
  timeout: 5000,
  endpoints: ['api.example.com', 'backup.example.com'],
} as const;

// Result type:
// {
//   readonly apiKey: "secret";        // Not string, but literal!
//   readonly timeout: 5000;           // Not number, but literal!
//   readonly endpoints: readonly ["api.example.com", "backup.example.com"];
// }

// config2.timeout = 10000; // ❌ Error
// config2.endpoints.push('new'); // ❌ Error

// ✅ Use case: Configuration objects
const ROUTES = {
  HOME: '/',
  ABOUT: '/about',
  PROFILE: '/profile',
} as const;

type Route = typeof ROUTES[keyof typeof ROUTES];
// Result: "/" | "/about" | "/profile"

// ✅ Use case: Enum-like constants
const STATUS = ['pending', 'approved', 'rejected'] as const;
type Status = typeof STATUS[number]; // "pending" | "approved" | "rejected"

// ========================================
// Deep Readonly (recursive)
// ========================================
type DeepReadonly<T> = {
  readonly [K in keyof T]: T[K] extends object
    ? DeepReadonly<T[K]>
    : T[K];
};

interface NestedConfig {
  database: {
    host: string;
    port: number;
  };
  cache: {
    enabled: boolean;
  };
}

const config3: DeepReadonly<NestedConfig> = {
  database: { host: 'localhost', port: 5432 },
  cache: { enabled: true },
};

// config3.database.port = 3000; // ❌ Error (deep readonly)

// ========================================
// Function parameters (defensive)
// ========================================
function processUsers(users: readonly User[]): User[] {
  // Can't accidentally mutate input
  // users.push({ id: 99, name: 'New' }); // ❌ Error
  
  // Must return new array
  return users.map((user) => ({ ...user, name: user.name.toUpperCase() }));
}

// ========================================
// Why readonly matters
// ========================================

// Without readonly: Risk of mutation bugs
function addUser(users: User[], newUser: User): User[] {
  users.push(newUser); // Mutates input! 🐛
  return users;
}

const originalUsers = [{ id: 1, name: 'Alice' }];
const updatedUsers = addUser(originalUsers, { id: 2, name: 'Bob' });
// originalUsers is now mutated!

// With readonly: Compiler prevents mutation
function addUserSafe(users: readonly User[], newUser: User): User[] {
  // users.push(newUser); // ❌ Compiler error
  return [...users, newUser]; // ✅ Must create new array
}
```

**Best practices:**
- Use `readonly` for properties that shouldn't change
- Use `as const` for constant configuration objects
- Use `readonly` array parameters to prevent accidental mutations
- Consider deep readonly for complex nested structures

### 4.4 Error Handling Patterns (2025)

#### Traditional try-catch

```typescript
// Basic error handling
async function fetchUser(id: number): Promise<User> {
  try {
    const response = await fetch(`/api/users/${id}`);
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
  } catch (err: unknown) {
    // TypeScript 4.0+: catch variables are unknown by default
    if (err instanceof Error) {
      console.error('Failed to fetch user:', err.message);
    }
    throw err; // Re-throw or handle
  }
}
```

#### Result Type Pattern (Functional Approach)

**Inspired by Rust, popularized in TypeScript 2025:**

```typescript
// Result type definition
type Result<T, E = Error> =
  | { success: true; value: T }
  | { success: false; error: E };

// Helper functions
function ok<T>(value: T): Result<T> {
  return { success: true, value };
}

function err<E>(error: E): Result<never, E> {
  return { success: false, error };
}

// Usage
async function fetchUserSafe(id: number): Promise<Result<User>> {
  try {
    const response = await fetch(`/api/users/${id}`);
    if (!response.ok) {
      return err(new Error(`HTTP ${response.status}`));
    }
    const data = await response.json();
    return ok(data);
  } catch (error) {
    return err(error instanceof Error ? error : new Error('Unknown error'));
  }
}

// Consumer code - explicit error handling
const result = await fetchUserSafe(123);
if (result.success) {
  console.log('User:', result.value.name);
} else {
  console.error('Error:', result.error.message);
}

// Chainable operations
async function getUserDisplayName(id: number): Promise<Result<string>> {
  const userResult = await fetchUserSafe(id);
  if (!userResult.success) {
    return err(userResult.error);
  }
  
  const user = userResult.value;
  return ok(`${user.name} (${user.email})`);
}
```

#### Using Libraries (2025 Recommendations)

**neverthrow - Most popular:**

```typescript
import { Result, ok, err, ResultAsync } from 'neverthrow';

// Sync operations
function divide(a: number, b: number): Result<number, string> {
  if (b === 0) {
    return err('Division by zero');
  }
  return ok(a / b);
}

const result = divide(10, 2)
  .map((value) => value * 2)  // Chain operations
  .mapErr((error) => `Error: ${error}`);

if (result.isOk()) {
  console.log(result.value); // 10
}

// Async operations
const fetchUser = ResultAsync.fromPromise(
  fetch('/api/user').then((res) => res.json()),
  (error) => new Error('Fetch failed')
);

const result = await fetchUser
  .andThen((user) => validateUser(user))
  .map((user) => user.name)
  .mapErr((error) => ({ message: error.message }));

result.match(
  (name) => console.log('User name:', name),
  (error) => console.error('Error:', error.message)
);
```

**When to use Result types:**
- Expected failures (validation, network, business logic)
- Functional programming style
- Explicit error handling requirements
- Railway-oriented programming

**When to use try-catch:**
- Unexpected failures (programmer errors)
- Integration with libraries that throw
- Simple error propagation
- Performance-critical code (try-catch is faster)

#### Custom Error Classes

```typescript
// Base error class
class AppError extends Error {
  constructor(
    message: string,
    public readonly code: string,
    public readonly statusCode: number = 500
  ) {
    super(message);
    this.name = this.constructor.name;
    Error.captureStackTrace(this, this.constructor);
  }
}

// Specific error types
class ValidationError extends AppError {
  constructor(message: string, public readonly fields: string[]) {
    super(message, 'VALIDATION_ERROR', 400);
  }
}

class NotFoundError extends AppError {
  constructor(resource: string, id: string | number) {
    super(`${resource} not found: ${id}`, 'NOT_FOUND', 404);
  }
}

class UnauthorizedError extends AppError {
  constructor(message: string = 'Unauthorized') {
    super(message, 'UNAUTHORIZED', 401);
  }
}

// Usage
function getUser(id: number): User {
  const user = database.findUser(id);
  if (!user) {
    throw new NotFoundError('User', id);
  }
  return user;
}

// Error handling middleware (Express example)
app.use((err: Error, req, res, next) => {
  if (err instanceof AppError) {
    res.status(err.statusCode).json({
      error: {
        code: err.code,
        message: err.message,
      },
    });
  } else {
    res.status(500).json({
      error: {
        code: 'INTERNAL_ERROR',
        message: 'An unexpected error occurred',
      },
    });
  }
});
```

**Best practices:**
- Use `unknown` for catch variables (TS 4.0+)
- Create custom error classes for domain errors
- Consider Result types for expected failures
- Use error boundaries in React applications
- Log errors with context (user ID, request ID, etc.)

---

## 5. Modern Features (2024-2025)

### 5.1 TypeScript 5.x Features

#### Decorators (TS 5.0 - Stage 3 ECMAScript)

**Major overhaul in TypeScript 5.0 - now aligned with ECMAScript standard**

**What changed:**
- Previous: `--experimentalDecorators` (legacy, TypeScript-specific)
- Now: Standard decorators (Stage 3 proposal, enabled by default)
- Better type inference and checking

**Class Decorators:**

```typescript
// Decorator function signature
type ClassDecorator = (
  target: Function,
  context: ClassDecoratorContext
) => Function | void;

// Example: Add metadata to class
function logClass<T extends { new (...args: any[]): {} }>(
  target: T,
  context: ClassDecoratorContext
) {
  console.log(`Class registered: ${context.name}`);
  
  // Return enhanced class
  return class extends target {
    constructor(...args: any[]) {
      super(...args);
      console.log(`Instance of ${context.name} created`);
    }
  };
}

@logClass
class UserService {
  constructor(private apiKey: string) {}
}

const service = new UserService('secret');
// Logs: "Class registered: UserService"
// Logs: "Instance of UserService created"
```

**Method Decorators:**

```typescript
// Method decorator signature
type MethodDecorator = (
  target: Function,
  context: ClassMethodDecoratorContext
) => Function | void;

// Example: Measure execution time
function measureTime(
  target: Function,
  context: ClassMethodDecoratorContext
) {
  const methodName = String(context.name);
  
  return function (this: any, ...args: any[]) {
    const start = performance.now();
    const result = target.apply(this, args);
    const end = performance.now();
    console.log(`${methodName} took ${end - start}ms`);
    return result;
  };
}

class DataService {
  @measureTime
  async fetchData(id: number) {
    const response = await fetch(`/api/data/${id}`);
    return response.json();
  }
}

// Example: Memoization decorator
function memoize(
  target: Function,
  context: ClassMethodDecoratorContext
) {
  const cache = new Map<string, any>();
  
  return function (this: any, ...args: any[]) {
    const key = JSON.stringify(args);
    if (cache.has(key)) {
      return cache.get(key);
    }
    const result = target.apply(this, args);
    cache.set(key, result);
    return result;
  };
}

class Calculator {
  @memoize
  expensiveCalculation(n: number): number {
    console.log('Computing...');
    return n * n;
  }
}

const calc = new Calculator();
calc.expensiveCalculation(5); // Logs: "Computing..."
calc.expensiveCalculation(5); // Uses cache, no log
```

**Property Decorators:**

```typescript
// Validation decorator
function validate(
  target: undefined,
  context: ClassFieldDecoratorContext
) {
  return function (this: any, initialValue: any) {
    let value = initialValue;
    
    return {
      get() {
        return value;
      },
      set(newValue: string) {
        if (typeof newValue !== 'string' || newValue.length < 3) {
          throw new Error(`${String(context.name)} must be at least 3 characters`);
        }
        value = newValue;
      },
    };
  };
}

class User {
  @validate
  username!: string;
  
  constructor(username: string) {
    this.username = username;
  }
}

const user = new User('alice'); // ✅ OK
// const invalid = new User('ab'); // ❌ Throws error
```

**Decorator Factories:**

```typescript
// Parameterized decorators
function log(prefix: string) {
  return function (
    target: Function,
    context: ClassMethodDecoratorContext
  ) {
    const methodName = String(context.name);
    
    return function (this: any, ...args: any[]) {
      console.log(`${prefix}: ${methodName}`, args);
      return target.apply(this, args);
    };
  };
}

class ApiClient {
  @log('API Call')
  async get(url: string) {
    // ...
  }
  
  @log('API Call')
  async post(url: string, data: any) {
    // ...
  }
}
```

**Common Use Cases:**
- Dependency injection frameworks
- ORM model decorators (TypeORM, Prisma)
- Validation (class-validator)
- Metadata reflection
- Logging and monitoring
- Authorization checks

**Migration from experimental decorators:**

```typescript
// Old (--experimentalDecorators)
function OldDecorator(target: any, propertyKey: string) {
  // ...
}

// New (TS 5.0+)
function NewDecorator(
  target: undefined,
  context: ClassFieldDecoratorContext
) {
  // ...
}
```

**tsconfig.json:**

```json
{
  "compilerOptions": {
    // New decorators (default in TS 5.0+)
    "experimentalDecorators": false,
    
    // Or use legacy decorators (for compatibility)
    // "experimentalDecorators": true
  }
}
```

#### Other TypeScript 5.x Features

**TS 5.0 - const type parameters:**

```typescript
// Preserve literal types in generics
function makeArray<const T>(values: T[]): T[] {
  return values;
}

const arr = makeArray(['a', 'b']); // Type: ("a" | "b")[]
// Without const: Type would be string[]
```

**TS 5.1 - Easier implicit returns for undefined:**

```typescript
// Now allowed (previously required explicit return)
function logMessage(msg: string): undefined {
  console.log(msg);
  // No return statement needed
}
```

**TS 5.2 - using declarations (explicit resource management):**

```typescript
{
  using file = await openFile('data.txt');
  // file is automatically disposed at end of scope
  const content = file.read();
} // file.dispose() called automatically
```

**TS 5.4 - NoInfer utility type:**

```typescript
// Prevent type inference in specific positions
function createStreetLight<C extends string>(
  colors: C[],
  defaultColor?: NoInfer<C>
) {
  // ...
}

createStreetLight(['red', 'yellow', 'green'], 'red'); // ✅
createStreetLight(['red', 'yellow', 'green'], 'blue'); // ❌ Error
```

**TS 5.6 - Module resolution bundler:**

```json
{
  "compilerOptions": {
    "moduleResolution": "bundler"  // New in 5.6
  }
}
```

Optimized for modern bundlers (Vite, Webpack, esbuild).

### 5.2 Widely Adopted Patterns (2024-2025)

#### Zod for Runtime Validation

```typescript
import { z } from 'zod';

// Define schema
const UserSchema = z.object({
  id: z.number().positive(),
  name: z.string().min(1),
  email: z.string().email(),
  age: z.number().int().min(0).optional(),
  role: z.enum(['admin', 'user', 'guest']),
});

// Infer TypeScript type from schema
type User = z.infer<typeof UserSchema>;
// Result: {
//   id: number;
//   name: string;
//   email: string;
//   age?: number | undefined;
//   role: "admin" | "user" | "guest";
// }

// Runtime validation
function createUser(data: unknown): User {
  return UserSchema.parse(data); // Throws if invalid
}

// Safe parsing (returns Result-like)
const result = UserSchema.safeParse(data);
if (result.success) {
  console.log(result.data); // Typed as User
} else {
  console.error(result.error); // ZodError with details
}
```

#### Template Literal Types

```typescript
// Type-safe string combinations
type HTTPMethod = 'GET' | 'POST' | 'PUT' | 'DELETE';
type Endpoint = '/users' | '/posts' | '/comments';
type APIRoute = `${HTTPMethod} ${Endpoint}`;

// Result: "GET /users" | "GET /posts" | ... (12 combinations)

const route: APIRoute = 'GET /users'; // ✅
// const invalid: APIRoute = 'PATCH /users'; // ❌

// String manipulation types
type Getters<T> = {
  [K in keyof T as `get${Capitalize<string & K>}`]: () => T[K];
};

interface User {
  name: string;
  age: number;
}

type UserGetters = Getters<User>;
// Result: {
//   getName: () => string;
//   getAge: () => number;
// }

// Path type safety
type Path = 'home' | 'about' | 'contact';
type PathParam = `/${Path}`;
type SubPath = 'profile' | 'settings';
type FullPath = PathParam | `${PathParam}/${SubPath}`;

const validPath: FullPath = '/home/profile'; // ✅
```

#### Satisfies Operator (TS 4.9)

```typescript
// Problem: as const loses flexibility
const routes1 = {
  home: '/',
  about: '/about',
  contact: '/contact',
} as const;
// Type: { readonly home: "/"; readonly about: "/about"; ... }
// Can't verify it satisfies a type

// Solution: satisfies operator
type Route = Record<string, `/${string}`>;

const routes2 = {
  home: '/',
  about: '/about',
  contact: '/contact',
  // invalid: 'no-slash', // ❌ Error: doesn't satisfy Route
} satisfies Route;

// Type is preserved (not widened to Route)
routes2.home; // Type: "/" (literal, not string)

// Benefits: Type checking + precise inference
const colors = {
  red: [255, 0, 0],
  green: '#00ff00',
  blue: { r: 0, g: 0, b: 255 },
} satisfies Record<string, string | number[] | { r: number; g: number; b: number }>;

colors.red; // Type: [number, number, number] (not union type!)
colors.green; // Type: string
```

---

## Summary: Quick Reference for AI Agents

**Type System:**
- Prefer `interface` for objects, `type` for unions/complex types
- Use discriminated unions for state machines
- Apply branded types for domain-specific primitives
- Leverage conditional types for type transformations

**Configuration:**
- Enable `strict: true` + `noUncheckedIndexedAccess` + `exactOptionalPropertyTypes`
- Use `moduleResolution: "bundler"` for modern apps
- Configure ESLint with TypeScript rules

**Code Organization:**
- Use feature-based structure for larger apps
- Avoid barrel files except for library entry points
- Prefer named exports and direct imports

**Patterns:**
- Use type guards over type assertions
- Prefer `unknown` over `any`
- Use `never` for exhaustiveness checks
- Apply `readonly` to prevent mutations
- Consider Result types for expected errors

**Modern Features:**
- Use TS 5.0+ decorators for metadata/DI
- Apply Zod for runtime validation
- Leverage template literal types for string patterns
- Use `satisfies` for type checking without widening

---

**Sources:**
- TypeScript Official Documentation (2025)
- DEV Community: "TypeScript Best Practices in 2025"
- DEV Community: "TypeScript Advanced Patterns: Writing Cleaner & Safer Code in 2025"
- whatislove.dev: "The Strictest TypeScript Config"
- 2ality.com: "The bottom type never in TypeScript"
- Atlassian Engineering: "75% Faster Builds by Removing Barrel Files"
- Multiple 2025 technical articles on TypeScript patterns

**Generated:** 2025-11-04 by Claude Code Research Agent
