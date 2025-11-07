---
name: react
description: Build client-side React applications (SPAs) with declarative components, Zustand/XState for state, and TanStack Query for remote data. Use when writing React code, designing component architecture, or managing application state. References the typescript skill for type patterns.
---

# React Expert

Build modern React SPAs where components are thin, declarative UI layers that consume state from external stores (Zustand, XState) and remote data from TanStack Query.

## When to Use This Skill

Use this skill when:
- Writing or refactoring React components
- Designing state management architecture
- Building SPAs with client-side rendering
- Optimizing React performance
- Setting up testing for components and state

**Note:** For general TypeScript type system features, use the `typescript` skill. This skill focuses on React-specific patterns and architecture.

## Core Principles

### 1. Component Architecture Philosophy

**Components are declarative UI layers that consume external state.**

**Pattern:**
```typescript
// ✅ GOOD: Thin component consuming external state
function UserProfile({ userId }: { userId: string }) {
  // External hooks at the top
  const { user, isLoading } = useUser(userId);
  const { updateProfile, deleteAccount } = useUserActions();

  if (isLoading) return <Spinner />;

  // Trivial derived values only
  const displayName = user.firstName + ' ' + user.lastName;

  return (
    <div>
      <h1>{displayName}</h1>
      <p>{user.email}</p>
      {/* Inline actions - no useCallback noise */}
      <button onClick={() => updateProfile({ email: 'new@email.com' })}>
        Update Email
      </button>
      <button onClick={() => deleteAccount(userId)}>
        Delete Account
      </button>
    </div>
  );
}

// ❌ BAD: Logic mixed into component
function UserProfile({ userId }: { userId: string }) {
  const [user, setUser] = useState(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    fetchUser(userId).then(setUser).finally(() => setLoading(false));
  }, [userId]);

  const handleUpdate = useCallback(() => {
    // Complex logic here
  }, [user]);

  // Too much logic in component
}
```

**Rules:**
- External hooks at the top (Zustand, XState, TanStack Query)
- JSX contains only trivial operations (map, filter, simple concatenation)
- **Inline actions** - only abstract if repeated 2+ times in same component
- No useState/useReducer/useEffect for complex logic
- State lives in stores, not components

### 2. State Management Decision Tree

**Critical: useState/useReducer/useEffect are last resorts, not defaults.**

| State Type | Solution | Why |
|------------|----------|-----|
| **Remote (REST API)** | TanStack Query | Caching, deduplication, refetching, testable |
| **Remote (GraphQL)** | Apollo Client | GraphQL-specific features, caching |
| **Application state** | Zustand | Testable, isolated from React lifecycle |
| **Complex state machines** | XState | Explicit states/transitions, visualizable, testable |
| **Truly local UI state** | useState (rare) | Toggle dropdowns, modals - if contained to one component |

**Decision Flow:**

```
Is it remote data?
  → Yes: TanStack Query (or Apollo for GraphQL)

Is it complex state with transitions/guards?
  → Yes: XState

Is it application state (auth, cart, preferences)?
  → Yes: Zustand

Is it trivial UI state (dropdown open/closed) in one component?
  → Maybe: useState (but consider if it should be in a store for testing)
```

**Examples:**

```typescript
// ✅ Remote state: TanStack Query
function useUser(userId: string) {
  return useQuery({
    queryKey: ['user', userId],
    queryFn: () => fetchUser(userId),
    staleTime: 5 * 60 * 1000,
  });
}

// ✅ Application state: Zustand (testable, isolated)
interface AuthStore {
  user: User | null;
  token: string | null;
  login: (credentials: Credentials) => Promise<void>;
  logout: () => void;
}

const useAuthStore = create<AuthStore>((set) => ({
  user: null,
  token: null,
  login: async (credentials) => {
    const { user, token } = await api.login(credentials);
    set({ user, token });
    localStorage.setItem('token', token);
  },
  logout: () => {
    set({ user: null, token: null });
    localStorage.removeItem('token');
  },
}));

// Consume in component (declarative)
function Header() {
  const user = useAuthStore((state) => state.user);
  const logout = useAuthStore((state) => state.logout);

  return (
    <header>
      {user && <span>Welcome, {user.name}</span>}
      <button onClick={() => logout()}>Logout</button>
    </header>
  );
}

// ✅ Complex state: XState
import { createMachine } from 'xstate';

const checkoutMachine = createMachine({
  id: 'checkout',
  initial: 'cart',
  states: {
    cart: {
      on: { PROCEED: 'shipping' }
    },
    shipping: {
      on: {
        NEXT: 'payment',
        BACK: 'cart'
      }
    },
    payment: {
      on: {
        SUBMIT: 'processing',
        BACK: 'shipping'
      }
    },
    processing: {
      on: {
        SUCCESS: 'complete',
        ERROR: 'payment'
      }
    },
    complete: { type: 'final' }
  }
});

// ❌ BAD: useState/useReducer for complex logic
function Checkout() {
  const [step, setStep] = useState('cart');
  const [cart, setCart] = useState([]);
  const [shipping, setShipping] = useState(null);

  // Complex logic mixed into component - hard to test
  const handleNext = () => {
    if (step === 'cart') setStep('shipping');
    else if (step === 'shipping') setStep('payment');
    // ... grows into spaghetti
  };
}
```

**Testing Benefits:**

```typescript
// Zustand store is testable without React
describe('authStore', () => {
  it('logs in user', async () => {
    const store = useAuthStore.getState();
    await store.login({ email: 'test@example.com', password: 'pass' });
    expect(store.user).toBeDefined();
  });
});

// XState machines are testable and visualizable
describe('checkoutMachine', () => {
  it('transitions from cart to shipping', () => {
    const nextState = checkoutMachine.transition('cart', { type: 'PROCEED' });
    expect(nextState.value).toBe('shipping');
  });
});
```

### 3. Performance Guidelines

**Profile first** (React DevTools Profiler), optimize second.

**Optimizations (use sparingly):**

| Technique | When | Avoid |
|-----------|------|-------|
| useMemo | Expensive computations in JSX | Trivial operations |
| useCallback | Repeated 2+ times in component | Every inline action |
| React.memo | Props rarely change | Most cases |
| Code splitting | Route-level | Over-splitting |

```typescript
// ✅ GOOD: Derived value in JSX
function UserList({ users }: { users: User[] }) {
  return (
    <ul>
      {users
        .filter(u => u.active)
        .map(u => <li key={u.id}>{u.name}</li>)}
    </ul>
  );
}

// ⚠️ ONLY if profiling shows issue:
const activeUsers = useMemo(() => users.filter(u => u.active), [users]);

// ✅ GOOD: Inline actions (no useCallback)
<button onClick={() => deleteUser(user.id)}>Delete</button>

// ❌ BAD: Premature useCallback
const handleDelete = useCallback(() => deleteUser(user.id), [user.id]);
<button onClick={handleDelete}>Delete</button>
```

**Code splitting at route level:**

```typescript
const AdminPanel = lazy(() => import('./AdminPanel'));

<Suspense fallback={<Spinner />}>
  <Routes>
    <Route path="/admin" element={<AdminPanel />} />
  </Routes>
</Suspense>
```

### 4. TypeScript Integration Quick Reference

**For advanced TypeScript patterns, use the `typescript` skill.**

```typescript
// Component props
interface ButtonProps {
  children: React.ReactNode;
  variant?: 'primary' | 'secondary';
}

// Extending HTML props
interface InputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  label: string;
}

// Generic components (function syntax required)
function Table<TItem>(props: TableProps<TItem>) { /* ... */ }

// Event handlers
const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => { };
const handleSubmit = (e: React.FormEvent<HTMLFormElement>) => { };

// Refs
const videoRef = useRef<HTMLVideoElement>(null);
```

### 5. Testing Strategy

**Primary: Unit test stores and machines. Secondary: E2E with Playwright.**

**Testing Hierarchy:**

| What to Test | Tool | Why |
|-------------|------|-----|
| **Zustand stores** | Vitest (unit tests) | Fast, isolated, no React |
| **XState machines** | Vitest (unit tests) | Deterministic, visualizable |
| **TanStack Query hooks** | Vitest + MSW | Mock API responses |
| **Critical user flows** | Playwright (E2E) | Real browser, real interactions |
| **Components** | Rarely needed | Logic should be in stores |

**Examples:**

```typescript
// ✅ GOOD: Unit test Zustand store
describe('authStore', () => {
  beforeEach(() => {
    useAuthStore.setState({ user: null, token: null });
  });

  it('logs in user and stores token', async () => {
    const { login } = useAuthStore.getState();
    await login({ email: 'test@example.com', password: 'pass123' });

    const state = useAuthStore.getState();
    expect(state.user).toBeDefined();
    expect(state.token).toBeDefined();
    expect(localStorage.getItem('token')).toBe(state.token);
  });

  it('clears user on logout', () => {
    useAuthStore.setState({ user: mockUser, token: 'abc123' });

    const { logout } = useAuthStore.getState();
    logout();

    const state = useAuthStore.getState();
    expect(state.user).toBeNull();
    expect(localStorage.getItem('token')).toBeNull();
  });
});

// ✅ GOOD: Unit test XState machine
describe('checkoutMachine', () => {
  it('transitions from cart to shipping', () => {
    const nextState = checkoutMachine.transition('cart', { type: 'PROCEED' });
    expect(nextState.value).toBe('shipping');
  });

  it('prevents invalid transitions', () => {
    const nextState = checkoutMachine.transition('cart', { type: 'SUBMIT' });
    expect(nextState.value).toBe('cart'); // No transition
  });
});

// ✅ GOOD: E2E with Playwright
test('user can complete checkout flow', async ({ page }) => {
  await page.goto('/cart');
  await page.getByRole('button', { name: 'Checkout' }).click();

  await page.getByLabel('Address').fill('123 Main St');
  await page.getByRole('button', { name: 'Continue' }).click();

  await page.getByLabel('Card Number').fill('4242424242424242');
  await page.getByRole('button', { name: 'Submit Payment' }).click();

  await expect(page.getByText('Order Complete')).toBeVisible();
});

// ❌ AVOID: React Testing Library for logic
// Logic should be in stores, not components
describe('LoginForm', () => {
  it('validates email', async () => {
    render(<LoginForm />);
    // Testing component logic - should be in store instead
  });
});
```

**Why this approach:**
- Stores are fast to test (no React rendering)
- Stores are isolated (no component coupling)
- Machines are deterministic and visualizable
- E2E tests catch integration issues
- Components become thin UI layers (minimal testing needed)

## Common Anti-Patterns

Avoid these patterns:

| Anti-Pattern | Solution | Why Wrong |
|-------------|----------|-----------|
| Logic in components | Zustand/XState stores | Hard to test, tightly coupled |
| useState for app state | Zustand store | Not isolated from React lifecycle |
| useReducer for complex state | XState machine | No visualization, hard to test transitions |
| Manual API state management | TanStack Query | Missing caching, deduplication, refetching |
| useCallback everywhere | Inline actions | Premature optimization, noisy code |
| Index as key | Unique, stable IDs | Breaks reconciliation |
| Direct state mutation | Immutable updates | React won't detect changes |

```typescript
// ❌ BAD: Logic in component
function UserDashboard() {
  const [user, setUser] = useState(null);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    setLoading(true);
    fetchUser().then(setUser).finally(() => setLoading(false));
  }, []);

  const handleUpdate = useCallback((data) => {
    // Complex logic here
  }, [user]);

  // Hard to test, tightly coupled to React
}

// ✅ GOOD: Store handles logic, component is declarative
function UserDashboard() {
  const { user, isLoading } = useUser();
  const { updateUser } = useUserActions();

  if (isLoading) return <Spinner />;

  return (
    <div>
      <h1>{user.name}</h1>
      <button onClick={() => updateUser({ name: 'New Name' })}>Update</button>
    </div>
  );
}

// ❌ BAD: Direct mutation
items.push(newItem);
setItems(items);

// ✅ GOOD: Immutable
setItems([...items, newItem]);
```

## Quick Reference Patterns

### Component Props
```tsx
// Order: simple → complex (booleans, values, callbacks, render props)
<Table
  data={items}
  loading={isLoading}
  sortable
  onSort={handleSort}
  renderRow={(item) => <Row key={item.id}>{item.name}</Row>}
/>

// Prefer inline for type inference over extracted constants
```

### Pattern Matching
```tsx
// Use ts-pattern for variant rendering
{match(state)
  .with({_tag: "loading"}, () => <Spinner />)
  .with({_tag: "success"}, (s) => <Data value={s.data} />)
  .exhaustive()}
```

### Styling
For CSS-in-JS patterns (styled-components, emotion, etc.), see `references/styling-patterns.md`:
- Theme access patterns
- Component consolidation vs CSS selectors
- Spacing with gap
- Responsive design

## When to Consult Detailed References

**Read these when you need deeper guidance on specific topics:**

### Component Organization (`references/component-patterns.md`)
Read when:
- Organizing components with many props or render props
- Deciding whether to split a large component
- Determining what belongs in components vs hooks vs utils
- Working with discriminated unions and variant rendering

Covers:
- Prop organization (simple→complex, inline vs extracted)
- Component colocation and when to split
- Separation of concerns (components vs hooks vs utils)
- Pattern matching for variants

### Styling Patterns (`references/styling-patterns.md`)
Read when:
- Building complex UI with styled-components (tables, cards, forms)
- Unsure about theme access patterns
- Creating responsive layouts
- Deciding between consolidated styles vs separate components

Covers:
- Theme access (always via props, never direct imports)
- Consolidating styles with CSS selectors
- Spacing preferences (gap over margin)
- Responsive layouts and media queries
- Extending UI library components

### React Best Practices (`references/best-practices-2025.md`)
Read when:
- Setting up state management architecture
- Need comprehensive TypeScript patterns
- Implementing testing strategy
- Choosing between frameworks (Next.js, Remix)
- Performance optimization strategies

Covers:
- Zustand store patterns and organization
- XState machine design and testing
- TanStack Query advanced patterns
- Apollo Client integration
- Testing strategies (unit tests for stores, E2E with Playwright)
- Modern frameworks (Next.js, Remix)
- Performance optimization (useMemo, useCallback, code splitting)

## Quality Checklist

Before completing React code:

**Architecture:**
- [ ] Components are declarative UI (logic in stores/hooks)
- [ ] External hooks at top (Zustand, XState, TanStack Query)
- [ ] No useState/useReducer/useEffect for complex logic
- [ ] Remote data uses TanStack Query (or Apollo)
- [ ] Application state in Zustand stores (testable)
- [ ] Complex state in XState machines (testable, visualizable)

**Component Organization:**
- [ ] Props ordered simple → complex (see `component-patterns.md`)
- [ ] Render props inlined for type inference (extract only if reused)
- [ ] Components colocated, split only when reused
- [ ] Pattern matching used for discriminated unions

**Styling:**
- [ ] Styling patterns follow project conventions (see `styling-patterns.md` for CSS-in-JS)
- [ ] Consistent spacing approach (prefer layout primitives like gap)
- [ ] Responsive design implemented correctly

**Performance & Quality:**
- [ ] Inline actions (no useCallback unless repeated 2+ times)
- [ ] Immutable state updates
- [ ] Unique, stable keys
- [ ] Unit tests for stores and machines
- [ ] E2E tests for critical flows (Playwright)
