# React Best Practices for 2025 (SPA-Focused)

Comprehensive guide to modern React development patterns for **client-side Single Page Applications**.

**Last Updated:** 2025-11-04
**Philosophy:** This guide focuses on SPAs with client-side rendering. No Server Components, SSR, or hydration concerns.

**State Management Approach:**
- **TanStack Query** for remote data (or Apollo for GraphQL)
- **Zustand** for application state (testable, isolated from React)
- **XState** for complex state machines (visualizable, testable)
- **useState** only for trivial UI state (rare)

**Component Philosophy:**
- Components are **declarative UI layers**
- Logic lives in **stores and machines**, not components
- External hooks at the top, inline actions in JSX
- No useState/useReducer/useEffect for complex logic

**Testing Strategy:**
- **Primary:** Unit test Zustand stores and XState machines
- **Secondary:** E2E tests with Playwright for critical flows
- **Minimal:** React Testing Library (logic should be in stores)

---

## Table of Contents

1. [Modern React Patterns](#1-modern-react-patterns)
2. [State Management](#2-state-management)
3. [Performance Optimization](#3-performance-optimization)
4. [TypeScript Integration](#4-typescript-integration)
5. [Testing](#5-testing)
6. [Common Anti-Patterns](#6-common-anti-patterns)
7. [Modern Frameworks](#7-modern-frameworks)

---

## 1. Modern React Patterns

### Functional Components and Hooks

**Status:** Functional components are the standard in 2025. Class components are considered legacy.

**Core Principle:** All new React code should use functional components with hooks.

```typescript
// ✅ GOOD: Modern functional component
function UserProfile({ userId }: { userId: string }) {
  const [user, setUser] = useState<User | null>(null);
  const [isLoading, setIsLoading] = useState(true);
  
  useEffect(() => {
    fetchUser(userId).then(setUser).finally(() => setIsLoading(false));
  }, [userId]);
  
  if (isLoading) return <Spinner />;
  return <div>{user?.name}</div>;
}

// ❌ BAD: Class component (legacy pattern)
class UserProfile extends React.Component {
  // Avoid for new code
}
```

### Custom Hooks Best Practices

**When to Create Custom Hooks:**
- Logic appears in 2+ components
- Complex stateful logic needs encapsulation
- Side effects need reusable abstraction

**Naming Convention:** Always prefix with `use` (e.g., `useLocalStorage`, `useWindowSize`)

```typescript
// ✅ GOOD: Reusable custom hook
function useLocalStorage<T>(key: string, initialValue: T) {
  const [storedValue, setStoredValue] = useState<T>(() => {
    try {
      const item = window.localStorage.getItem(key);
      return item ? JSON.parse(item) : initialValue;
    } catch (error) {
      console.error(error);
      return initialValue;
    }
  });

  const setValue = (value: T | ((val: T) => T)) => {
    try {
      const valueToStore = value instanceof Function ? value(storedValue) : value;
      setStoredValue(valueToStore);
      window.localStorage.setItem(key, JSON.stringify(valueToStore));
    } catch (error) {
      console.error(error);
    }
  };

  return [storedValue, setValue] as const;
}

// Usage
function Settings() {
  const [theme, setTheme] = useLocalStorage('theme', 'dark');
  return <button onClick={() => setTheme('light')}>Switch Theme</button>;
}
```

### Component Composition Patterns

**Principle:** Prefer composition over prop drilling or complex conditionals.

```typescript
// ✅ GOOD: Composition with children
function Card({ children }: { children: React.ReactNode }) {
  return <div className="card">{children}</div>;
}

function CardHeader({ children }: { children: React.ReactNode }) {
  return <div className="card-header">{children}</div>;
}

function CardBody({ children }: { children: React.ReactNode }) {
  return <div className="card-body">{children}</div>;
}

// Usage - composable and flexible
<Card>
  <CardHeader>Title</CardHeader>
  <CardBody>Content</CardBody>
</Card>

// ❌ BAD: Monolithic component with many props
function Card({ title, body, footer, hasHeader, hasBorder, ... }) {
  // Too many conditional branches
}
```

### Server Components vs Client Components (RSC)

**Critical Decision Point:** With Next.js 15 and React 19, understanding RSC is essential.

**Server Components (Default in Next.js App Router):**
- Run only on server
- Zero JavaScript sent to client
- Can directly access databases, filesystems, APIs
- Cannot use hooks, event handlers, or browser APIs

**Client Components (Opt-in with 'use client'):**
- Run on both server (initial render) and client (hydration)
- Required for interactivity
- Can use all React hooks and browser APIs

```typescript
// ✅ GOOD: Server Component (no directive needed)
// app/posts/page.tsx
async function PostsPage() {
  // Direct database access - no API layer needed
  const posts = await db.posts.findMany();
  
  return (
    <div>
      {posts.map(post => (
        <PostCard key={post.id} post={post} />
      ))}
    </div>
  );
}

// ✅ GOOD: Client Component (interactive)
// components/like-button.tsx
'use client'

function LikeButton({ postId }: { postId: string }) {
  const [likes, setLikes] = useState(0);
  
  return (
    <button onClick={() => setLikes(prev => prev + 1)}>
      Likes: {likes}
    </button>
  );
}
```

**Composition Pattern - Server + Client:**

```typescript
// ✅ GOOD: Server Component wraps Client Component
// app/post/[id]/page.tsx
async function PostPage({ params }: { params: { id: string } }) {
  // Fetch on server
  const post = await db.posts.findUnique({ where: { id: params.id } });
  
  return (
    <article>
      <h1>{post.title}</h1>
      <p>{post.content}</p>
      {/* Client component for interactivity */}
      <LikeButton postId={post.id} initialLikes={post.likes} />
      <CommentSection postId={post.id} />
    </article>
  );
}
```

**Key Rules:**
1. Keep Client Components as leaf nodes when possible
2. Pass Server Component output as children/props to Client Components
3. Never import Server Components into Client Components
4. Minimize 'use client' boundaries to reduce bundle size

---

## 2. State Management

### The Modern State Management Philosophy (2025)

**Core Principle:** You likely don't need a traditional state management library. Break state into categories and use specialized solutions.

### State Categories and Solutions

#### 1. Remote State (API/Database Data)

**Solution:** TanStack Query (formerly React Query) or SWR

**Why Not Redux/Zustand?** These solve caching, deduplication, invalidation, retries, and optimistic updates automatically.

```typescript
// ✅ GOOD: TanStack Query for server state
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';

function UserProfile({ userId }: { userId: string }) {
  const queryClient = useQueryClient();
  
  const { data: user, isLoading, error } = useQuery({
    queryKey: ['user', userId],
    queryFn: () => fetchUser(userId),
    staleTime: 5 * 60 * 1000, // 5 minutes
  });
  
  const updateMutation = useMutation({
    mutationFn: updateUser,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['user', userId] });
    },
  });
  
  if (isLoading) return <Spinner />;
  if (error) return <Error error={error} />;
  
  return <div>{user.name}</div>;
}

// ❌ BAD: Manual state management for API data
function UserProfile({ userId }: { userId: string }) {
  const [user, setUser] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  
  useEffect(() => {
    // Manual fetching, no caching, no deduplication
    fetchUser(userId)
      .then(setUser)
      .catch(setError)
      .finally(() => setLoading(false));
  }, [userId]);
  // ... Missing retry logic, refetch on focus, etc.
}
```

#### 2. URL State (Query Parameters)

**Solution:** nuqs library or custom hooks

```typescript
// ✅ GOOD: nuqs for URL state
import { useQueryState } from 'nuqs';

function SearchPage() {
  const [search, setSearch] = useQueryState('q', { defaultValue: '' });
  const [page, setPage] = useQueryState('page', { defaultValue: 1, parse: parseInt });
  
  return (
    <div>
      <input value={search} onChange={(e) => setSearch(e.target.value)} />
      <Results search={search} page={page} />
    </div>
  );
}
```

#### 3. Local State (Component-Level)

**Solution:** useState or useReducer

**Rule:** If only one component needs it, keep it local.

```typescript
// ✅ GOOD: Local state for UI-only concerns
function Dropdown() {
  const [isOpen, setIsOpen] = useState(false);
  
  return (
    <div>
      <button onClick={() => setIsOpen(!isOpen)}>Toggle</button>
      {isOpen && <Menu />}
    </div>
  );
}

// ✅ GOOD: useReducer for complex state transitions
type State = { count: number; history: number[] };
type Action = { type: 'increment' } | { type: 'decrement' } | { type: 'reset' };

function reducer(state: State, action: Action): State {
  switch (action.type) {
    case 'increment':
      return { count: state.count + 1, history: [...state.history, state.count + 1] };
    case 'decrement':
      return { count: state.count - 1, history: [...state.history, state.count - 1] };
    case 'reset':
      return { count: 0, history: [0] };
    default:
      return state;
  }
}

function Counter() {
  const [state, dispatch] = useReducer(reducer, { count: 0, history: [0] });
  
  return (
    <div>
      <p>Count: {state.count}</p>
      <button onClick={() => dispatch({ type: 'increment' })}>+</button>
      <button onClick={() => dispatch({ type: 'decrement' })}>-</button>
      <button onClick={() => dispatch({ type: 'reset' })}>Reset</button>
    </div>
  );
}
```

**When to use useState vs useReducer:**

| Use useState | Use useReducer |
|--------------|----------------|
| Single value | Multiple related values |
| Simple updates | Complex state transitions |
| No interdependencies | State with history/undo |
| Independent state | Predictable state machines |

#### 4. Shared State (Cross-Component)

**Decision Tree:**

1. **2-3 levels deep?** → Use props (prop drilling is fine at small scale)
2. **1-2 global concerns (theme, auth)?** → Use React Context
3. **3+ global concerns or performance issues?** → Use Zustand/Jotai/Redux

**Context API Pattern:**

```typescript
// ✅ GOOD: Context for stable, infrequent updates
import { createContext, useContext, useState, type ReactNode } from 'react';

type Theme = 'light' | 'dark';
type ThemeContextType = { theme: Theme; toggleTheme: () => void };

const ThemeContext = createContext<ThemeContextType | undefined>(undefined);

export function ThemeProvider({ children }: { children: ReactNode }) {
  const [theme, setTheme] = useState<Theme>('light');
  
  const toggleTheme = () => {
    setTheme(prev => prev === 'light' ? 'dark' : 'light');
  };
  
  return (
    <ThemeContext.Provider value={{ theme, toggleTheme }}>
      {children}
    </ThemeContext.Provider>
  );
}

export function useTheme() {
  const context = useContext(ThemeContext);
  if (!context) throw new Error('useTheme must be used within ThemeProvider');
  return context;
}

// ⚠️ WARNING: Context causes ALL consumers to re-render
// Use only for infrequently changing values
```

**Zustand Pattern (Recommended for 2025):**

```typescript
// ✅ GOOD: Zustand for global client state
import { create } from 'zustand';

interface CartStore {
  items: CartItem[];
  addItem: (item: CartItem) => void;
  removeItem: (id: string) => void;
  clearCart: () => void;
}

const useCartStore = create<CartStore>((set) => ({
  items: [],
  addItem: (item) => set((state) => ({ items: [...state.items, item] })),
  removeItem: (id) => set((state) => ({ items: state.items.filter(i => i.id !== id) })),
  clearCart: () => set({ items: [] }),
}));

// Usage - only re-renders when items change
function CartBadge() {
  const items = useCartStore((state) => state.items);
  return <span>{items.length}</span>;
}

function AddToCartButton({ product }: { product: Product }) {
  const addItem = useCartStore((state) => state.addItem);
  return <button onClick={() => addItem(product)}>Add to Cart</button>;
}
```

**Jotai Pattern (Fine-Grained Updates):**

```typescript
// ✅ GOOD: Jotai for atomic state
import { atom, useAtom, useAtomValue, useSetAtom } from 'jotai';

const countAtom = atom(0);
const doubleCountAtom = atom((get) => get(countAtom) * 2);

function Counter() {
  const [count, setCount] = useAtom(countAtom);
  const doubleCount = useAtomValue(doubleCountAtom);
  
  return (
    <div>
      <p>Count: {count}</p>
      <p>Double: {doubleCount}</p>
      <button onClick={() => setCount(c => c + 1)}>Increment</button>
    </div>
  );
}
```

### State Management Decision Matrix

| Library | Size | Best For | Performance |
|---------|------|----------|-------------|
| **TanStack Query** | Medium | Server state, API data | Excellent (automatic caching) |
| **Zustand** | Small | Simple global state | Excellent (selective subscriptions) |
| **Jotai** | Small | Fine-grained updates, derived state | Excellent (atomic updates) |
| **Redux Toolkit** | Large | Complex apps, established codebases | Good (with proper selectors) |
| **Context API** | Built-in | 1-2 stable global values | Poor (all consumers re-render) |
| **XState** | Large | State machines, complex flows | Good (predictable transitions) |

**Recommended Stack for 2025:**

```
TanStack Query (server state) + Zustand (client state) + nuqs (URL state)
```

---

## 3. Performance Optimization

### The Golden Rule of Performance

**DO NOT optimize prematurely.** Always profile first using React DevTools Profiler.

### useMemo and useCallback

**When to Use:**
- ✅ Expensive computations (e.g., filtering/sorting large lists)
- ✅ Preventing child re-renders (when child uses React.memo)
- ✅ Dependency in useEffect that shouldn't change
- ❌ Simple calculations or primitive values
- ❌ Every function/object "just in case"

```typescript
// ✅ GOOD: useMemo for expensive computation
function DataTable({ data }: { data: Item[] }) {
  const sortedAndFiltered = useMemo(() => {
    return data
      .filter(item => item.active)
      .sort((a, b) => a.name.localeCompare(b.name));
  }, [data]);
  
  return <Table rows={sortedAndFiltered} />;
}

// ✅ GOOD: useCallback to prevent child re-render
const MemoizedChild = React.memo(Child);

function Parent() {
  const [count, setCount] = useState(0);
  const [unrelated, setUnrelated] = useState(0);
  
  const handleClick = useCallback(() => {
    setCount(c => c + 1);
  }, []); // Empty deps - function never changes
  
  return (
    <div>
      <button onClick={() => setUnrelated(u => u + 1)}>Unrelated</button>
      <MemoizedChild onClick={handleClick} />
    </div>
  );
}

// ❌ BAD: Unnecessary memoization
function Component() {
  const value = useMemo(() => 2 + 2, []); // Pointless - simple calculation
  const onClick = useCallback(() => console.log('hi'), []); // Pointless if child isn't memoized
  return <div>{value}</div>;
}
```

**React Forget (Experimental):** React's auto-memoizing compiler will eliminate manual useMemo/useCallback in the future. Expected to stabilize in 2026.

### Concurrent Features (React 18+)

**useTransition** - Mark non-urgent updates as transitions

```typescript
// ✅ GOOD: useTransition for expensive updates
function SearchResults() {
  const [query, setQuery] = useState('');
  const [results, setResults] = useState([]);
  const [isPending, startTransition] = useTransition();
  
  const handleSearch = (value: string) => {
    setQuery(value); // Urgent - update input immediately
    
    startTransition(() => {
      // Non-urgent - can be interrupted
      const filtered = expensiveSearch(value);
      setResults(filtered);
    });
  };
  
  return (
    <div>
      <input value={query} onChange={(e) => handleSearch(e.target.value)} />
      {isPending && <Spinner />}
      <Results data={results} />
    </div>
  );
}
```

**useDeferredValue** - Defer updates to a value

```typescript
// ✅ GOOD: useDeferredValue for responsive UI
function SearchPage({ query }: { query: string }) {
  const deferredQuery = useDeferredValue(query);
  const results = useMemo(() => expensiveSearch(deferredQuery), [deferredQuery]);
  
  return (
    <div>
      {/* Input stays responsive */}
      <SearchInput value={query} />
      {/* Results update after input settles */}
      <Results data={results} />
    </div>
  );
}
```

### Code Splitting and Lazy Loading

```typescript
// ✅ GOOD: Route-based code splitting
import { lazy, Suspense } from 'react';

const AdminPanel = lazy(() => import('./AdminPanel'));
const Dashboard = lazy(() => import('./Dashboard'));

function App() {
  return (
    <Suspense fallback={<PageSpinner />}>
      <Routes>
        <Route path="/" element={<Dashboard />} />
        <Route path="/admin" element={<AdminPanel />} />
      </Routes>
    </Suspense>
  );
}

// ✅ GOOD: Component-based lazy loading
const HeavyChart = lazy(() => import('./HeavyChart'));

function Analytics() {
  const [showChart, setShowChart] = useState(false);
  
  return (
    <div>
      <button onClick={() => setShowChart(true)}>Show Chart</button>
      {showChart && (
        <Suspense fallback={<ChartSkeleton />}>
          <HeavyChart />
        </Suspense>
      )}
    </div>
  );
}
```

### Virtual Scrolling for Large Lists

**Solution:** react-window or @tanstack/react-virtual

```typescript
// ✅ GOOD: Virtual scrolling for 10,000+ items
import { useVirtualizer } from '@tanstack/react-virtual';
import { useRef } from 'react';

function VirtualList({ items }: { items: Item[] }) {
  const parentRef = useRef<HTMLDivElement>(null);
  
  const virtualizer = useVirtualizer({
    count: items.length,
    getScrollElement: () => parentRef.current,
    estimateSize: () => 50, // Estimated height of each item
  });
  
  return (
    <div ref={parentRef} style={{ height: '500px', overflow: 'auto' }}>
      <div style={{ height: virtualizer.getTotalSize() }}>
        {virtualizer.getVirtualItems().map(virtualRow => (
          <div
            key={virtualRow.index}
            style={{
              position: 'absolute',
              top: 0,
              left: 0,
              width: '100%',
              transform: `translateY(${virtualRow.start}px)`,
            }}
          >
            {items[virtualRow.index].name}
          </div>
        ))}
      </div>
    </div>
  );
}

// ❌ BAD: Rendering 10,000 DOM nodes
function List({ items }: { items: Item[] }) {
  return (
    <div>
      {items.map(item => <div key={item.id}>{item.name}</div>)}
    </div>
  );
}
```

### Performance Checklist

- [ ] Profile with React DevTools before optimizing
- [ ] Use React.memo for expensive components that receive same props
- [ ] Apply useMemo/useCallback strategically, not universally
- [ ] Implement code splitting at route level
- [ ] Use virtual scrolling for lists with 100+ items
- [ ] Leverage useTransition/useDeferredValue for expensive updates
- [ ] Keep Client Components minimal in RSC architecture

---

## 4. TypeScript Integration

### Component Props Typing

```typescript
// ✅ GOOD: Explicit prop types
interface ButtonProps {
  children: React.ReactNode;
  variant?: 'primary' | 'secondary' | 'danger';
  size?: 'small' | 'medium' | 'large';
  onClick?: () => void;
  disabled?: boolean;
}

function Button({ 
  children, 
  variant = 'primary', 
  size = 'medium', 
  onClick,
  disabled = false 
}: ButtonProps) {
  return (
    <button 
      className={`btn-${variant} btn-${size}`}
      onClick={onClick}
      disabled={disabled}
    >
      {children}
    </button>
  );
}

// ✅ GOOD: Extending HTML element props
interface InputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  label: string;
  error?: string;
}

function Input({ label, error, ...props }: InputProps) {
  return (
    <div>
      <label>{label}</label>
      <input {...props} />
      {error && <span className="error">{error}</span>}
    </div>
  );
}
```

### Generic Components

**Critical Pattern:** Use function declaration syntax (not arrow functions) for generic components in TSX.

```typescript
// ✅ GOOD: Generic component with function syntax
interface TableProps<TItem> {
  items: TItem[];
  renderRow: (item: TItem) => React.ReactNode;
  keyExtractor: (item: TItem) => string | number;
}

function Table<TItem>(props: TableProps<TItem>) {
  return (
    <table>
      <tbody>
        {props.items.map(item => (
          <tr key={props.keyExtractor(item)}>
            {props.renderRow(item)}
          </tr>
        ))}
      </tbody>
    </table>
  );
}

// Usage - TypeScript infers TItem from items array
function UserTable() {
  const users: User[] = [{ id: 1, name: 'Alice' }];
  
  return (
    <Table
      items={users}
      keyExtractor={(user) => user.id}
      renderRow={(user) => <td>{user.name}</td>}
    />
  );
}

// ❌ BAD: Arrow function syntax breaks in TSX
const Table = <TItem,>(props: TableProps<TItem>) => {
  // This syntax is ambiguous with JSX in .tsx files
};
```

**Advanced: Generic Form Component**

```typescript
interface FormProps<TValues> {
  initialValues: TValues;
  onSubmit: (values: TValues) => void | Promise<void>;
  children: (props: {
    values: TValues;
    handleChange: (field: keyof TValues, value: any) => void;
    handleSubmit: (e: React.FormEvent) => void;
  }) => React.ReactNode;
}

function Form<TValues extends Record<string, any>>(props: FormProps<TValues>) {
  const [values, setValues] = useState(props.initialValues);
  
  const handleChange = (field: keyof TValues, value: any) => {
    setValues(prev => ({ ...prev, [field]: value }));
  };
  
  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    await props.onSubmit(values);
  };
  
  return (
    <form onSubmit={handleSubmit}>
      {props.children({ values, handleChange, handleSubmit })}
    </form>
  );
}

// Usage - fully type-safe
interface LoginFormValues {
  email: string;
  password: string;
}

function LoginForm() {
  return (
    <Form<LoginFormValues>
      initialValues={{ email: '', password: '' }}
      onSubmit={async (values) => {
        // values is typed as LoginFormValues
        await login(values.email, values.password);
      }}
    >
      {({ values, handleChange }) => (
        <>
          <input
            value={values.email}
            onChange={(e) => handleChange('email', e.target.value)}
          />
          <input
            type="password"
            value={values.password}
            onChange={(e) => handleChange('password', e.target.value)}
          />
          <button type="submit">Login</button>
        </>
      )}
    </Form>
  );
}
```

### Event Handler Typing

```typescript
// ✅ GOOD: Specific event types
function Input() {
  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    console.log(e.target.value);
  };
  
  const handleClick = (e: React.MouseEvent<HTMLButtonElement>) => {
    e.preventDefault();
    console.log('clicked');
  };
  
  const handleSubmit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    // Form submission logic
  };
  
  return (
    <form onSubmit={handleSubmit}>
      <input onChange={handleChange} />
      <button onClick={handleClick}>Submit</button>
    </form>
  );
}

// ❌ BAD: Generic event type
const handleChange = (e: React.SyntheticEvent) => {
  // Loses access to e.target.value
};
```

### Ref Typing

```typescript
// ✅ GOOD: useRef with proper typing
function VideoPlayer() {
  const videoRef = useRef<HTMLVideoElement>(null);
  
  const play = () => {
    videoRef.current?.play();
  };
  
  return (
    <div>
      <video ref={videoRef} />
      <button onClick={play}>Play</button>
    </div>
  );
}

// ✅ GOOD: forwardRef with TypeScript
interface InputProps {
  label: string;
}

const Input = forwardRef<HTMLInputElement, InputProps>(
  ({ label }, ref) => {
    return (
      <div>
        <label>{label}</label>
        <input ref={ref} />
      </div>
    );
  }
);

// Usage
function Parent() {
  const inputRef = useRef<HTMLInputElement>(null);
  
  return <Input ref={inputRef} label="Name" />;
}
```

### TypeScript Best Practices Summary

| Pattern | Approach | Why |
|---------|----------|-----|
| Component props | Explicit interface | Clarity and documentation |
| Generic components | Function declaration syntax | TSX compatibility |
| Event handlers | Specific event types | Type safety on event properties |
| Refs | `useRef<ElementType>(null)` | Null-safe element access |
| Children | `React.ReactNode` | Most flexible type |
| Extending HTML props | `extends React.HTMLAttributes<T>` | Inherit all standard props |

---

## 5. Testing

### Modern Testing Strategy (2025 - SPA Focused)

**Core Philosophy:** Test stores and machines with unit tests, verify user flows with E2E tests.

**Testing Hierarchy:**

| Priority | What to Test | Tool | Why |
|----------|--------------|------|-----|
| **1. Stores (Zustand)** | Application state logic | Vitest (unit) | Fast, isolated, no React |
| **2. Machines (XState)** | Complex state transitions | Vitest (unit) | Deterministic, visualizable |
| **3. Query Hooks** | TanStack Query/Apollo | Vitest + MSW | Mock API responses |
| **4. Critical Flows** | User journeys | Playwright (E2E) | Real browser, real interactions |
| **5. Components** | Rarely needed | React Testing Library | Logic should be in stores |

**Why this approach:**
- **Stores are testable** - No React lifecycle, no mocking complexity
- **Machines are deterministic** - State transitions are pure logic
- **E2E catches integration issues** - Real user interactions
- **Components are thin** - Mostly declarative UI, minimal logic

---

### 1. Testing Zustand Stores (Primary)

**Setup:**

```typescript
// vitest.config.ts
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'jsdom', // For localStorage access
  },
});
```

**Example: Auth Store**

```typescript
// stores/authStore.ts
import { create } from 'zustand';

interface AuthStore {
  user: User | null;
  token: string | null;
  isAuthenticated: boolean;
  login: (credentials: Credentials) => Promise<void>;
  logout: () => void;
  updateProfile: (updates: Partial<User>) => void;
}

export const useAuthStore = create<AuthStore>((set, get) => ({
  user: null,
  token: null,
  isAuthenticated: false,

  login: async (credentials) => {
    const { user, token } = await api.login(credentials);
    set({ user, token, isAuthenticated: true });
    localStorage.setItem('token', token);
  },

  logout: () => {
    set({ user: null, token: null, isAuthenticated: false });
    localStorage.removeItem('token');
  },

  updateProfile: (updates) => {
    const { user } = get();
    if (user) {
      set({ user: { ...user, ...updates } });
    }
  },
}));
```

**Test Suite:**

```typescript
// stores/authStore.test.ts
import { describe, it, expect, beforeEach, vi } from 'vitest';
import { useAuthStore } from './authStore';
import * as api from '../api';

// Mock API
vi.mock('../api', () => ({
  api: {
    login: vi.fn(),
  },
}));

describe('authStore', () => {
  beforeEach(() => {
    // Reset store before each test
    useAuthStore.setState({ user: null, token: null, isAuthenticated: false });
    localStorage.clear();
    vi.clearAllMocks();
  });

  describe('login', () => {
    it('authenticates user and stores token', async () => {
      const mockUser = { id: 1, name: 'Alice', email: 'alice@example.com' };
      const mockToken = 'abc123';

      vi.mocked(api.api.login).mockResolvedValue({
        user: mockUser,
        token: mockToken,
      });

      const { login } = useAuthStore.getState();
      await login({ email: 'alice@example.com', password: 'pass123' });

      // Assert store state
      const state = useAuthStore.getState();
      expect(state.user).toEqual(mockUser);
      expect(state.token).toBe(mockToken);
      expect(state.isAuthenticated).toBe(true);

      // Assert localStorage
      expect(localStorage.getItem('token')).toBe(mockToken);
    });

    it('handles login failure', async () => {
      vi.mocked(api.api.login).mockRejectedValue(new Error('Invalid credentials'));

      const { login } = useAuthStore.getState();

      await expect(login({ email: 'bad@example.com', password: 'wrong' }))
        .rejects
        .toThrow('Invalid credentials');

      // State should remain unchanged
      const state = useAuthStore.getState();
      expect(state.user).toBeNull();
      expect(state.isAuthenticated).toBe(false);
    });
  });

  describe('logout', () => {
    it('clears user and token', () => {
      // Setup authenticated state
      useAuthStore.setState({
        user: { id: 1, name: 'Alice', email: 'alice@example.com' },
        token: 'abc123',
        isAuthenticated: true,
      });
      localStorage.setItem('token', 'abc123');

      const { logout } = useAuthStore.getState();
      logout();

      const state = useAuthStore.getState();
      expect(state.user).toBeNull();
      expect(state.token).toBeNull();
      expect(state.isAuthenticated).toBe(false);
      expect(localStorage.getItem('token')).toBeNull();
    });
  });

  describe('updateProfile', () => {
    it('updates user profile', () => {
      const initialUser = { id: 1, name: 'Alice', email: 'alice@example.com' };
      useAuthStore.setState({ user: initialUser, isAuthenticated: true });

      const { updateProfile } = useAuthStore.getState();
      updateProfile({ name: 'Alice Smith' });

      const state = useAuthStore.getState();
      expect(state.user?.name).toBe('Alice Smith');
      expect(state.user?.email).toBe('alice@example.com'); // Unchanged
    });

    it('does nothing when user is null', () => {
      useAuthStore.setState({ user: null, isAuthenticated: false });

      const { updateProfile } = useAuthStore.getState();
      updateProfile({ name: 'New Name' });

      const state = useAuthStore.getState();
      expect(state.user).toBeNull();
    });
  });
});
```

**Benefits:**
- ✅ No React rendering required
- ✅ Fast execution (<10ms per test)
- ✅ Easy to mock external dependencies
- ✅ Fully isolated from component lifecycle
- ✅ Direct state access via `getState()` and `setState()`

---

### 2. Testing XState Machines (Primary)

**Example: Checkout Machine**

```typescript
// machines/checkoutMachine.ts
import { createMachine, assign } from 'xstate';

interface CheckoutContext {
  cart: CartItem[];
  shipping: ShippingInfo | null;
  payment: PaymentInfo | null;
  error: string | null;
}

type CheckoutEvent =
  | { type: 'PROCEED' }
  | { type: 'BACK' }
  | { type: 'SET_SHIPPING'; data: ShippingInfo }
  | { type: 'SUBMIT_PAYMENT'; data: PaymentInfo }
  | { type: 'PAYMENT_SUCCESS' }
  | { type: 'PAYMENT_ERROR'; error: string };

export const checkoutMachine = createMachine({
  id: 'checkout',
  initial: 'cart',
  context: {
    cart: [],
    shipping: null,
    payment: null,
    error: null,
  },
  states: {
    cart: {
      on: {
        PROCEED: {
          target: 'shipping',
          guard: ({ context }) => context.cart.length > 0,
        },
      },
    },
    shipping: {
      on: {
        SET_SHIPPING: {
          target: 'payment',
          actions: assign({
            shipping: ({ event }) => event.data,
          }),
        },
        BACK: 'cart',
      },
    },
    payment: {
      on: {
        SUBMIT_PAYMENT: 'processing',
        BACK: 'shipping',
      },
    },
    processing: {
      on: {
        PAYMENT_SUCCESS: 'complete',
        PAYMENT_ERROR: {
          target: 'payment',
          actions: assign({
            error: ({ event }) => event.error,
          }),
        },
      },
    },
    complete: {
      type: 'final',
    },
  },
});
```

**Test Suite:**

```typescript
// machines/checkoutMachine.test.ts
import { describe, it, expect } from 'vitest';
import { createActor } from 'xstate';
import { checkoutMachine } from './checkoutMachine';

describe('checkoutMachine', () => {
  it('starts in cart state', () => {
    const actor = createActor(checkoutMachine);
    actor.start();

    expect(actor.getSnapshot().value).toBe('cart');
  });

  it('transitions from cart to shipping when cart is not empty', () => {
    const actor = createActor(checkoutMachine, {
      input: { cart: [{ id: 1, name: 'Product', price: 10 }] },
    });
    actor.start();

    actor.send({ type: 'PROCEED' });

    expect(actor.getSnapshot().value).toBe('shipping');
  });

  it('does not transition from cart when cart is empty', () => {
    const actor = createActor(checkoutMachine, {
      input: { cart: [] },
    });
    actor.start();

    actor.send({ type: 'PROCEED' });

    expect(actor.getSnapshot().value).toBe('cart'); // No transition
  });

  it('stores shipping info and transitions to payment', () => {
    const actor = createActor(checkoutMachine, {
      input: { cart: [{ id: 1, name: 'Product', price: 10 }] },
    });
    actor.start();

    actor.send({ type: 'PROCEED' });
    actor.send({
      type: 'SET_SHIPPING',
      data: { address: '123 Main St', city: 'Boston' },
    });

    expect(actor.getSnapshot().value).toBe('payment');
    expect(actor.getSnapshot().context.shipping).toEqual({
      address: '123 Main St',
      city: 'Boston',
    });
  });

  it('allows navigation back from shipping to cart', () => {
    const actor = createActor(checkoutMachine, {
      input: { cart: [{ id: 1, name: 'Product', price: 10 }] },
    });
    actor.start();

    actor.send({ type: 'PROCEED' });
    actor.send({ type: 'BACK' });

    expect(actor.getSnapshot().value).toBe('cart');
  });

  it('transitions to processing then complete on successful payment', () => {
    const actor = createActor(checkoutMachine, {
      input: { cart: [{ id: 1, name: 'Product', price: 10 }] },
    });
    actor.start();

    // Navigate to payment
    actor.send({ type: 'PROCEED' });
    actor.send({
      type: 'SET_SHIPPING',
      data: { address: '123 Main St', city: 'Boston' },
    });

    // Submit payment
    actor.send({
      type: 'SUBMIT_PAYMENT',
      data: { cardNumber: '4242', cvv: '123' },
    });
    expect(actor.getSnapshot().value).toBe('processing');

    // Payment succeeds
    actor.send({ type: 'PAYMENT_SUCCESS' });
    expect(actor.getSnapshot().value).toBe('complete');
  });

  it('returns to payment on payment error with error message', () => {
    const actor = createActor(checkoutMachine, {
      input: { cart: [{ id: 1, name: 'Product', price: 10 }] },
    });
    actor.start();

    // Navigate to processing
    actor.send({ type: 'PROCEED' });
    actor.send({
      type: 'SET_SHIPPING',
      data: { address: '123 Main St', city: 'Boston' },
    });
    actor.send({
      type: 'SUBMIT_PAYMENT',
      data: { cardNumber: '4242', cvv: '123' },
    });

    // Payment fails
    actor.send({ type: 'PAYMENT_ERROR', error: 'Card declined' });

    expect(actor.getSnapshot().value).toBe('payment');
    expect(actor.getSnapshot().context.error).toBe('Card declined');
  });
});
```

**Benefits:**
- ✅ Deterministic state transitions
- ✅ Visualizable with XState Inspector
- ✅ Guards and actions are testable
- ✅ No React coupling
- ✅ Explicit exhaustive testing of all paths

---

### 3. E2E Testing with Playwright (Secondary)

**When to use E2E:** Critical user flows that span multiple components/pages.

**Setup:**

```typescript
// playwright.config.ts
import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  testDir: './e2e',
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: process.env.CI ? 1 : undefined,
  reporter: 'html',
  use: {
    baseURL: 'http://localhost:5173',
    trace: 'on-first-retry',
  },
  projects: [
    { name: 'chromium', use: { ...devices['Desktop Chrome'] } },
  ],
  webServer: {
    command: 'npm run dev',
    url: 'http://localhost:5173',
    reuseExistingServer: !process.env.CI,
  },
});
```

**Example: Complete Checkout Flow**

```typescript
// e2e/checkout.spec.ts
import { test, expect } from '@playwright/test';

test.describe('Checkout Flow', () => {
  test('user can complete full checkout process', async ({ page }) => {
    // Start at homepage
    await page.goto('/');

    // Add items to cart
    await page.getByRole('button', { name: 'Add to Cart' }).first().click();
    await expect(page.getByText('1 item in cart')).toBeVisible();

    // Navigate to cart
    await page.getByRole('link', { name: 'Cart' }).click();
    await expect(page).toHaveURL('/cart');

    // Proceed to checkout
    await page.getByRole('button', { name: 'Checkout' }).click();
    await expect(page).toHaveURL('/checkout');

    // Fill shipping information
    await page.getByLabel('Full Name').fill('Alice Smith');
    await page.getByLabel('Address').fill('123 Main Street');
    await page.getByLabel('City').fill('Boston');
    await page.getByLabel('ZIP Code').fill('02101');
    await page.getByRole('button', { name: 'Continue to Payment' }).click();

    // Verify shipping summary appears
    await expect(page.getByText('Ship to: 123 Main Street')).toBeVisible();

    // Fill payment information
    await page.getByLabel('Card Number').fill('4242424242424242');
    await page.getByLabel('Expiry').fill('12/25');
    await page.getByLabel('CVV').fill('123');
    await page.getByRole('button', { name: 'Submit Payment' }).click();

    // Verify success
    await expect(page.getByRole('heading', { name: 'Order Complete' })).toBeVisible();
    await expect(page.getByText(/Order #\d+/)).toBeVisible();
  });

  test('prevents checkout with empty cart', async ({ page }) => {
    await page.goto('/cart');

    const checkoutButton = page.getByRole('button', { name: 'Checkout' });
    await expect(checkoutButton).toBeDisabled();
  });

  test('handles payment failure gracefully', async ({ page }) => {
    // Setup: Add item and navigate to payment
    await page.goto('/');
    await page.getByRole('button', { name: 'Add to Cart' }).first().click();
    await page.getByRole('link', { name: 'Cart' }).click();
    await page.getByRole('button', { name: 'Checkout' }).click();

    await page.getByLabel('Full Name').fill('Alice Smith');
    await page.getByLabel('Address').fill('123 Main Street');
    await page.getByLabel('City').fill('Boston');
    await page.getByLabel('ZIP Code').fill('02101');
    await page.getByRole('button', { name: 'Continue to Payment' }).click();

    // Use invalid card (triggers failure in test env)
    await page.getByLabel('Card Number').fill('0000000000000000');
    await page.getByLabel('Expiry').fill('12/25');
    await page.getByLabel('CVV').fill('123');
    await page.getByRole('button', { name: 'Submit Payment' }).click();

    // Verify error handling
    await expect(page.getByText(/Payment failed/i)).toBeVisible();
    await expect(page.getByRole('button', { name: 'Try Again' })).toBeVisible();
  });
});
```

**Benefits:**
- ✅ Tests real user interactions in real browser
- ✅ Catches integration issues between components
- ✅ Validates entire user journey
- ✅ No mocking required
- ✅ Visual regression testing possible

---

### 4. React Testing Library (Minimal Use)

**When to use:** Only when logic truly must live in a component (rare).

**Example: Simple UI-only component**

```typescript
// components/Dropdown.test.tsx
import { render, screen } from '@testing-library/react';
import { userEvent } from '@testing-library/user-event';
import { Dropdown } from './Dropdown';

test('toggles dropdown menu on click', async () => {
  const user = userEvent.setup();
  render(<Dropdown items={['Option 1', 'Option 2']} />);

  const button = screen.getByRole('button', { name: /select/i });

  // Closed by default
  expect(screen.queryByRole('menu')).not.toBeInTheDocument();

  // Opens on click
  await user.click(button);
  expect(screen.getByRole('menu')).toBeVisible();

  // Closes on second click
  await user.click(button);
  expect(screen.queryByRole('menu')).not.toBeInTheDocument();
});
```

**Why minimal:**
- If component has logic → Move to Zustand/XState
- If component is declarative → E2E tests cover it
- Only test truly stateless UI components

---

### Testing Strategy Summary

| Test Type | Priority | When | Tool |
|-----------|----------|------|------|
| **Zustand Stores** | 🔥 Primary | Application state, business logic | Vitest |
| **XState Machines** | 🔥 Primary | Complex state transitions | Vitest |
| **E2E Flows** | ⭐ Secondary | Critical user journeys | Playwright |
| **React Components** | ⚪ Rare | UI-only components | React Testing Library |

**Testing Checklist:**

- [ ] All Zustand stores have unit tests
- [ ] All XState machines have transition tests
- [ ] Critical user flows covered by E2E tests
- [ ] Components are declarative (minimal testing needed)
- [ ] TanStack Query hooks mocked with MSW when needed
- [ ] Tests run fast (<1s for unit tests)
- [ ] E2E tests run in CI/CD pipeline

---

## 6. Common Anti-Patterns

### 1. Prop Drilling

**Problem:** Passing props through multiple components that don't use them.

```typescript
// ❌ BAD: Prop drilling
function App() {
  const [user, setUser] = useState(null);
  return <Layout user={user} />;
}

function Layout({ user }) {
  return <Sidebar user={user} />;
}

function Sidebar({ user }) {
  return <Navigation user={user} />;
}

function Navigation({ user }) {
  return <UserMenu user={user} />;
}

function UserMenu({ user }) {
  return <div>{user.name}</div>;
}

// ✅ GOOD: Context or state management
const UserContext = createContext(null);

function App() {
  const [user, setUser] = useState(null);
  return (
    <UserContext.Provider value={user}>
      <Layout />
    </UserContext.Provider>
  );
}

function UserMenu() {
  const user = useContext(UserContext);
  return <div>{user.name}</div>;
}

// ✅ BETTER: Zustand for complex cases
const useUserStore = create((set) => ({
  user: null,
  setUser: (user) => set({ user }),
}));

function UserMenu() {
  const user = useUserStore((state) => state.user);
  return <div>{user.name}</div>;
}
```

### 2. Unnecessary Re-renders

**Problem:** Creating new function/object references on every render.

```typescript
// ❌ BAD: New function reference every render
function Parent() {
  const [count, setCount] = useState(0);
  
  return (
    <div>
      <button onClick={() => setCount(count + 1)}>Increment</button>
      <ExpensiveChild onEvent={() => console.log('event')} />
    </div>
  );
}

// ExpensiveChild re-renders on every Parent render

// ✅ GOOD: Memoize callback + memo child
const MemoizedChild = React.memo(ExpensiveChild);

function Parent() {
  const [count, setCount] = useState(0);
  
  const handleEvent = useCallback(() => {
    console.log('event');
  }, []);
  
  return (
    <div>
      <button onClick={() => setCount(count + 1)}>Increment</button>
      <MemoizedChild onEvent={handleEvent} />
    </div>
  );
}

// ❌ BAD: Inline object props
<Component style={{ margin: 10 }} />
// New object reference every render

// ✅ GOOD: Stable reference
const style = { margin: 10 };
<Component style={style} />

// Or with useMemo if dynamic:
const style = useMemo(() => ({ margin: spacing * 2 }), [spacing]);
<Component style={style} />
```

### 3. useEffect Dependency Issues

**Problem:** Missing dependencies or infinite loops.

```typescript
// ❌ BAD: Missing dependencies
function Component({ userId }) {
  const [user, setUser] = useState(null);
  
  useEffect(() => {
    fetchUser(userId).then(setUser);
  }, []); // Missing userId - won't refetch on change
  
  return <div>{user?.name}</div>;
}

// ❌ BAD: Infinite loop
function Component() {
  const [data, setData] = useState([]);
  
  useEffect(() => {
    fetchData().then(setData);
  }, [data]); // data changes → effect runs → data changes → infinite loop
  
  return <div>{data.length}</div>;
}

// ✅ GOOD: Correct dependencies
function Component({ userId }) {
  const [user, setUser] = useState(null);
  
  useEffect(() => {
    fetchUser(userId).then(setUser);
  }, [userId]); // Refetches when userId changes
  
  return <div>{user?.name}</div>;
}

// ✅ GOOD: Proper cleanup
function Component() {
  useEffect(() => {
    const controller = new AbortController();
    
    fetchData(controller.signal)
      .then(setData)
      .catch(err => {
        if (err.name !== 'AbortError') {
          console.error(err);
        }
      });
    
    return () => controller.abort(); // Cleanup on unmount
  }, []);
}

// ❌ BAD: Async function directly in useEffect
useEffect(async () => {
  const data = await fetchData();
}, []); // Error: useEffect must return cleanup function or nothing

// ✅ GOOD: Async function inside useEffect
useEffect(() => {
  async function loadData() {
    const data = await fetchData();
    setData(data);
  }
  loadData();
}, []);
```

### 4. Direct State Mutation

**Problem:** Mutating state directly instead of creating new references.

```typescript
// ❌ BAD: Direct mutation
function TodoList() {
  const [todos, setTodos] = useState([]);
  
  const addTodo = (text) => {
    todos.push({ id: Date.now(), text }); // Mutates array
    setTodos(todos); // React may not detect change
  };
  
  const toggleTodo = (id) => {
    const todo = todos.find(t => t.id === id);
    todo.completed = !todo.completed; // Mutates object
    setTodos(todos);
  };
  
  return <div>...</div>;
}

// ✅ GOOD: Immutable updates
function TodoList() {
  const [todos, setTodos] = useState([]);
  
  const addTodo = (text) => {
    setTodos([...todos, { id: Date.now(), text, completed: false }]);
  };
  
  const toggleTodo = (id) => {
    setTodos(todos.map(todo =>
      todo.id === id ? { ...todo, completed: !todo.completed } : todo
    ));
  };
  
  return <div>...</div>;
}
```

### 5. Key Prop Mistakes

**Problem:** Using array index as key or non-unique keys.

```typescript
// ❌ BAD: Index as key
{items.map((item, index) => (
  <Item key={index} data={item} />
))}
// Problems: reordering breaks, deletions cause re-renders

// ❌ BAD: Non-unique key
{items.map(item => (
  <Item key={item.type} data={item} />
))}
// Multiple items may have same type

// ✅ GOOD: Unique, stable key
{items.map(item => (
  <Item key={item.id} data={item} />
))}

// ✅ GOOD: Generate stable ID if none exists
import { nanoid } from 'nanoid';

const itemsWithIds = useMemo(
  () => items.map(item => ({ ...item, _id: nanoid() })),
  [items]
);

{itemsWithIds.map(item => (
  <Item key={item._id} data={item} />
))}
```

### 6. Context API Misuse

**Problem:** All consumers re-render when any context value changes.

```typescript
// ❌ BAD: Multiple values in one context
const AppContext = createContext(null);

function AppProvider({ children }) {
  const [user, setUser] = useState(null);
  const [theme, setTheme] = useState('light');
  const [settings, setSettings] = useState({});
  
  return (
    <AppContext.Provider value={{ user, setUser, theme, setTheme, settings, setSettings }}>
      {children}
    </AppContext.Provider>
  );
}

// ThemeToggle re-renders when user changes!

// ✅ GOOD: Separate contexts for independent concerns
const UserContext = createContext(null);
const ThemeContext = createContext(null);
const SettingsContext = createContext(null);

function AppProvider({ children }) {
  const [user, setUser] = useState(null);
  const [theme, setTheme] = useState('light');
  const [settings, setSettings] = useState({});
  
  return (
    <UserContext.Provider value={{ user, setUser }}>
      <ThemeContext.Provider value={{ theme, setTheme }}>
        <SettingsContext.Provider value={{ settings, setSettings }}>
          {children}
        </SettingsContext.Provider>
      </ThemeContext.Provider>
    </UserContext.Provider>
  );
}

// ✅ BETTER: Use Zustand instead for frequently changing state
```

### 7. Overusing useMemo/useCallback

**Problem:** Premature optimization adds complexity without benefit.

```typescript
// ❌ BAD: Unnecessary memoization
function Component({ a, b }) {
  const sum = useMemo(() => a + b, [a, b]); // Simple calculation
  const handleClick = useCallback(() => {
    console.log('clicked');
  }, []); // Child isn't memoized anyway
  
  return <div onClick={handleClick}>{sum}</div>;
}

// ✅ GOOD: Only memoize when beneficial
function Component({ data }) {
  // Memoize expensive computation
  const sortedData = useMemo(() => {
    return data.slice().sort((a, b) => a.value - b.value);
  }, [data]);
  
  // No memoization for simple values
  const total = data.reduce((sum, item) => sum + item.value, 0);
  
  return <div>{total}</div>;
}
```

---

## 7. Modern Frameworks

### Next.js App Router

**Status:** Production-ready as of Next.js 13+, refined in 14-15.

**When to Use Next.js:**
- Content-heavy sites (blogs, marketing, e-commerce)
- SEO-critical applications
- Need hybrid rendering (SSG + SSR + ISR)
- Want integrated backend APIs
- Deploying to Vercel

**Key Features:**
- React Server Components by default
- File-system based routing
- Built-in image optimization
- API routes for backend logic
- Edge runtime support

**App Router Best Practices:**

```typescript
// ✅ GOOD: Server Component fetching data
// app/posts/page.tsx
async function PostsPage() {
  // Fetch directly on server - no API route needed
  const posts = await db.posts.findMany();
  
  return (
    <div>
      <h1>Posts</h1>
      {posts.map(post => (
        <PostCard key={post.id} post={post} />
      ))}
    </div>
  );
}

// ✅ GOOD: Client Component for interactivity
// components/like-button.tsx
'use client'

export function LikeButton({ postId, initialLikes }: Props) {
  const [likes, setLikes] = useState(initialLikes);
  const [optimisticLikes, setOptimisticLikes] = useOptimistic(likes);
  
  const handleLike = async () => {
    setOptimisticLikes(likes + 1);
    await fetch(`/api/posts/${postId}/like`, { method: 'POST' });
    setLikes(likes + 1);
  };
  
  return <button onClick={handleLike}>Likes: {optimisticLikes}</button>;
}

// ✅ GOOD: Streaming with Suspense
// app/dashboard/page.tsx
async function DashboardPage() {
  return (
    <div>
      <Header />
      <Suspense fallback={<AnalyticsSkeleton />}>
        <Analytics />
      </Suspense>
      <Suspense fallback={<ChartSkeleton />}>
        <Charts />
      </Suspense>
    </div>
  );
}

async function Analytics() {
  const data = await fetchAnalytics(); // Slow query
  return <AnalyticsDisplay data={data} />;
}
```

**Caching Strategy:**

Next.js has multiple cache layers. Understand them:

| Cache Type | Location | Duration | Opt-out |
|------------|----------|----------|---------|
| Request Memoization | Server (render) | Request lifecycle | N/A |
| Data Cache | Server (persistent) | Until revalidated | `{ cache: 'no-store' }` |
| Full Route Cache | Server (build/revalidate) | Until revalidated | `dynamic = 'force-dynamic'` |
| Router Cache | Client | Session/time-based | `router.refresh()` |

```typescript
// ✅ GOOD: Control caching per fetch
async function Page() {
  // Cached by default
  const staticData = await fetch('https://api.example.com/static');
  
  // Revalidate every 60 seconds
  const freshData = await fetch('https://api.example.com/data', {
    next: { revalidate: 60 }
  });
  
  // Never cache
  const dynamicData = await fetch('https://api.example.com/realtime', {
    cache: 'no-store'
  });
  
  return <div>...</div>;
}

// ✅ GOOD: Opt out of static rendering
export const dynamic = 'force-dynamic';
export const revalidate = 0;

async function Page() {
  // Always dynamic, never cached
  const data = await fetch('...');
  return <div>...</div>;
}
```

### Remix

**When to Use Remix:**
- Data-intensive applications (dashboards, admin panels)
- Form-heavy applications
- Need progressive enhancement (works without JS)
- Want platform-agnostic deployment
- Prefer nested routing by default

**Key Features:**
- Server-first data loading with loaders
- Built-in form handling with actions
- Nested routes with parallel data loading
- Web standards focused (Request/Response)
- Platform-agnostic (Node, Deno, Cloudflare, etc.)

**Remix Patterns:**

```typescript
// ✅ GOOD: Remix loader for data fetching
// app/routes/posts.$postId.tsx
import { json, type LoaderFunctionArgs } from '@remix-run/node';
import { useLoaderData } from '@remix-run/react';

export async function loader({ params }: LoaderFunctionArgs) {
  const post = await db.posts.findUnique({ where: { id: params.postId } });
  if (!post) throw new Response('Not Found', { status: 404 });
  return json({ post });
}

export default function PostPage() {
  const { post } = useLoaderData<typeof loader>();
  return (
    <article>
      <h1>{post.title}</h1>
      <p>{post.content}</p>
    </article>
  );
}

// ✅ GOOD: Remix action for mutations
import { type ActionFunctionArgs, redirect } from '@remix-run/node';

export async function action({ request }: ActionFunctionArgs) {
  const formData = await request.formData();
  const title = formData.get('title');
  const content = formData.get('content');
  
  const post = await db.posts.create({ data: { title, content } });
  return redirect(`/posts/${post.id}`);
}

export default function NewPostPage() {
  return (
    <Form method="post">
      <input name="title" required />
      <textarea name="content" required />
      <button type="submit">Create Post</button>
    </Form>
  );
}

// ✅ GOOD: Nested routes with shared layouts
// app/routes/dashboard.tsx (layout)
export default function DashboardLayout() {
  return (
    <div>
      <Sidebar />
      <main>
        <Outlet /> {/* Child routes render here */}
      </main>
    </div>
  );
}

// app/routes/dashboard.analytics.tsx (child)
export default function AnalyticsPage() {
  return <div>Analytics</div>;
}
```

### Framework Decision Matrix

| Factor | Next.js | Remix |
|--------|---------|-------|
| **Rendering** | Hybrid (SSG/ISR/SSR) | Primarily SSR |
| **Data Loading** | Server Components + fetch | Loaders |
| **Mutations** | Server Actions | Actions + Forms |
| **Routing** | File-system (flat-first) | Nested by default |
| **Philosophy** | Abstraction-heavy | Web standards-focused |
| **Best For** | Content sites, SEO, static-first | Data apps, forms, dynamic-first |
| **Deployment** | Optimized for Vercel | Platform-agnostic |
| **Progressive Enhancement** | Limited | First-class |
| **Learning Curve** | Steeper (many concepts) | Moderate (web standards) |
| **Community/Ecosystem** | Larger | Growing |

### Quick Decision Guide

**Choose Next.js if:**
- Your site is content-heavy (blog, marketing, docs)
- SEO is critical
- You need static generation (SSG)
- You want an all-in-one framework
- You're deploying to Vercel

**Choose Remix if:**
- Your app is data-intensive (dashboards, admin tools)
- You have many forms and mutations
- You want progressive enhancement
- You need deployment flexibility
- You prefer nested routing

**Both are excellent choices in 2025.** The decision depends more on your use case and team preferences than framework quality.

---

## Summary: React Development in 2025

### The Modern React Stack

```
Framework: Next.js 15 (App Router) or Remix
Language: TypeScript
State: TanStack Query + Zustand + nuqs
Testing: Vitest + React Testing Library
Performance: React 19 concurrent features
```

### Key Principles

1. **Functional components + hooks** are the standard
2. **Server Components** reduce bundle size and improve performance
3. **State management** is categorized: remote (TanStack Query), URL (nuqs), shared (Zustand)
4. **TypeScript** is essential for maintainability
5. **Performance optimization** should be measured, not assumed
6. **Testing** focuses on user behavior, not implementation
7. **Anti-patterns** like prop drilling and unnecessary re-renders have known solutions

### For AI Agents: Key Reminders

- Always use functional components with TypeScript
- Prefer Server Components unless interactivity is needed
- Use TanStack Query for API data, not manual useState + useEffect
- Apply useMemo/useCallback strategically after profiling
- Test with role-based queries (getByRole, getByLabelText)
- Avoid prop drilling with Context or Zustand
- Use generic components with function syntax, not arrow functions
- Remember: React is highly optimized—don't over-optimize prematurely

---

**Document Version:** 1.0  
**Maintained by:** AI Research Agent  
**Next Review:** 2025-Q2 (or when React 19 stable releases)
