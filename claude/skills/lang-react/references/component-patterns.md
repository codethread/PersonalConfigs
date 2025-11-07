# React Component Organization Patterns

Universal patterns for organizing React components, props, and file structure.

## Component Prop Organization

When components have many props, especially those accepting render props or component props, follow these patterns for maximum readability and type safety.

### Order Props from Shortest to Largest

Place simple value props first, complex props (callbacks, render functions, components) last. This keeps the most important props visible at a glance.

```tsx
// ✅ GOOD: Simple props first, complex props last
<Table
  data={items}
  loading={isLoading}
  sortable
  onSort={handleSort}
  renderRow={(item) => (
    <TableRow key={item.id}>
      <Cell>{item.name}</Cell>
      <Cell>{item.status}</Cell>
    </TableRow>
  )}
  renderEmpty={() => (
    <EmptyState>
      <Body>No items found</Body>
    </EmptyState>
  )}
/>

// ❌ AVOID: Complex props mixed with simple ones
<Table
  renderRow={(item) => (
    <TableRow key={item.id}>
      <Cell>{item.name}</Cell>
    </TableRow>
  )}
  data={items}
  renderEmpty={() => <EmptyState />}
  loading={isLoading}
  onSort={handleSort}
/>
```

### Inline Prop Values for Maximum Type Inference

Prefer inline definitions over extracting to constants. This eliminates manual type annotations and lets TypeScript infer types from the component's prop interface.

```tsx
// ✅ EXCELLENT: Inline with full type inference
<SearchableList
  items={budgets}
  searchKeys={["name", "owner"]}
  renderItem={(budget) => (
    <BudgetCard
      name={budget.name}
      balance={budget.balance}
      onClick={() => handleSelect(budget.id)}
    />
  )}
  renderEmpty={() => (
    <EmptyState message="No budgets found" />
  )}
/>

// ❌ AVOID: Extracted constants require manual typing
const renderItem = (budget: Budget) => (
  <BudgetCard
    name={budget.name}
    balance={budget.balance}
    onClick={() => handleSelect(budget.id)}
  />
)

const renderEmpty: () => JSX.Element = () => (
  <EmptyState message="No budgets found" />
)

<SearchableList
  items={budgets}
  searchKeys={["name", "owner"]}
  renderItem={renderItem}
  renderEmpty={renderEmpty}
/>
```

### Combined Benefits

These patterns work together - inline values naturally create larger props, so ordering simple props first and complex props last improves scanability:

```tsx
// Visual hierarchy from simple to complex
<DataTable
  // Simple values - easy to scan
  columns={3}
  striped
  hoverable

  // Event handlers - medium complexity
  onRowClick={handleRowClick}
  onSort={handleSort}

  // Render props - most complex, at the bottom
  renderHeader={() => (
    <TableHeader>
      <HeaderCell sortable>Name</HeaderCell>
      <HeaderCell sortable>Amount</HeaderCell>
      <HeaderCell>Actions</HeaderCell>
    </TableHeader>
  )}
  renderRow={(item) => (
    <TableRow>
      <Cell>{item.name}</Cell>
      <Cell>
        <Money value={item.amount} currency="USD" />
      </Cell>
      <Cell>
        <Button onClick={() => handleEdit(item.id)}>Edit</Button>
      </Cell>
    </TableRow>
  )}
/>
```

### When to Extract

Only extract when the callback/render function is:
- Reused across multiple component instances
- Complex enough that inlining hurts readability
- Needs memoization with `useCallback` for performance

```tsx
// ✅ GOOD: Extract when reused
const renderBudgetCard = (budget: Budget) => (
  <BudgetCard name={budget.name} balance={budget.balance} />
)

<SearchableList items={activeBudgets} renderItem={renderBudgetCard} />
<FilteredList items={inactiveBudgets} renderItem={renderBudgetCard} />
```

## Component Structure and Colocation

Keep components **colocated and focused** rather than prematurely abstracted.

### Directory Structure

```
pages/
├── BulkUploadBudgets/
│   ├── BulkUploadBudgets.tsx         # Main component
│   ├── components/                    # Subcomponents used by this page
│   │   ├── ValidView.tsx
│   │   ├── ErrorView.tsx
│   │   └── AllocationsTable.tsx
│   ├── hooks/                         # Business logic hooks
│   │   └── useCSVUpload.ts
│   └── utils/                         # Pure utility functions
│       ├── parseCSV.ts
│       └── validateAssignments.ts
```

### Guidelines

- **Accept large, colocated components** when they serve a focused purpose
- **Break into smaller components** only when sub-parts need props and are reused
- **Extract logic to hooks**, not helper components
- **Keep related code together** - don't prematurely split for "clean" structure

```tsx
// ✅ GOOD: Colocated 100-line component serving single purpose
function BulkUploadPage() {
  const {loading, uploadState} = useCSVUpload()
  const {data: wallet} = useWallet()

  const onDrop = useCallback((files: File[]) => {
    // Logic here
  }, [])

  return (
    <Card>
      <Header>
        <Title>Bulk Upload</Title>
        <Description>Upload CSV to assign budgets</Description>
      </Header>

      {match(uploadState)
        .with({_tag: "ready"}, () => (
          <DragAndDrop
            onDrop={onDrop}
            accept=".csv"
            loading={loading}
          />
        ))
        .with({_tag: "valid"}, (state) => (
          <ValidView data={state.data} />
        ))
        .exhaustive()}

      <Footer>
        <Button onClick={handleReset}>Reset</Button>
      </Footer>
    </Card>
  )
}

// ❌ AVOID: Premature abstraction without reuse
function BulkUploadPage() {
  return (
    <PageLayout>
      <PageHeader />
      <PageContent />
      <PageFooter />
    </PageLayout>
  )
}

// Now you have 5 files instead of 1, with no benefit
```

## Separation of Concerns

Components should be **presentational** with logic extracted to hooks and utilities.

### What Belongs in Components

**Simple data transformations for rendering:**

```tsx
// ✅ GOOD: Component receives props and renders
export function ProductList({products}: Props) {
  return (
    <div>
      {products
        .filter(p => p.inStock)
        .map(p => <ProductCard key={p.id} product={p} />)}
    </div>
  )
}
```

**Acceptable in components:**
- ✅ Simple mapping for rendering: `items.map(item => <Card {...item} />)`
- ✅ Basic filtering for visibility: `items.filter(item => item.visible)`
- ✅ Prop destructuring: `const {user, balance} = data`
- ✅ Pattern matching for variants: `match(state).with(...)`
- ✅ Simple string concatenation: `user.firstName + ' ' + user.lastName`

### What Belongs in Hooks/Utilities

**Business logic and complex transformations:**

```tsx
// ✅ GOOD: Hook handles business logic
export function useCSVUpload() {
  const [uploadState, setUploadState] = useState<UploadState>({_tag: "ready"})

  const uploadCSV = useCallback(async (file: File, maxBudget: number) => {
    setUploadState({_tag: "processing"})

    try {
      const parsed = await parseCSV(file)
      const validated = validateAssignments(parsed, maxBudget)

      if (validated.errors.length > 0) {
        setUploadState({_tag: "parseError", errors: validated.errors})
      } else {
        setUploadState({_tag: "valid", data: validated.data})
      }
    } catch (error) {
      setUploadState({_tag: "error", message: error.message})
    }
  }, [])

  return {uploadState, uploadCSV}
}

// Component just consumes the hook
function BulkUpload() {
  const {uploadState, uploadCSV} = useCSVUpload()

  return (
    <Card>
      {match(uploadState)
        .with({_tag: "ready"}, () => <UploadForm onUpload={uploadCSV} />)
        .with({_tag: "valid"}, (s) => <ValidView data={s.data} />)
        .exhaustive()}
    </Card>
  )
}
```

**Extract to hooks/utilities:**
- ❌ Complex calculations and business logic
- ❌ Multiple chained transformations
- ❌ Data normalization and aggregation
- ❌ API calls and data fetching
- ❌ State machines and workflows

## Component Size and Complexity

### When to Keep Components Large

Large components are acceptable when:
- They serve a single, focused purpose
- Logic is extracted to hooks
- Sub-parts don't need independent props
- Splitting would create artificial boundaries

```tsx
// ✅ GOOD: 150-line component with single purpose
function CheckoutForm() {
  // Hooks for data and logic
  const {cart} = useCart()
  const {user} = useAuth()
  const {submitOrder} = useCheckout()

  // Form state
  const [shippingAddress, setShippingAddress] = useState("")
  const [billingAddress, setBillingAddress] = useState("")
  const [paymentMethod, setPaymentMethod] = useState<PaymentMethod>()

  // All related checkout UI in one place
  return (
    <Form onSubmit={submitOrder}>
      <Section>
        <H2>Shipping Address</H2>
        <AddressInput
          value={shippingAddress}
          onChange={setShippingAddress}
        />
      </Section>

      <Section>
        <H2>Billing Address</H2>
        <AddressInput
          value={billingAddress}
          onChange={setBillingAddress}
        />
      </Section>

      <Section>
        <H2>Payment Method</H2>
        <PaymentSelector
          selected={paymentMethod}
          onChange={setPaymentMethod}
        />
      </Section>

      <Section>
        <H2>Order Summary</H2>
        <OrderSummary items={cart.items} total={cart.total} />
      </Section>

      <Button type="submit" primary>
        Complete Purchase
      </Button>
    </Form>
  )
}
```

### When to Split Components

Split when:
- Sub-component is reused in multiple places
- Sub-component needs props from parent
- Sub-component has distinct responsibility
- Splitting improves testability

```tsx
// ✅ GOOD: Split when reused with different props
function OrderSummary({items, total}: Props) {
  return (
    <Card>
      <ItemList items={items} />
      <Divider />
      <TotalRow amount={total} />
    </Card>
  )
}

// Used in multiple contexts
<OrderSummary items={cart.items} total={cart.total} />
<OrderSummary items={wishlist.items} total={wishlist.total} />
```

## Pattern Matching for Variants

Use `ts-pattern` for clean variant rendering instead of nested conditionals:

```tsx
import {match} from "ts-pattern"

// ✅ GOOD: Pattern matching with exhaustiveness checking
{match(uploadState)
  .with({_tag: "ready"}, () => <DragAndDrop onDrop={onDrop} />)
  .with({_tag: "processing"}, () => <Loading />)
  .with({_tag: "parseError"}, (state) => <ErrorView errors={state.errors} />)
  .with({_tag: "valid"}, (state) => <ValidView data={state.data} />)
  .exhaustive()}

// ❌ AVOID: Nested ternaries
{uploadState._tag === "ready" ? (
  <DragAndDrop onDrop={onDrop} />
) : uploadState._tag === "processing" ? (
  <Loading />
) : uploadState._tag === "parseError" ? (
  <ErrorView errors={uploadState.errors} />
) : (
  <ValidView data={uploadState.data} />
)}

// ❌ AVOID: Switch statements in JSX
{(() => {
  switch (uploadState._tag) {
    case "ready":
      return <DragAndDrop onDrop={onDrop} />
    case "processing":
      return <Loading />
    // ...
  }
})()}
```

**Benefits:**
- Type-safe exhaustiveness checking
- TypeScript narrows types in each branch
- Clearer than nested ternaries or switch statements
- Refactoring-friendly (adding new variants is a compile error)

## Summary

**Key Principles:**
1. **Props:** Simple first, complex last; prefer inline for type inference
2. **Structure:** Colocate related code; split only when reused
3. **Separation:** Logic in hooks/utils, components are presentational
4. **Size:** Large components acceptable when focused and logic-free
5. **Variants:** Use pattern matching for discriminated unions
