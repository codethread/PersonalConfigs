# React Styling Patterns with styled-components

Universal patterns for organizing and writing styled-components in React applications.

## Theme Access Patterns

### Always Access Theme via Props

When using styled-components, **always access the theme through the `theme` prop** in your styled component definitions. Never import or use theme objects directly.

```tsx
import styled from "styled-components"

// ✅ CORRECT: Access theme via props
const Container = styled.div`
  background: ${({theme}) => theme.colors.background};
  border-radius: ${({theme}) => theme.borderRadius.medium};
  gap: ${({theme}) => theme.spacing.medium};

  ${({theme}) => theme.mediaQueries.tablet`
    grid-template-columns: 1fr 2fr;
  `}
`

// ❌ INCORRECT: Never import theme directly
// import {theme} from './theme'
// const Container = styled.div`
//   background: ${theme.colors.background};
// `
```

### Theme in Components (useTheme Hook)

For accessing theme values in component logic (not in styled-components), use the `useTheme` hook if your theme system provides one:

```tsx
import {useTheme} from "styled-components"

function MyComponent() {
  const theme = useTheme()

  return (
    <div style={{color: theme.colors.primary}}>
      <Icon color={theme.colors.accent} />
    </div>
  )
}
```

## Consolidating Styled Components

When building complex components with multiple related parts (tables, cards, lists), **prefer using CSS selectors within a parent styled component** over creating many small styled components.

### Benefits of Consolidation

- Keeps related styles colocated in one place
- Reduces component proliferation
- Makes styling relationships explicit
- Easier to maintain and understand

### Pattern: Consolidated Table Styling

```tsx
// ✅ GOOD: Consolidated table styling with CSS selectors
const StyledTable = styled.table`
  width: 100%;
  border-collapse: collapse;
  background: ${({theme}) => theme.colors.background};

  thead {
    background: ${({theme}) => theme.colors.backgroundAccent};
    border-bottom: 2px solid ${({theme}) => theme.colors.border};

    th {
      padding: ${({theme}) => theme.spacing.medium};
      text-align: left;
      font-weight: 600;
      color: ${({theme}) => theme.colors.textPrimary};

      // First column styling
      &:first-child {
        padding-left: ${({theme}) => theme.spacing.large};
        border-radius: ${({theme}) => theme.borderRadius.small} 0 0 0;
      }

      // Last column styling
      &:last-child {
        padding-right: ${({theme}) => theme.spacing.large};
        border-radius: 0 ${({theme}) => theme.borderRadius.small} 0 0;
        text-align: right;
      }
    }
  }

  tbody {
    tr {
      border-bottom: 1px solid ${({theme}) => theme.colors.border};
      transition: background 0.2s;

      &:hover {
        background: ${({theme}) => theme.colors.backgroundHover};
      }

      &:last-child {
        border-bottom: none;
      }
    }

    td {
      padding: ${({theme}) => theme.spacing.medium};

      &:first-child {
        padding-left: ${({theme}) => theme.spacing.large};
        font-weight: 500;
      }

      &:last-child {
        padding-right: ${({theme}) => theme.spacing.large};
        text-align: right;
        color: ${({theme}) => theme.colors.textSecondary};
      }
    }
  }
`

// Usage - simple and clean
export function BudgetTable({budgets}: Props) {
  return (
    <StyledTable>
      <thead>
        <tr>
          <th>Name</th>
          <th>Owner</th>
          <th>Balance</th>
        </tr>
      </thead>
      <tbody>
        {budgets.map(budget => (
          <tr key={budget.id}>
            <td>{budget.name}</td>
            <td>{budget.owner}</td>
            <td>{budget.balance}</td>
          </tr>
        ))}
      </tbody>
    </StyledTable>
  )
}
```

### Anti-Pattern: Too Many Small Components

```tsx
// ❌ AVOID: Too many small styled components
const StyledTable = styled.table`
  width: 100%;
  border-collapse: collapse;
`

const TableHead = styled.thead`
  background: ${({theme}) => theme.colors.backgroundAccent};
`

const HeaderCell = styled.th`
  padding: ${({theme}) => theme.spacing.medium};
  text-align: left;
`

const FirstHeaderCell = styled(HeaderCell)`
  padding-left: ${({theme}) => theme.spacing.large};
`

const LastHeaderCell = styled(HeaderCell)`
  padding-right: ${({theme}) => theme.spacing.large};
  text-align: right;
`

const TableBody = styled.tbody``

const TableRow = styled.tr`
  border-bottom: 1px solid ${({theme}) => theme.colors.border};
  &:hover {
    background: ${({theme}) => theme.colors.backgroundHover};
  }
`

const Cell = styled.td`
  padding: ${({theme}) => theme.spacing.medium};
`

const FirstCell = styled(Cell)`
  padding-left: ${({theme}) => theme.spacing.large};
`

const LastCell = styled(Cell)`
  padding-right: ${({theme}) => theme.spacing.large};
  text-align: right;
`

// Usage - verbose and harder to maintain
export function BudgetTable({budgets}: Props) {
  return (
    <StyledTable>
      <TableHead>
        <tr>
          <FirstHeaderCell>Name</FirstHeaderCell>
          <HeaderCell>Owner</HeaderCell>
          <LastHeaderCell>Balance</LastHeaderCell>
        </tr>
      </TableHead>
      <TableBody>
        {budgets.map(budget => (
          <TableRow key={budget.id}>
            <FirstCell>{budget.name}</FirstCell>
            <Cell>{budget.owner}</Cell>
            <LastCell>{budget.balance}</LastCell>
          </TableRow>
        ))}
      </TableBody>
    </StyledTable>
  )
}
```

### When to Use CSS Selectors vs Multiple Components

```tsx
// ✅ Use CSS selectors for structural styling
const Card = styled.div`
  border: 1px solid ${({theme}) => theme.colors.border};
  border-radius: ${({theme}) => theme.borderRadius.medium};

  // Header styling
  .card-header {
    padding: ${({theme}) => theme.spacing.large};
    border-bottom: 1px solid ${({theme}) => theme.colors.border};
    background: ${({theme}) => theme.colors.backgroundAccent};
  }

  // Body styling
  .card-body {
    padding: ${({theme}) => theme.spacing.large};
  }

  // Footer styling
  .card-footer {
    padding: ${({theme}) => theme.spacing.medium};
    border-top: 1px solid ${({theme}) => theme.colors.border};
    display: flex;
    justify-content: flex-end;
    gap: ${({theme}) => theme.spacing.medium};
  }
`

// ✅ Create separate components when they have distinct props/behavior
const IconButton = styled.button<{variant?: "primary" | "secondary"}>`
  padding: ${({theme}) => theme.spacing.small};
  border-radius: ${({theme}) => theme.borderRadius.small};
  background: ${({theme, variant}) =>
    variant === "primary" ? theme.colors.primary : "transparent"};
  // ... variant-specific styling
`
```

### Guidelines

- **Use CSS selectors** for styling parent-child relationships within a single component
- **Keep all related styles colocated** in the same file
- **Leverage pseudo-selectors** (`:first-child`, `:last-child`, `:hover`, `:nth-child`) for positional styling
- **Avoid deeply nested overrides** that target components defined in other files
- **Create separate styled components** only when they have distinct props or are reused independently

## Spacing and Layout Best Practices

### Prefer Gap for Spacing

**Always use `gap` for spacing between elements** in Flex/Grid layouts rather than individual margins:

```tsx
// ✅ PREFERRED: Use gap for consistent spacing
const Container = styled.div`
  display: flex;
  flex-direction: column;
  gap: ${({theme}) => theme.spacing.medium};
`

function MyComponent() {
  return (
    <Container>
      <Card>First</Card>
      <Card>Second</Card>
      <Card>Third</Card>
    </Container>
  )
}

// ❌ AVOID: Individual margins
const Container = styled.div`
  display: flex;
  flex-direction: column;
`

const Card = styled.div`
  margin-bottom: ${({theme}) => theme.spacing.medium};

  &:last-child {
    margin-bottom: 0;
  }
`
```

**Benefits of gap:**
- No need for special last-child handling
- Works with flexbox and grid
- Spacing is defined on the container, not individual items
- Easier to maintain and understand

### Responsive Spacing

```tsx
const ResponsiveContainer = styled.div`
  display: grid;
  gap: ${({theme}) => theme.spacing.small};

  ${({theme}) => theme.mediaQueries.tablet`
    gap: ${theme.spacing.medium};
  `}

  ${({theme}) => theme.mediaQueries.desktop`
    gap: ${theme.spacing.large};
  `}
`
```

## Extending UI Library Components

When extending components from a UI library, use styled-components' extension syntax:

```tsx
import {Button} from "@your-ui-lib/Button"
import {Flex} from "@your-ui-lib/Flex"

// ✅ GOOD: Extend existing component
const PrimaryButton = styled(Button)`
  background: ${({theme}) => theme.colors.primary};
  font-weight: 600;

  &:hover {
    background: ${({theme}) => theme.colors.primaryDark};
  }
`

// ✅ GOOD: Extend with custom props
const GridItem = styled(Flex)<{area: string}>`
  flex-direction: column;
  grid-area: ${({area}) => area};
`

// Usage
<GridItem area="header">
  <PrimaryButton>Save</PrimaryButton>
</GridItem>
```

## Responsive Layouts

Use theme-provided media queries for consistency:

```tsx
const ResponsiveGrid = styled.div`
  display: grid;
  gap: ${({theme}) => theme.spacing.medium};

  // Mobile first (default)
  grid-template-columns: 1fr;

  // Tablet and up
  ${({theme}) => theme.mediaQueries.tablet`
    grid-template-columns: 1fr 2fr;
  `}

  // Desktop and up
  ${({theme}) => theme.mediaQueries.desktop`
    grid-template-columns: 1fr 2fr 1fr;
  `}
`
```

## Component Prop Styling

When creating styled components that accept props, use TypeScript for type safety:

```tsx
interface ButtonProps {
  variant: "primary" | "secondary" | "danger"
  size?: "small" | "medium" | "large"
  fullWidth?: boolean
}

const Button = styled.button<ButtonProps>`
  // Size styling
  padding: ${({theme, size = "medium"}) => {
    switch (size) {
      case "small":
        return theme.spacing.small
      case "large":
        return theme.spacing.large
      default:
        return theme.spacing.medium
    }
  }};

  // Variant styling
  background: ${({theme, variant}) => {
    switch (variant) {
      case "primary":
        return theme.colors.primary
      case "danger":
        return theme.colors.danger
      default:
        return theme.colors.secondary
    }
  }};

  // Boolean prop styling
  width: ${({fullWidth}) => (fullWidth ? "100%" : "auto")};

  // Hover states
  &:hover {
    opacity: 0.9;
  }

  &:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }
`

// Usage with full type safety
<Button variant="primary" size="large" fullWidth>
  Save Changes
</Button>
```

## Transient Props

Use transient props (prefixed with `$`) for styled-components-only props that shouldn't be passed to the DOM:

```tsx
// ✅ GOOD: Transient props don't leak to DOM
const StyledDiv = styled.div<{$highlight?: boolean}>`
  background: ${({theme, $highlight}) =>
    $highlight ? theme.colors.highlight : "transparent"};
`

// Usage - $highlight won't appear in DOM
<StyledDiv $highlight={true}>Content</StyledDiv>

// ❌ AVOID: Non-transient props leak to DOM
const StyledDiv = styled.div<{highlight?: boolean}>`
  background: ${({theme, highlight}) =>
    highlight ? theme.colors.highlight : "transparent"};
`

// Results in <div highlight="true"> in DOM (invalid HTML)
<StyledDiv highlight={true}>Content</StyledDiv>
```

## File Organization

Keep styled components colocated with their usage:

```
components/
├── UserCard/
│   ├── UserCard.tsx
│   ├── UserCard.styles.ts    // Styled components here
│   └── index.ts

// Or inline if simple
components/
├── UserCard.tsx              // Component + styled components together
└── index.ts
```

**Guidelines:**
- **Inline** (same file): When styled components are simple and only used by one component
- **Separate file** (`.styles.ts`): When styled components are complex or shared within the directory
- **Shared styles** (`styles/`): Only for truly global styled components used across many features

## Summary

**Key Principles:**

1. **Theme Access:** Always via `${({theme}) => theme.property}` in styled-components
2. **Consolidation:** Use CSS selectors over many small styled components
3. **Spacing:** Prefer `gap` for spacing between elements
4. **Extension:** Use styled(Component) to extend existing components
5. **Props:** Use TypeScript for prop styling, transient props for style-only props
6. **Responsive:** Use theme media queries for consistency
7. **Organization:** Colocate styles with components

**When to Create Separate Styled Components:**
- Component is reused independently across multiple files
- Component needs distinct props/behavior
- Component has complex variant styling

**When to Use CSS Selectors:**
- Styling parent-child relationships
- Positional styling (first-child, last-child, nth-child)
- Pseudo-states (:hover, :focus, :active)
- Related elements within same component
