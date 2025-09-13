# ast-grep: Structural Code Search

Quick reference for searching TypeScript/TSX codebases using ast-grep's structural patterns.

## Basic Usage

```bash
# Search pattern in current directory
ast-grep -p 'pattern' -l typescript

# Search with file output only
ast-grep -p 'pattern' -l typescript

# Get JSON output for tooling
ast-grep -p 'pattern' -l typescript --json

# Debug pattern matching
ast-grep -p 'pattern' -l typescript --debug-query
```

## Essential Patterns for TypeScript/TSX

### React Hooks
```bash
# useState patterns
ast-grep -p 'const [$STATE, $SETTER] = useState($INIT)' -l typescript
ast-grep -p 'useState($$$)' -l typescript

# useEffect patterns  
ast-grep -p 'useEffect(() => { $$$ }, [$$$])' -l typescript
ast-grep -p 'useEffect($$$)' -l typescript

# useMemo patterns
ast-grep -p 'const $NAME = useMemo(() => { $$$ }, [$$$])' -l typescript
ast-grep -p 'useMemo($$$)' -l typescript

# Custom hook calls
ast-grep -p 'use$HOOK($$$)' -l typescript
```

### TypeScript Types & Interfaces
```bash
# Interface definitions
ast-grep -p 'interface $NAME { $$$ }' -l typescript

# Type definitions  
ast-grep -p 'type $NAME = $$$' -l typescript

# Function type annotations
ast-grep -p 'function $NAME($$$): $TYPE { $$$ }' -l typescript

# Generic functions
ast-grep -p 'function $NAME<$GENERICS>($$$) { $$$ }' -l typescript

# Optional parameters
ast-grep -p 'function $NAME($PARAM?: $TYPE) { $$$ }' -l typescript
```

### React Components (TSX)
```bash
# React.FC components
ast-grep -p 'const $NAME: React.FC<$PROPS> = ($$$) => { $$$ }' -l typescript

# Self-closing JSX components
ast-grep -p '<$COMPONENT $$$PROPS />' -l tsx

# JSX components with children
ast-grep -p '<$COMPONENT $$$PROPS>$$$</$COMPONENT>' -l tsx

# Component props destructuring
ast-grep -p 'const { $$$PROPS } = props' -l typescript
```

### Import/Export Patterns
```bash
# Import statements
ast-grep -p 'import $MODULE from $PATH' -l typescript
ast-grep -p 'import { $$$ITEMS } from $PATH' -l typescript

# Export statements
ast-grep -p 'export const $NAME = $$$' -l typescript
ast-grep -p 'export { $$$ITEMS }' -l typescript
```

### Apollo GraphQL (Common in this codebase)
```bash
# Query hook patterns
ast-grep -p 'useQuery($$$)' -l typescript
ast-grep -p 'const { $$$OPTIONS } = useQuery($$$)' -l typescript

# Mutation hook patterns  
ast-grep -p 'useMutation($$$)' -l typescript
ast-grep -p 'const [$MUTATION, { $$$OPTIONS }] = useMutation($$$)' -l typescript
```

### Common JavaScript Patterns
```bash
# Object destructuring
ast-grep -p 'const { $$$PROPS } = $OBJ' -l typescript

# Array destructuring  
ast-grep -p 'const [$$$ITEMS] = $ARRAY' -l typescript

# Arrow functions
ast-grep -p '($$$PARAMS) => $$$BODY' -l typescript

# Async functions
ast-grep -p 'async function $NAME($$$) { $$$ }' -l typescript

# Try-catch blocks
ast-grep -p 'try { $$$ } catch ($ERR) { $$$ }' -l typescript
```

## Pattern Variables

- `$VAR` - single identifier or expression  
- `$$$` - multiple statements/expressions (zero or more)
- `$$$VAR` - multiple statements with name (for reuse)

## Essential Options

| Flag | Description |
|------|-------------|
| `-l typescript` | Parse as TypeScript (use for .ts files) |  
| `-l tsx` | Parse as TSX (use for .tsx files) |
| `--json` | Structured JSON output |
| `--debug-query` | Show AST structure of pattern |

## Pro Tips

1. **Always specify language**: Use `-l typescript` or `-l tsx`
2. **Start simple**: Begin with basic patterns, add complexity gradually  
3. **Use JSON for tooling**: `--json` flag provides structured data
4. **Debug patterns**: Use `--debug-query` when patterns don't match as expected
5. **Test patterns**: Validate on known code before running on large codebases
