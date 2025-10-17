---
allowed-tools: Bash(claude-ui-test:*), Bash(echo:*)
argument-hint: [description of UI changes to test]
description: Test UI changes with specialized UI testing agent and custom MCP configuration
---

# UI Testing Agent

Use this command when you need to verify visual changes, user interface functionality, or interactive elements that require specialized UI testing tools and browser automation.

## When to Use This Command

Invoke this UI testing agent when:

- Testing visual changes to components, layouts, or styles
- Verifying responsive design across different viewport sizes
- Checking accessibility compliance (ARIA labels, keyboard navigation)
- Validating user interaction flows (forms, buttons, navigation)
- Detecting visual regressions after code changes
- Testing browser-specific behavior or compatibility
- Verifying animations and transitions work correctly

## How It Works

This command delegates to a specialized Claude instance configured with:

1. **Custom MCP Configuration**: Uses `~/.claude/x-mcp/ui.json` which should include browser automation tools, screenshot capabilities, and visual testing utilities
2. **UI Testing System Prompt**: Optimized for visual analysis, accessibility checks, and interactive element testing
3. **Isolated Context**: Runs in a separate Claude instance via `--print` mode to maintain context separation

## Usage

The command takes your testing instructions as arguments:

```
/test-ui Check that the new login form is accessible and works on mobile
```

## Instructions for Main Agent

When you need to test UI changes:

1. **Formulate a clear testing prompt** describing:
   - What UI elements changed
   - What needs to be verified
   - Any specific scenarios or edge cases to test

2. **Invoke the wrapper**:

   ```bash
   echo "Your detailed testing prompt here" | claude-ui-test
   ```

3. **Parse the response**: The UI testing agent will return text-based findings including:
   - What was tested
   - Pass/fail status for each aspect
   - Screenshots or visual descriptions (if available)
   - Specific issues found with file locations
   - Recommendations for fixes

4. **Report findings**: Summarize the UI testing results for the user, including any issues that need to be addressed.

## Example Invocation

```bash
echo "Test the new dashboard layout:
- I is currently running on localhost:3000
- Verify responsive behavior on mobile (375px) and tablet (768px) viewports
- Check that all navigation links are keyboard accessible
- Validate ARIA labels on interactive elements
- Ensure no console errors appear on page load
- Test the search functionality works correctly" | claude-ui-test
```

## Requirements

- **MCP Config**: Ensure `~/.claude/x-mcp/ui.json` exists and contains UI testing tools
- **Browser Tools**: MCP server should provide browser automation (Playwright, Puppeteer, etc.)
- **Visual Testing**: Screenshot comparison tools should be configured in the MCP server

## Testing Scope

The UI testing agent can verify:

- ✅ Visual appearance and layout
- ✅ Responsive design breakpoints
- ✅ Accessibility compliance
- ✅ Interactive element functionality
- ✅ Form validation and submissions
- ✅ Navigation and routing
- ✅ Error states and edge cases
- ✅ Browser console warnings/errors

## User Request

$ARGUMENTS

---

**Delegating to UI testing agent with custom MCP configuration...**
