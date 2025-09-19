# Project Context Discovery

## IMPORTANT: For All Agents and Subagents

When starting ANY new task or when invoked as a subagent, you MUST immediately run the following command to discover project documentation:

```bash
bun ~/.claude/x-hooks/agent-docs-injection.ts
```

This command will provide you with a list of all AGENTS.md and README.md files in the project, which contain critical documentation.
