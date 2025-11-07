# Agent instructions (via CLAUDE.md)

## General information

- the year is 2025
- if the Update or Write tools fail to edit files after a few tries, stop and prompt the user to restart the session

## Ways of working

- read relavent README.md files when working in nested directories
- always check CLAUDE.md files after completing work

## Skills

- Always load a Skill if you think it might be useful (better to not need it, than miss valuable context)
- **IMPORTANT: always load `lang-*` skills when working inappropriate files**
  - e.g `lang-react` Skill must be used when attempting to write/update/review React code

## Task Agent Workflow

### CRITICAL: Always Check for Existing Agents Before Creating New Ones

Before using the Task tool to spawn a new agent, you MUST:

1. **Extract current session ID** from the session start message:

   ```
   Initialized agent context session: <session-id>
   ```

2. **Check for existing agents** using `cc-logs--extract-agents`:

   ```bash
   cc-logs--extract-agents <session-id>
   ```

3. **Evaluate resumption** - Review the output to determine if an existing agent can be resumed:
   - Does an agent already exist that did similar work?
   - Can the new task build upon previous agent's context?
   - Would resuming save time/cost compared to starting fresh?

4. **Resume or create**:
   - **If suitable agent exists**: Use Task tool with `resume: "<agent-id>"` parameter
   - **If no suitable agent**: Create new agent with Task tool

### Why This Matters

- **Cost efficiency**: Resuming agents avoids re-processing context
- **Better results**: Agents maintain full context from previous execution
- **Faster execution**: No need to re-gather information already collected
- **Token savings**: Avoids redundant analysis and research

### Example Workflow

```bash
# 1. Get session ID (shown at session start)
# "Initialized agent context session: 61a79fc9-2fac-4dc8-97ff-eee2a775e0a9"

# 2. Check existing agents
cc-logs--extract-agents 61a79fc9-2fac-4dc8-97ff-eee2a775e0a9

# Output shows:
# Agent ID: 87cad6f3
# Model: haiku
# Description: Analyze test files
# Prompt: Please analyze the test files in oven/tests...

# 3a. If resuming:
Task({
  resume: "87cad6f3",
  prompt: "Now analyze the implementation files that correspond to those tests"
})

# 3b. If creating new:
Task({
  subagent_type: "general-purpose",
  prompt: "Completely different task that no existing agent covers"
})
```

### Exceptions

Only skip agent checking when:

- This is the very first agent in the session
- The task is completely unrelated to any previous work
- Time-sensitive situations where checking would delay critical work

## Code comments

### Changes to code

When commenting on code, always write in reference to the latest state of the code, unless the change should be explicitly called out.

- **BAD:** comment states the obvious
  ```js
  // fetch ther user
  const userInfo = await externalUserApi(userId);
  ```
- **GOOD:** comment explains ambiguous argument needed for api with reason
  ```js
  // api requires explicit null to avoid nested table joins, we aggregate extra data elsewhere
  const userInfo = await externalUserApi(userId, null);
  ```
- **BAD:** comment explains a change relative to old code, which no longer makes sense in isolation (this is the role of `git diff` and changelogs)
  ```diff
  - // api requires explicit null to avoid nested table joins, we aggregate extra data elsewhere
  - const userInfo = await externalUserApi(userId, null)
  + // api no longer requires null to avoid joins
  + const userInfo = await externalUserApiV2(userId)
  ```
- **BAD:** comment removed on improved api
  ```diff
  - // api requires explicit null to avoid nested table joins, we aggregate extra data elsewhere
  - const userInfo = await externalUserApi(userId, null)
  + const userInfo = await externalUserApiV2(userId)
  ```
