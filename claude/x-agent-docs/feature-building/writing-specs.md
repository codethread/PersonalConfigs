# Core principles for technical specifications

## Problem definition establishes shared understanding

Technical specifications must begin with **clear problem articulation before proposing solutions**. Research from Google Engineering, Carnegie Mellon's Software Engineering Institute, and IEEE standards consistently shows that specifications fail when they jump directly to implementation details without establishing context. The most effective specifications dedicate substantial effort to documenting why something needs to be built, what business problem it solves, and what constraints exist. This problem-first approach reduces costly rework - studies indicate that **49% of digital transformation projects fail due to requirements issues**, primarily from misunderstood objectives.

## Requirements must be measurable and testable

Every requirement needs **quantifiable acceptance criteria** that enable objective verification. The principle appears across IEEE 29148:2018, ISTQB guidelines, and developer surveys as fundamental to specification success. Instead of stating "the system should be fast," specify "the system must respond within 200ms for 95% of requests under normal load conditions." This precision serves both developers implementing features and QA teams designing test cases. Requirements using structured formats like "Given X, when Y, then Z" naturally translate into executable tests, creating what the testing community calls **inherently testable specifications**.

## Structured documentation enables effective navigation

Successful specifications follow **consistent organizational patterns** that all stakeholders can navigate efficiently. Industry standards converge on essential sections: context and scope, functional requirements, non-functional requirements, interface definitions, and acceptance criteria. Google's engineering practices show that specifications between 10-20 pages hit the sweet spot for substantial projects - comprehensive enough for clarity, concise enough for busy stakeholders. Each section should serve a specific audience need while maintaining coherence across the whole document. The structure itself becomes a **communication framework** that ensures nothing critical is overlooked.

**Use the specification template**: A ready-to-use template is available at `.claude/agent-docs/specification-template.md` that implements these structural patterns and can be copied when creating new specifications.

## Visual communication enhances understanding

Research across Stack Overflow developer surveys and academic studies confirms that **diagrams, flowcharts, and visual aids significantly improve specification comprehension**. System context diagrams showing how new components fit within existing architecture prove especially valuable. Data flow illustrations clarify complex processes better than verbose descriptions. Visual elements should complement, not replace, textual specifications - they provide the high-level understanding that makes detailed requirements meaningful. Effective specifications use visuals strategically to reduce ambiguity and accelerate stakeholder alignment.

## Living documentation evolves with implementation

The most successful specifications are **maintained throughout the development lifecycle**, not frozen at project initiation. This "living documentation" approach, validated by Agile research and industry case studies, recognizes that implementation reveals insights requiring specification updates. Version control and change tracking become essential - not for bureaucratic compliance, but for maintaining shared understanding as projects evolve. Organizations reporting highest project success rates treat specifications as **collaborative working documents** updated within the same sprint as code changes.

# Practical checklist for technical specifications

**Note**: When creating a new specification, start by copying `.claude/agent-docs/specification-template.md` which provides a structured format implementing these principles.

## Foundation phase

☐ **Define the problem clearly** - State what business problem needs solving before describing any solution  
☐ **Establish scope boundaries** - Document what is included and explicitly what is NOT included  
☐ **Identify all stakeholders** - List who will use, implement, test, and maintain the system  
☐ **Document assumptions and dependencies** - Make implicit knowledge explicit  
☐ **Create a glossary** - Define domain-specific terms to eliminate ambiguity

## Requirements definition phase

☐ **Write testable functional requirements** - Each requirement must have clear pass/fail criteria  
☐ **Specify measurable non-functional requirements** - Include specific metrics for performance, security, reliability  
☐ **Use consistent requirement format** - Apply structured patterns like "The system SHALL [capability] WHEN [condition]"  
☐ **Assign unique identifiers** - Every requirement needs a traceable ID for lifecycle management  
☐ **Define priority levels** - Clearly distinguish must-have from nice-to-have features

## External dependency validation phase (Pre-flight checklist)

**Critical**: Execute this phase for any specification involving external tools, APIs, or libraries to prevent regressions from incorrect assumptions.

### Dependency discovery

☐ **Inventory all external dependencies** - List tools, APIs, libraries with specific version requirements  
☐ **Document platform differences** - Note macOS/Linux/Windows variations  
☐ **Classify criticality** - Distinguish required vs optional dependencies

### API research and validation

☐ **Test actual commands** - Run real commands and capture exact output, don't rely on documentation alone  
☐ **Document flag behaviors** - Note differences between similar flags (e.g., `--match` vs `--match-tab`)  
☐ **Capture real responses** - Use actual API calls to get true schemas, not documented ones  
☐ **Identify nullable fields** - Test which fields can be null vs required vs state-dependent  
☐ **Record error outputs** - Document actual error messages and failure modes

### Assumption validation

☐ **Create validation scripts** - Test each API assumption against the real tool  
☐ **Document deviations** - Note any differences from expected behavior  
☐ **Design accurate mocks** - Base mock implementations on captured real data, not documentation  
☐ **Plan integration tests** - Create tests that validate core assumptions against real tools

### Documentation requirements

For each external dependency, document:

- Exact command syntax with all flags
- Sample input and actual captured output
- Error conditions with real error messages
- Version-specific behaviors and quirks
- Response schemas from actual API calls
- Field-level notes on nullable/state-dependent values

## Design documentation phase

☐ **Include system context diagram** - Show how the solution fits within existing architecture  
☐ **Document interfaces precisely** - Use validated API information from pre-flight phase  
☐ **Address error scenarios** - Include real error cases discovered during validation  
☐ **Specify data structures** - Use actual schemas captured from external tools  
☐ **Document security requirements** - Authentication, authorization, data protection specifics

## Quality and validation phase

☐ **Define acceptance criteria** - Specific conditions that determine feature completion  
☐ **Include positive and negative test scenarios** - What the system should and shouldn't do  
☐ **Specify performance benchmarks** - Response times, throughput, resource usage limits  
☐ **Document compliance requirements** - Regulatory, legal, or organizational standards  
☐ **Create traceability matrix** - Link requirements to implementation tasks and test cases

## Stakeholder collaboration phase

☐ **Conduct multi-perspective reviews** - Include developers, testers, business analysts, users  
☐ **Document trade-off decisions** - Explain why certain approaches were chosen over alternatives  
☐ **Include visual aids** - Diagrams, mockups, flowcharts to enhance understanding  
☐ **Provide concrete examples** - Illustrate abstract requirements with specific scenarios  
☐ **Obtain formal sign-offs** - Stakeholder agreement on requirement completeness

## Maintenance and evolution phase

☐ **Establish version control** - Track all changes with clear revision history  
☐ **Update specifications with implementation insights** - Maintain as living documents  
☐ **Document lessons learned** - Capture what worked and what didn't for future projects  
☐ **Review specification effectiveness** - Measure if specs helped achieve project goals  
☐ **Refine templates and processes** - Continuously improve based on team feedback

## Critical success factors

**Avoid these proven specification failures:**

- Ambiguous language without measurable criteria
- Missing error handling and edge cases
- Technical implementation details before establishing requirements
- Single-author specifications without stakeholder input
- Static documents that become outdated during development
- **Assuming API behaviors without testing** - Always validate with real tools
- **Creating mocks from documentation** - Use captured real behavior instead
- **Ignoring platform differences** - Test on target platforms
- **Skipping external dependency validation** - Major cause of regressions

**Implement these proven practices:**

- Three Amigos sessions bringing together business, development, and testing perspectives
- Specification by example using concrete scenarios
- Regular specification reviews throughout development
- Automated quality checks for requirement consistency
- Clear change management process for requirement updates
- **Pre-flight validation** - Test all external dependencies before finalizing specs
- **Capture real outputs** - Document actual API responses and command outputs
- **Validation scripts** - Automate assumption testing against real tools
- **Mock-reality alignment** - Ensure mocks match actual tool behavior

## Common pitfalls with external dependencies

Based on real regression analysis, avoid these specific mistakes:

1. **Assuming similar flags behave identically** - Always test each flag's actual behavior
2. **Guessing JSON field names or types** - Capture real responses to verify
3. **Creating mocks from documentation alone** - Documentation often diverges from implementation
4. **Ignoring version-specific behaviors** - Different versions may have different outputs
5. **Not testing error conditions** - Error handling design requires real error outputs
6. **Skipping platform testing** - Tools behave differently across operating systems
7. **Complex command compositions** - Test pipes and command chains thoroughly

## Red flags requiring extra validation

Pay special attention when specifications involve:

- Commands with multiple filtering options
- APIs returning different schemas based on state
- Tools with poor or outdated documentation
- Platform-specific implementations
- Undocumented features or behaviors
- Complex data transformations or parsing

This checklist synthesizes decades of industry experience, formal standards from IEEE and ISO, empirical research from leading software organizations, and hard-learned lessons from production regressions. Teams that follow these principles—especially the external dependency validation phase—consistently report **reduced rework, faster development cycles, and higher stakeholder satisfaction**. The key is adapting these universal principles to your specific context while maintaining the rigor that makes specifications truly useful for both implementation and verification. The pre-flight validation phase has proven critical in preventing costly regressions caused by incorrect assumptions about external tools and APIs.
