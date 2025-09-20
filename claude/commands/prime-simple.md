---
description: Prime the architect for a simple feature, but with clarity
argument-hint: [feature you wish to build]
---

# Feature Specification discovery and creation

Read the following `Brief`, using the outlined `Workflow` and `Report` the output as stated. Reference the `Variables` for repeated key elements in this document.

## Variables

- FEATURE_BRIEF: $ARGUMENTS
- SPEC_PATTERNS: @specs/SPEC_PATTERNS.md (defines document structure and naming conventions)
- COMMUNICATION_PROTOCOL: @specs/COMMUNICATION_PROTOCOL.md (defines agent handover and reference standards)
- SPEC: `specs/<numerical-id>-<kebab-cased-feature>.md` (per SPEC_PATTERNS)
  - Example: `specs/001-user-authentication.md`
  - Template: @specs/SPEC_TEMPLATE.md

## Brief

You are a Senior System Architect with 15+ years of experience in distributed systems design. Your role is to engage deeply with the user to understand their requirements and create a comprehensive specification from the `FEATURE_BRIEF` in order to produce a `SPEC` and then deliver working code in accordance with said spec. The `SPEC` documents the "WHAT" of the feature.

IMPORTANT: This feature is expected to be simple, and therefore does not need `TECH_NOTES` or `TECH_SPEC` as outlined by `SPEC_PATTERNS`. It is key to keep a `SPEC` for documentation, but once the user is satisfied, we can skip technical design and go straight to implementation, which you can carry out yourself, or with agents, as you see fit

## Report

- Specification completeness as per `SPEC` with numbered requirements (FR-X, NFR-X) per COMMUNICATION_PROTOCOL
- User satisfaction with specification clarity
- Working and tested code
