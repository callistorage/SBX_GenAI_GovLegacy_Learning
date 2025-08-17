# Lesson 1: AI Tool Fundamentals for Legacy Development

## Learning Objectives

By the end of this lesson, you will be able to:

1. **Set up and configure** AI coding assistants for government legacy development environments
2. **Apply basic prompting strategies** to understand and work with unfamiliar legacy code
3. **Use AI assistance effectively** across COBOL, legacy Java, and SQL simultaneously
4. **Understand AI tool limitations** and appropriate use cases in government environments

## Prerequisites

- Basic programming knowledge in at least one language
- Access to an AI coding assistant (GitHub Copilot, Claude Code, or similar)
- Text editor or IDE with AI tool integration

## Technology Overview

This lesson introduces three core legacy technologies common in government systems:

### COBOL (Common Business-Oriented Language)
- **Purpose**: Business data processing, financial calculations, administrative systems
- **Key characteristics**: English-like syntax, fixed-format columns, strong data typing
- **Government use**: Mainframe applications, social services, tax processing
- **External resources**: [COBOL Programming Course](https://www.ibm.com/docs/en/cobol-zos) | [Modern COBOL Guide](https://developer.ibm.com/cobol/)

### Legacy Java (Enterprise Java, circa 2000-2010)
- **Purpose**: Web applications, enterprise systems, middleware
- **Key characteristics**: Verbose syntax, extensive XML configuration, complex frameworks
- **Government use**: Web portals, service-oriented architecture, integration layers
- **External resources**: [Legacy Java Patterns](https://docs.oracle.com/javase/8/) | [Enterprise Java Guide](https://jakarta.ee/)

### Government Database Systems
- **Common systems**: DB2, Oracle (on mainframes), legacy SQL patterns
- **Key characteristics**: Strict naming conventions, extensive stored procedures, security constraints
- **Government use**: Data warehousing, transaction processing, reporting systems
- **External resources**: [DB2 Documentation](https://www.ibm.com/docs/en/db2) | [Government Data Standards](https://www.data.gov/)

## Lesson Structure

### Part 1: AI Tool Setup and Configuration (15 minutes)
### Part 2: Basic Prompting Strategies (20 minutes)
### Part 3: Multi-Technology Practice (15 minutes)
### Part 4: Limitations and Best Practices (10 minutes)

---

## Part 1: AI Tool Setup and Configuration

### Understanding Government Environment Constraints

Before diving into AI tool usage, it's crucial to understand the unique constraints of government development environments:

- **Security considerations**: Code may contain sensitive information
- **Network restrictions**: Limited or no internet access in some environments
- **Approval processes**: Tools may require security review before deployment
- **Compliance requirements**: Must adhere to federal development standards

### AI Tool Configuration Best Practices

#### 1. Privacy and Security Settings
```text
AI Tool Checklist:
□ Review data sharing and privacy policies
□ Configure offline mode if available
□ Understand what code/data is transmitted
□ Set up project-specific configurations
□ Document tool approval status for your organization
```

#### 2. Government-Friendly Prompting Approach
- **Be explicit about context**: Always specify the technology and government context
- **Request explanations**: Ask for rationale behind suggestions
- **Verify compliance**: Request consideration of security and compliance factors
- **Ask for alternatives**: Government environments often need multiple solution approaches

### 🔧 **Hands-On Exercise 1.1: Tool Setup**

**Objective**: Configure your AI tool for government legacy development

**Steps**:
1. Open your AI coding assistant
2. Test basic functionality with this simple prompt:
   ```text
   Explain what this code does in simple terms:
   
   IF WS-TOTAL-AMOUNT > 1000
       MOVE "HIGH" TO WS-RISK-LEVEL
   ELSE
       MOVE "LOW" TO WS-RISK-LEVEL
   END-IF
   ```
3. Note how the AI responds to COBOL syntax
4. Document any configuration options relevant to government use

**Expected outcome**: Understanding of your AI tool's baseline capability with legacy code

---

## Part 2: Basic Prompting Strategies

### The Government Legacy Development Prompting Framework

#### Strategy 1: Context-Rich Prompting
Always provide comprehensive context about:
- **Technology stack**: Specify COBOL, legacy Java version, database system
- **Government domain**: Tax processing, social services, regulatory compliance
- **Constraints**: Performance requirements, backwards compatibility, security needs

#### Strategy 2: Explanation-First Approach
Before making changes, always ask AI tools to:
1. Explain what the existing code does
2. Identify potential issues or improvements
3. Suggest changes with rationale
4. Consider government-specific implications

#### Strategy 3: Incremental Enhancement
- Start with understanding, then documentation, then small improvements
- Avoid large-scale changes without thorough analysis
- Maintain backwards compatibility unless explicitly changing requirements

### Effective Prompt Templates for Legacy Code

#### Template 1: Code Understanding
```text
Please analyze this [TECHNOLOGY] code used in a government [DOMAIN] system:

[CODE SAMPLE]

1. Explain what this code does in business terms
2. Identify any potential issues or inefficiencies
3. Note any patterns typical of legacy [TECHNOLOGY] development
4. Suggest how this might integrate with modern systems
```

#### Template 2: Documentation Generation
```text
Create comprehensive documentation for this government legacy code:

[CODE SAMPLE]

Include:
- Purpose and business logic
- Input/output specifications  
- Dependencies and assumptions
- Potential modernization considerations
- Security or compliance notes
```

#### Template 3: Improvement Suggestions
```text
Review this [TECHNOLOGY] code from a government system and suggest improvements:

[CODE SAMPLE]

Consider:
- Backwards compatibility requirements
- Government security standards
- Performance in legacy environments
- Maintainability for future developers
```

### 🔧 **Hands-On Exercise 1.2: Basic Prompting Practice**

Work through these three code samples using the prompting templates above:

**Sample A: COBOL Business Logic** (in `code-samples/cobol-sample.cob`)
**Sample B: Legacy Java Service** (in `code-samples/java-sample.java`)  
**Sample C: Database Query** (in `code-samples/sql-sample.sql`)

For each sample:
1. Use Template 1 to understand the code
2. Use Template 2 to generate documentation
3. Use Template 3 to identify improvements

**Time allocation**: 5-7 minutes per sample

---

## Part 3: Multi-Technology Practice

### Real Government Scenario: Tax Processing System Enhancement

You've been assigned to enhance a legacy tax processing system that includes:
- COBOL programs for tax calculation logic
- Java web services for taxpayer portal integration  
- SQL queries for data retrieval and reporting

The system needs minor enhancements to support a new tax credit calculation.

### 🔧 **Hands-On Exercise 1.3: Cross-Technology Integration**

**Scenario**: Adding support for a new "Green Energy Tax Credit" across all three technology layers.

**Your tasks**:
1. **COBOL Enhancement**: Modify calculation logic to include the new credit
2. **Java Integration**: Update the web service to pass the new credit data
3. **SQL Updates**: Create queries to retrieve and store the new credit information

**Approach**:
- Use your AI tool to understand how each component currently works
- Ask for suggestions on how to integrate the new functionality
- Request explanation of how the three components interact

**Files to work with**:
- `code-samples/tax-calc.cob` (COBOL calculation engine)
- `code-samples/tax-service.java` (Java web service)
- `code-samples/tax-queries.sql` (Database operations)

**Key prompting strategy**: For each component, ask your AI tool:
1. "How does this component fit into the overall tax processing system?"
2. "What would be the safest way to add new tax credit logic here?"
3. "How should this component communicate the new data to the other layers?"

---

## Part 4: Limitations and Best Practices

### Understanding AI Tool Limitations in Government Legacy Environments

#### Common Limitations
1. **Legacy syntax recognition**: AI tools may struggle with older language versions
2. **Government-specific patterns**: May not understand agency-specific conventions
3. **Security context**: Cannot assess classified or sensitive data implications
4. **Integration complexity**: May oversimplify complex legacy system interactions

#### Mitigation Strategies
1. **Provide extra context**: Include comments about government requirements
2. **Verify suggestions**: Always review AI recommendations against agency standards
3. **Use incremental approach**: Test small changes before large modifications
4. **Maintain human oversight**: AI assists, but developers decide

### Best Practices for Government Legacy Development

#### Do:
- ✅ Start with understanding before making changes
- ✅ Document AI tool usage in change management processes
- ✅ Verify suggestions against government coding standards
- ✅ Use AI to generate comprehensive documentation
- ✅ Ask for multiple solution alternatives

#### Don't:
- ❌ Blindly implement AI suggestions without review
- ❌ Share sensitive government data with cloud-based AI tools without approval
- ❌ Make large-scale changes without proper testing
- ❌ Ignore legacy system integration requirements
- ❌ Assume AI understands government compliance needs

### 🔧 **Hands-On Exercise 1.4: Limitation Awareness**

**Challenge**: Using the tax processing scenario from Exercise 1.3, intentionally test your AI tool's limitations:

1. **Ask for security advice**: "What security considerations should I have for this tax processing code?"
2. **Request compliance guidance**: "How does this code comply with federal data protection standards?"
3. **Seek integration advice**: "How should this integrate with mainframe batch processing?"

**Objective**: Understand where AI tools help and where human expertise remains essential.

---

## Assessment and Reflection

### Practical Challenge
**Time**: 15 minutes

Using your AI tool, take the provided legacy government system code sample (in `assessments/challenge-code.cob`) and:

1. Generate comprehensive documentation
2. Identify three potential improvements
3. Explain how you would safely implement one improvement
4. List three questions you would need to research beyond what the AI tool provided

### Reflective Analysis
**Time**: 10 minutes

Consider these questions:

1. **Tool Effectiveness**: Where did your AI tool excel, and where did it struggle?
2. **Government Context**: What government-specific knowledge did you need to provide?
3. **Confidence Level**: For which tasks would you feel confident using AI assistance?
4. **Learning Gaps**: What additional knowledge do you need to work effectively with legacy systems?

### Self-Assessment Checklist

After completing this lesson, you should be able to:

- [ ] Configure an AI tool appropriately for government legacy development
- [ ] Write effective prompts that provide necessary context for legacy code
- [ ] Use AI assistance to understand unfamiliar legacy code across multiple technologies
- [ ] Recognize when AI tool suggestions need additional human review
- [ ] Apply appropriate caution and best practices in government development environments

---

## Next Steps

**Lesson 2 Preview**: In the next lesson, we'll dive deeper into using AI tools for comprehensive legacy code analysis and documentation generation, building on the foundational skills developed here.

**Additional Practice**: Continue practicing with the provided code samples using different prompting strategies to build confidence and familiarity.

---

## Resources and Links

### External Learning Resources
- [IBM COBOL Programming Guide](https://www.ibm.com/docs/en/cobol-zos)
- [Legacy Java Enterprise Patterns](https://docs.oracle.com/javase/8/)
- [Government Database Standards](https://www.data.gov/)
- [Federal Secure Coding Standards](https://www.cisa.gov/secure-coding-standards)

### Internal Project Resources
- [Main Repository README](../../README.md)
- [Instructor Guide](instructor-guide.md)
- [Code Samples Directory](code-samples/)
- [Assessment Materials](assessments/)

---

*Lesson 1 of 10 | Estimated completion time: 45-60 minutes*
