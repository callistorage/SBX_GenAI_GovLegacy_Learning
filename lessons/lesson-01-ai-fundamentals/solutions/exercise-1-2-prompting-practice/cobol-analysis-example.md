# Exercise 1.2: COBOL Analysis - Worked Example

## Purpose

This document demonstrates a complete prompting session for analyzing the COBOL sample code, showing how to apply the three prompt templates effectively and what to expect from AI tool responses.

## Code Sample Being Analyzed

**File:** `code-samples/cobol-sample.cob`

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RISK-ASSESSMENT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-APPLICANT-DATA.
          05 WS-CREDIT-SCORE      PIC 9(3).
          05 WS-ANNUAL-INCOME     PIC 9(8)V99.
          05 WS-DEBT-RATIO        PIC 9(2)V99.
          05 WS-EMPLOYMENT-YEARS  PIC 9(2).
       
       01 WS-RISK-FACTORS.
          05 WS-RISK-LEVEL        PIC X(4).
          05 WS-APPROVAL-FLAG     PIC X(1).
          05 WS-REVIEW-FLAG       PIC X(1).
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM CALCULATE-RISK
           PERFORM DETERMINE-APPROVAL
           GOBACK.
       
       CALCULATE-RISK.
           IF WS-CREDIT-SCORE < 600
              MOVE "HIGH" TO WS-RISK-LEVEL
           ELSE
              IF WS-DEBT-RATIO > 0.40
                 MOVE "MED" TO WS-RISK-LEVEL
              ELSE
                 MOVE "LOW" TO WS-RISK-LEVEL
              END-IF
           END-IF.
       
       DETERMINE-APPROVAL.
           IF WS-RISK-LEVEL = "LOW"
              MOVE "Y" TO WS-APPROVAL-FLAG
              MOVE "N" TO WS-REVIEW-FLAG
           ELSE
              IF WS-RISK-LEVEL = "MED" AND WS-EMPLOYMENT-YEARS > 2
                 MOVE "Y" TO WS-APPROVAL-FLAG
                 MOVE "Y" TO WS-REVIEW-FLAG
              ELSE
                 MOVE "N" TO WS-APPROVAL-FLAG
                 MOVE "Y" TO WS-REVIEW-FLAG
              END-IF
           END-IF.
```

## Template 1: Code Understanding

### Prompt Used:
```text
Please analyze this COBOL code used in a government benefit system:

[COBOL code sample above]

1. Explain what this code does in business terms
2. Identify any potential issues or inefficiencies  
3. Note any patterns typical of legacy COBOL development
4. Suggest how this might integrate with modern systems
```

### AI Response (GitHub Copilot Example):

> **Business Logic Analysis:**
> This COBOL program implements a risk assessment system for government benefit applications. It evaluates applicants based on credit score, debt-to-income ratio, and employment history to determine approval and review requirements.

> **Program Flow:**
> 1. **Risk Calculation**: Assigns risk levels (HIGH/MED/LOW) based on credit score and debt ratio
> 2. **Approval Decision**: Determines if application should be approved and if manual review is needed
> 3. **Decision Rules**:
>    - Credit score < 600 = HIGH risk (rejected, requires review)
>    - Debt ratio > 40% = MEDIUM risk
>    - Low risk + employment stability = auto-approval
>    - Medium risk + 2+ years employment = conditional approval with review

> **Potential Issues:**
> - Hard-coded thresholds (600 credit score, 0.40 debt ratio) should be configurable
> - No input validation or error handling
> - Limited risk categories (only 3 levels)
> - No audit trail or logging capability

> **Legacy COBOL Patterns:**
> - Fixed-format data structures with PIC clauses for data typing
> - Procedural flow with PERFORM statements
> - Simple conditional logic with nested IF statements
> - Working storage for intermediate calculations

> **Modern Integration Possibilities:**
> - Expose as web service through COBOL-to-REST adapters
> - Extract business rules into configurable rule engine
> - Add database integration for persistent storage
> - Implement real-time API calls for credit score verification

### Analysis of AI Response:

✅ **Strengths:**
- Correctly identifies government benefit context
- Understands the business logic flow
- Recognizes COBOL-specific syntax patterns
- Provides practical modernization suggestions
- Identifies key technical limitations

⚠️ **Areas Requiring Human Expertise:**
- Doesn't mention government compliance requirements (fair lending, etc.)
- No consideration of security or privacy implications
- Modernization suggestions need government architecture review

## Template 2: Documentation Generation

### Prompt Used:
```text
Create comprehensive documentation for this government legacy code:

[COBOL code sample above]

Include:
- Purpose and business logic
- Input/output specifications
- Dependencies and assumptions  
- Potential modernization considerations
- Security or compliance notes
```

### AI Response (Claude Code Example):

> # RISK-ASSESSMENT Program Documentation

> ## Purpose and Business Logic
> **Program Name:** RISK-ASSESSMENT  
> **Function:** Automated risk evaluation for government benefit applications
> 
> **Business Rules:**
> - **High Risk:** Credit score below 600 (automatic rejection)
> - **Medium Risk:** Debt-to-income ratio above 40%
> - **Low Risk:** All other applicants
> 
> **Approval Logic:**
> - Low risk applicants: Automatic approval, no review required
> - Medium risk with 2+ years employment: Conditional approval with mandatory review
> - High risk or insufficient employment: Rejection with mandatory review

> ## Input/Output Specifications
> 
> **Input Data Structure (WS-APPLICANT-DATA):**
> - `WS-CREDIT-SCORE`: 3-digit numeric (000-999)
> - `WS-ANNUAL-INCOME`: 8-digit currency with 2 decimal places
> - `WS-DEBT-RATIO`: Percentage with 2 decimal places (0.00-99.99)
> - `WS-EMPLOYMENT-YEARS`: 2-digit numeric (00-99)
> 
> **Output Data Structure (WS-RISK-FACTORS):**
> - `WS-RISK-LEVEL`: 4-character text ("HIGH", "MED", "LOW")
> - `WS-APPROVAL-FLAG`: Single character ("Y"/"N")
> - `WS-REVIEW-FLAG`: Single character ("Y"/"N")

> ## Dependencies and Assumptions
> **Data Dependencies:**
> - Credit score data must be current (assumption: within 90 days)
> - Employment verification completed before processing
> - Income figures represent verified annual amounts
> 
> **System Assumptions:**
> - No real-time external system integration
> - All input data pre-validated before program execution
> - Processing occurs in batch mode
> 
> **Business Assumptions:**
> - Risk thresholds align with current government lending guidelines
> - Employment history is primary stability indicator
> - Manual review capacity exists for flagged applications

> ## Modernization Considerations
> **Technical Modernization:**
> - Convert to microservice architecture for scalability
> - Implement configurable business rules engine
> - Add real-time integration with credit bureaus
> - Enhance data validation and error handling
> 
> **Process Modernization:**
> - Enable real-time application processing
> - Add applicant notification capabilities
> - Implement audit logging for compliance
> - Support appeal and reconsideration workflows

> ## Security and Compliance Notes
> **Data Security:**
> - Contains PII (credit scores, income data) requiring encryption at rest
> - Processing must comply with Fair Credit Reporting Act (FCRA)
> - Audit trails required for all decisions
> 
> **Compliance Considerations:**
> - Risk assessment criteria must comply with equal lending regulations
> - Decision logic must be transparent for fair lending audits
> - Data retention policies must align with government record-keeping requirements
> 
> **Access Controls:**
> - Processing should be restricted to authorized personnel only
> - Review functions may require higher privilege levels
> - Decision overrides must be logged and justified

### Analysis of AI Response:

✅ **Excellent Documentation Quality:**
- Comprehensive business logic explanation
- Detailed input/output specifications
- Practical modernization roadmap
- Government compliance awareness

✅ **Government-Specific Insights:**
- Mentions Fair Credit Reporting Act (FCRA)
- Considers equal lending regulations
- Addresses audit and transparency requirements
- Recognizes PII handling needs

⚠️ **Human Review Still Needed:**
- Specific agency requirements may differ
- Security classification levels not addressed
- Integration with agency-specific systems requires domain knowledge

## Template 3: Improvement Suggestions

### Prompt Used:
```text
Review this COBOL code from a government system and suggest improvements:

[COBOL code sample above]

Consider:
- Backwards compatibility requirements
- Government security standards
- Performance in legacy environments
- Maintainability for future developers
```

### AI Response (GitHub Copilot Example):

> # COBOL Risk Assessment - Improvement Recommendations

> ## Backwards Compatibility Improvements
> 
> **1. Parameterized Thresholds**
> ```cobol
> 01 WS-BUSINESS-RULES.
>    05 WS-MIN-CREDIT-SCORE     PIC 9(3) VALUE 600.
>    05 WS-MAX-DEBT-RATIO       PIC 9(2)V99 VALUE 0.40.
>    05 WS-MIN-EMPLOYMENT-YEARS PIC 9(2) VALUE 2.
> ```
> *Benefit:* Rules can be updated without code changes while maintaining existing interfaces.

> **2. Enhanced Data Validation**
> ```cobol
> VALIDATE-INPUT.
>     IF WS-CREDIT-SCORE < 300 OR WS-CREDIT-SCORE > 850
>        MOVE "INPUT-ERROR" TO WS-ERROR-CODE
>        GO TO ERROR-HANDLER
>     END-IF.
> ```
> *Benefit:* Prevents invalid data processing while maintaining program structure.

> ## Government Security Enhancements
> 
> **1. Audit Trail Implementation**
> ```cobol
> LOG-DECISION.
>     MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP
>     WRITE AUDIT-RECORD FROM WS-AUDIT-DATA
>     IF FILE-STATUS NOT = "00"
>        PERFORM ERROR-HANDLER
>     END-IF.
> ```
> 
> **2. Data Masking for Logs**
> ```cobol
> MASK-SENSITIVE-DATA.
>     MOVE "XXX" TO WS-MASKED-CREDIT-SCORE(1:3)
>     MOVE WS-CREDIT-SCORE(1:1) TO WS-MASKED-CREDIT-SCORE(4:1).
> ```

> ## Performance Optimizations
> 
> **1. Eliminate GOBACK for Batch Processing**
> ```cobol
> MAIN-PROCESS.
>     PERFORM UNTIL END-OF-FILE = "Y"
>        READ INPUT-FILE
>        IF NOT END-OF-FILE
>           PERFORM CALCULATE-RISK
>           PERFORM DETERMINE-APPROVAL
>           WRITE OUTPUT-RECORD
>        END-IF
>     END-PERFORM.
> ```
> 
> **2. Pre-compile Calculations**
> ```cobol
> 01 WS-DEBT-RATIO-CALC.
>    05 WS-DEBT-RATIO-PERCENT   PIC 9(2)V99 COMP-3.
> ```
> *Use COMP-3 for faster arithmetic operations in batch processing*

> ## Maintainability Improvements
> 
> **1. Modular Design with Copybooks**
> ```cobol
> COPY RISK-ASSESSMENT-DATA.
> COPY BUSINESS-RULES.
> COPY ERROR-HANDLING.
> ```
> 
> **2. Standardized Error Handling**
> ```cobol
> ERROR-HANDLER.
>     EVALUATE WS-ERROR-CODE
>        WHEN "INPUT-ERROR"
>           PERFORM LOG-INPUT-ERROR
>        WHEN "CALC-ERROR"  
>           PERFORM LOG-CALCULATION-ERROR
>        WHEN OTHER
>           PERFORM LOG-UNKNOWN-ERROR
>     END-EVALUATE.
> ```
> 
> **3. Enhanced Comments and Documentation**
> ```cobol
> * Business Rule: Credit scores below 600 indicate high risk
> * Reference: Agency Policy Document XYZ-2024-001
> * Last Updated: [Date] by [Developer]
>        IF WS-CREDIT-SCORE < WS-MIN-CREDIT-SCORE
> ```

> ## Government-Specific Considerations
> 
> **Security Standards Compliance:**
> - Implement FIPS 140-2 compliant encryption for sensitive data
> - Add role-based access controls for different decision levels
> - Include data classification headers in all modules
> 
> **Legacy Environment Performance:**
> - Use SORT statements for large batch processing
> - Implement checkpoint/restart capabilities for long-running jobs
> - Optimize file access patterns for mainframe efficiency
> 
> **Future Developer Support:**
> - Create comprehensive program documentation
> - Implement standardized naming conventions
> - Add inline business rule explanations

### Analysis of AI Response:

✅ **Excellent Government-Aware Improvements:**
- Addresses backwards compatibility concerns
- Provides security-focused enhancements
- Considers legacy mainframe performance
- Offers practical maintainability solutions

✅ **Technical Quality:**
- Code examples are syntactically correct
- Suggestions are incrementally implementable
- Performance improvements are appropriate for COBOL/mainframe
- Security suggestions align with government standards

⚠️ **Implementation Considerations:**
- Changes would require testing in agency-specific environment
- Security implementations need to align with agency's encryption standards
- Performance improvements should be validated against actual workloads

## Key Learning Points

### What This Exercise Demonstrates:
1. **Progressive Understanding**: Each template builds deeper comprehension
2. **Government Context Matters**: AI tools can provide government-aware responses when prompted correctly
3. **Human Expertise Required**: AI provides excellent starting points but requires domain expert validation
4. **Tool Quality Varies**: Different AI tools provide different levels of government context awareness

### Best Practices Validated:
- **Context-rich prompting** produces significantly better results
- **Template-based approaches** ensure comprehensive analysis
- **Government-specific prompts** elicit compliance and security considerations
- **Iterative questioning** builds deeper understanding than single prompts

### Next Steps for Real Implementation:
1. **Validate suggestions** against agency-specific requirements
2. **Test code changes** in development environment
3. **Review security implications** with agency security team
4. **Document changes** for compliance and audit purposes

---

*This worked example demonstrates how effective prompting can extract valuable insights from AI tools while highlighting areas where human government expertise remains essential.*
