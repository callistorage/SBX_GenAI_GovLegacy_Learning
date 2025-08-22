# Exercise 1.1: AI Tool Configuration Examples

## Purpose

This document provides concrete examples of how to configure popular AI coding assistants for government legacy development work, including screenshots, settings locations, and government-specific considerations.

## GitHub Copilot Configuration

### Privacy and Security Settings

**Location:** VS Code → Settings → Extensions → GitHub Copilot

#### 1. Data Sharing and Privacy Policies
```text
Setting: "github.copilot.advanced"
Recommended: Review the data usage policy at https://docs.github.com/en/copilot/privacy
Government Consideration: Understand that code snippets may be transmitted to OpenAI services
```

#### 2. Offline Mode Configuration
```text
Current Status: GitHub Copilot requires internet connection
Government Workaround: 
- Use in air-gapped environments: Not currently supported
- Alternative: Use locally hosted AI models or approved cloud services
```

#### 3. Telemetry Settings
```text
Setting: VS Code → Settings → "telemetry.telemetryLevel"
Recommended: Set to "off" or "error"
Government Reason: Prevents automatic transmission of usage data
```

#### 4. Project-Specific Configuration
```text
Location: .vscode/settings.json in your project
Configuration:
{
  "github.copilot.enable": {
    "*": false,
    "plaintext": false,
    "markdown": false,
    "javascript": true,
    "cobol": true,
    "java": true,
    "sql": true
  }
}
```

#### 5. Documentation for Approval
```text
Information to Document:
- Tool: GitHub Copilot
- Version: [Current version]
- Data transmission: Code snippets sent to OpenAI
- Privacy policy: https://docs.github.com/en/copilot/privacy
- Security review: [Date and approver]
- Project scope: [Specific government project]
```

## Claude Code Configuration

### Privacy and Security Settings

**Location:** Claude interface settings or API configuration

#### 1. Data Sharing Policies
```text
Review: https://www.anthropic.com/privacy
Key Points: 
- Conversations may be used to improve the service
- Option to request data deletion
- No training on conversations without explicit consent
```

#### 2. Project Isolation
```text
Best Practice: Use separate Claude accounts/sessions for:
- Personal projects
- Government work (with approval)
- Different classification levels
```

#### 3. Conversation Management
```text
Government Approach:
- Start fresh conversations for each work session
- Don't include sensitive data in prompts
- Clear conversation history regularly
- Document AI usage in project logs
```

## Cursor Configuration

### Privacy and Security Settings

**Location:** Cursor → Settings → Privacy

#### 1. Data Collection Settings
```text
Setting: "cursor.general.telemetry"
Recommended: Disabled for government use
Location: Settings → General → Data Collection
```

#### 2. AI Model Selection
```text
Options:
- GPT-4 (requires OpenAI account)
- Claude (requires Anthropic account) 
- Local models (best for air-gapped environments)
Government Preference: Local models when available
```

## General Government Configuration Principles

### Network and Security Configurations

#### 1. Proxy Settings (Common in Government)
```text
VS Code Configuration:
- File → Preferences → Settings
- Search: "proxy"
- Configure: "http.proxy" and "https.proxy"
- Certificate: Add government CA certificates if required
```

#### 2. Firewall Considerations
```text
Common Government Blocks:
- OpenAI API endpoints (api.openai.com)
- Anthropic API endpoints (api.anthropic.com)
- GitHub Copilot endpoints (copilot-proxy.githubusercontent.com)

Workaround Options:
- Request firewall exceptions (requires security review)
- Use approved cloud services only
- Implement local AI models
```

### Documentation Templates

#### Security Review Documentation
```text
AI Tool Security Review Request

Tool Name: [GitHub Copilot/Claude Code/etc.]
Purpose: Legacy government system development assistance
Data Transmission: [Describe what data is sent]
Storage Location: [Where data is processed/stored]
Retention Policy: [How long data is kept]
Access Controls: [Who can access the data]
Compliance Alignment: [NIST/FedRAMP/etc. considerations]
Risk Assessment: [Potential security risks]
Mitigation Measures: [How risks are addressed]
Approval Request: [Specific approval being requested]
```

#### Project Configuration Log
```text
AI Tool Configuration Log - Project: [Project Name]

Date: [Configuration Date]
Configured By: [Name and Role]
Tool: [AI Tool Name and Version]
Settings Applied: [List specific settings]
Rationale: [Why each setting was chosen]
Security Review: [Reference to security approval]
Testing Performed: [Basic functionality tests]
Issues Encountered: [Any configuration problems]
Resolution: [How issues were resolved]
Next Review Date: [When to review configuration again]
```

## Troubleshooting Common Government Configuration Issues

### Issue 1: Corporate Firewall Blocking AI Services
**Symptoms:** Connection errors, timeouts, authentication failures
**Solution:** 
1. Identify specific endpoints being blocked
2. Submit firewall exception request with business justification
3. Consider approved alternatives while waiting for approval

### Issue 2: Certificate Authority Problems
**Symptoms:** SSL/TLS certificate errors
**Solution:**
1. Install government CA certificates in VS Code
2. Configure proxy settings if required
3. Contact IT support for certificate issues

### Issue 3: Authentication Proxy Requirements
**Symptoms:** Authentication prompts, credential errors
**Solution:**
1. Configure proxy authentication in tool settings
2. Use service accounts if personal accounts aren't allowed
3. Coordinate with IT for authentication method

### Issue 4: Tool Performance in Government Networks
**Symptoms:** Slow responses, frequent timeouts
**Solution:**
1. Adjust timeout settings if configurable
2. Use during off-peak hours
3. Consider local alternatives for better performance

## Validation Checklist

After configuring your AI tool, verify:

- [ ] Tool responds to basic prompts
- [ ] No unauthorized data transmission (monitor network traffic if possible)
- [ ] Project-specific settings are applied
- [ ] Documentation is complete for security review
- [ ] Configuration meets your organization's requirements
- [ ] Backup configuration is saved
- [ ] Team members can replicate the setup

## Next Steps

1. **Document your specific configuration** using the templates above
2. **Test basic functionality** with non-sensitive code samples
3. **Submit for security review** if required by your organization
4. **Train team members** on the approved configuration
5. **Set up regular review schedule** for configuration updates

---

*Remember: Configuration requirements vary by agency and security level. Always consult with your organization's security and compliance teams before implementing AI tools in government environments.*
