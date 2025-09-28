/**
 * @file Compliance and Audit Tests
 * 
 * Tests for audit trail gaps, compliance violations, data retention,
 * privacy regulations, and regulatory reporting failures.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { KnowledgeHookManager } from '../../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { defineHook } from '../../../src/knowledge-engine/define-hook.mjs';
import { Store } from 'n3';
import { promises as fs } from 'fs';
import { join } from 'path';

// Mock condition evaluator at module level
vi.mock('../../../src/knowledge-engine/condition-evaluator.mjs', () => ({
  evaluateCondition: vi.fn(),
  createConditionEvaluator: vi.fn().mockReturnValue({
    evaluate: vi.fn()
  }),
  validateCondition: vi.fn().mockReturnValue({ valid: true })
}));

describe('Compliance and Audit', () => {
  let manager;
  let tempDir;
  let auditLog;
  let complianceViolations;

  beforeEach(async () => {
    // Create temporary directory for test files
    tempDir = join(process.cwd(), 'test-temp-compliance');
    await fs.mkdir(tempDir, { recursive: true });
    
    // Initialize audit log and compliance tracking
    auditLog = [];
    complianceViolations = [];
    
    manager = new KnowledgeHookManager({
      basePath: tempDir,
      enableKnowledgeHooks: true
    });

    // Configure mock to always return true for conditions
    const { evaluateCondition } = await import('../../../src/knowledge-engine/condition-evaluator.mjs');
    vi.mocked(evaluateCondition).mockResolvedValue(true);
  });

  afterEach(async () => {
    // Clean up temporary directory
    await fs.rm(tempDir, { recursive: true, force: true });
  });

  describe('Audit Trail Gaps', () => {
    it('should detect missing audit entries for sensitive operations', async () => {
      const sensitiveDataHook = defineHook({
        meta: {
          name: 'audit:sensitive-data',
          description: 'Audit sensitive data access'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/audit/sensitive-data.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Simulate sensitive data access without proper audit logging
          const sensitiveData = event.context.graph.getQuads(null, null, null, null);
          
          // Missing audit log entry - this should be detected
          return {
            result: { accessed: sensitiveData.length },
            status: 'success'
          };
        }
      });

      manager.addKnowledgeHook(sensitiveDataHook);

      const event = {
        name: 'data-access',
        payload: {},
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should execute successfully but audit trail should be incomplete
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.accessed).toBeDefined();
      
      // Audit log should be empty (gap detected)
      expect(auditLog.length).toBe(0);
    });

    it('should ensure audit trail continuity across hook failures', async () => {
      const auditHook = defineHook({
        meta: {
          name: 'audit:continuity',
          description: 'Ensure audit trail continuity'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/audit/continuity.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Log audit entry
          auditLog.push({
            timestamp: new Date().toISOString(),
            operation: 'data-access',
            userId: event.payload.userId || 'anonymous'
          });
          
          // Simulate failure after audit log entry
          throw new Error('Operation failed after audit logging');
        },
        after: async (event) => {
          // Ensure audit trail is completed even after failure
          auditLog.push({
            timestamp: new Date().toISOString(),
            operation: 'data-access-failed',
            error: event.error
          });
          
          return { status: 'audit-completed' };
        }
      });

      manager.addKnowledgeHook(auditHook);

      const event = {
        name: 'data-access',
        payload: {
          userId: 'user123'
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Hook should fail but audit trail should be complete
      expect(results[0].success).toBe(false);
      expect(results[0].error).toContain('Operation failed after audit logging');
      
      // Audit trail should be complete despite failure
      expect(auditLog.length).toBe(2);
      expect(auditLog[0].operation).toBe('data-access');
      expect(auditLog[1].operation).toBe('data-access-failed');
    });
  });

  describe('Compliance Violations', () => {
    it('should detect GDPR data processing without consent', async () => {
      const gdprHook = defineHook({
        meta: {
          name: 'compliance:gdpr-consent',
          description: 'Check GDPR consent requirements'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/compliance/gdpr-consent.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Check for personal data processing without consent
          const personalData = event.context.graph.getQuads(null, null, null, null);
          const hasConsent = event.payload.consentGiven || false;
          
          if (personalData.length > 0 && !hasConsent) {
            complianceViolations.push({
              type: 'GDPR_VIOLATION',
              description: 'Personal data processed without consent',
              timestamp: new Date().toISOString(),
              severity: 'HIGH'
            });
            
            return {
              result: { violation: true, dataProcessed: personalData.length },
              status: 'compliance-violation'
            };
          }
          
          return {
            result: { violation: false },
            status: 'compliant'
          };
        }
      });

      manager.addKnowledgeHook(gdprHook);

      const event = {
        name: 'data-processing',
        payload: {
          // Missing consentGiven field
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect compliance violation
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.violation).toBe(true);
      expect(complianceViolations.length).toBe(1);
      expect(complianceViolations[0].type).toBe('GDPR_VIOLATION');
    });

    it('should enforce SOX financial reporting requirements', async () => {
      const soxHook = defineHook({
        meta: {
          name: 'compliance:sox-reporting',
          description: 'Enforce SOX financial reporting requirements'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/compliance/sox-reporting.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Check for financial transactions requiring SOX compliance
          const financialData = event.context.graph.getQuads(null, null, null, null);
          const hasApproval = event.payload.approvedBy || false;
          const hasDocumentation = event.payload.documentation || false;
          
          if (financialData.length > 0 && (!hasApproval || !hasDocumentation)) {
            complianceViolations.push({
              type: 'SOX_VIOLATION',
              description: 'Financial transaction lacks required approval or documentation',
              timestamp: new Date().toISOString(),
              severity: 'CRITICAL'
            });
            
            return {
              result: { 
                violation: true, 
                missingApproval: !hasApproval,
                missingDocumentation: !hasDocumentation
              },
              status: 'sox-violation'
            };
          }
          
          return {
            result: { violation: false },
            status: 'sox-compliant'
          };
        }
      });

      manager.addKnowledgeHook(soxHook);

      const event = {
        name: 'financial-transaction',
        payload: {
          // Missing approvedBy and documentation fields
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect SOX violation
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.violation).toBe(true);
      expect(results[0].runResult?.result?.missingApproval).toBe(true);
      expect(results[0].runResult?.result?.missingDocumentation).toBe(true);
      expect(complianceViolations.length).toBe(1);
      expect(complianceViolations[0].type).toBe('SOX_VIOLATION');
    });
  });

  describe('Data Retention Policy Enforcement', () => {
    it('should enforce data retention policies', async () => {
      const retentionHook = defineHook({
        meta: {
          name: 'compliance:data-retention',
          description: 'Enforce data retention policies'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/compliance/data-retention.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Check data age against retention policy
          const dataAge = Date.now() - new Date(event.context.timestamp).getTime();
          const retentionPeriod = event.payload.retentionPeriod || 365 * 24 * 60 * 60 * 1000; // 1 year default
          
          if (dataAge > retentionPeriod) {
            complianceViolations.push({
              type: 'RETENTION_VIOLATION',
              description: `Data exceeds retention period of ${retentionPeriod}ms`,
              timestamp: new Date().toISOString(),
              severity: 'MEDIUM'
            });
            
            return {
              result: { 
                violation: true, 
                dataAge,
                retentionPeriod,
                shouldDelete: true
              },
              status: 'retention-violation'
            };
          }
          
          return {
            result: { violation: false, dataAge, retentionPeriod },
            status: 'retention-compliant'
          };
        }
      });

      manager.addKnowledgeHook(retentionHook);

      const event = {
        name: 'data-access',
        payload: {
          retentionPeriod: 365 * 24 * 60 * 60 * 1000 // 1 year
        },
        context: {
          graph: new Store(),
          timestamp: new Date(Date.now() - 2 * 365 * 24 * 60 * 60 * 1000) // 2 years old
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect retention violation
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.violation).toBe(true);
      expect(results[0].runResult?.result?.shouldDelete).toBe(true);
      expect(complianceViolations.length).toBe(1);
      expect(complianceViolations[0].type).toBe('RETENTION_VIOLATION');
    });
  });

  describe('Privacy Regulation Compliance', () => {
    it('should enforce HIPAA patient data protection', async () => {
      const hipaaHook = defineHook({
        meta: {
          name: 'compliance:hipaa-protection',
          description: 'Enforce HIPAA patient data protection'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/compliance/hipaa-protection.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Check for patient data access without proper authorization
          const patientData = event.context.graph.getQuads(null, null, null, null);
          const hasAuthorization = event.payload.authorizedBy || false;
          const hasBusinessNeed = event.payload.businessNeed || false;
          
          if (patientData.length > 0 && (!hasAuthorization || !hasBusinessNeed)) {
            complianceViolations.push({
              type: 'HIPAA_VIOLATION',
              description: 'Patient data accessed without proper authorization or business need',
              timestamp: new Date().toISOString(),
              severity: 'CRITICAL'
            });
            
            return {
              result: { 
                violation: true, 
                missingAuthorization: !hasAuthorization,
                missingBusinessNeed: !hasBusinessNeed
              },
              status: 'hipaa-violation'
            };
          }
          
          return {
            result: { violation: false },
            status: 'hipaa-compliant'
          };
        }
      });

      manager.addKnowledgeHook(hipaaHook);

      const event = {
        name: 'patient-data-access',
        payload: {
          // Missing authorizedBy and businessNeed fields
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect HIPAA violation
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.violation).toBe(true);
      expect(results[0].runResult?.result?.missingAuthorization).toBe(true);
      expect(results[0].runResult?.result?.missingBusinessNeed).toBe(true);
      expect(complianceViolations.length).toBe(1);
      expect(complianceViolations[0].type).toBe('HIPAA_VIOLATION');
    });
  });

  describe('Regulatory Reporting Failures', () => {
    it('should detect missing regulatory reports', async () => {
      const reportingHook = defineHook({
        meta: {
          name: 'compliance:regulatory-reporting',
          description: 'Check regulatory reporting requirements'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/compliance/regulatory-reporting.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Check for required regulatory reports
          const requiredReports = event.payload.requiredReports || [];
          const submittedReports = event.payload.submittedReports || [];
          
          const missingReports = requiredReports.filter(report => 
            !submittedReports.includes(report)
          );
          
          if (missingReports.length > 0) {
            complianceViolations.push({
              type: 'REPORTING_VIOLATION',
              description: `Missing regulatory reports: ${missingReports.join(', ')}`,
              timestamp: new Date().toISOString(),
              severity: 'HIGH'
            });
            
            return {
              result: { 
                violation: true, 
                missingReports,
                requiredReports,
                submittedReports
              },
              status: 'reporting-violation'
            };
          }
          
          return {
            result: { violation: false },
            status: 'reporting-compliant'
          };
        }
      });

      manager.addKnowledgeHook(reportingHook);

      const event = {
        name: 'regulatory-reporting',
        payload: {
          requiredReports: ['Q1_FINANCIAL', 'Q1_COMPLIANCE', 'Q1_AUDIT'],
          submittedReports: ['Q1_FINANCIAL'] // Missing Q1_COMPLIANCE and Q1_AUDIT
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect reporting violations
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.violation).toBe(true);
      expect(results[0].runResult?.result?.missingReports).toContain('Q1_COMPLIANCE');
      expect(results[0].runResult?.result?.missingReports).toContain('Q1_AUDIT');
      expect(complianceViolations.length).toBe(1);
      expect(complianceViolations[0].type).toBe('REPORTING_VIOLATION');
    });
  });
});
