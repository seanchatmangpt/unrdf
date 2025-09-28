/**
 * @file Business Logic and Domain-Specific Tests
 * 
 * Tests for domain rule validation, business process compliance,
 * regulatory requirement changes, industry standard compliance, and customer-specific requirements.
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

describe('Business Logic and Domain-Specific', () => {
  let manager;
  let tempDir;
  let domainViolations;
  let businessProcessResults;
  let regulatoryCompliance;

  beforeEach(async () => {
    // Create temporary directory for test files
    tempDir = join(process.cwd(), 'test-temp-business-logic');
    await fs.mkdir(tempDir, { recursive: true });
    
    // Initialize tracking arrays
    domainViolations = [];
    businessProcessResults = [];
    regulatoryCompliance = [];
    
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

  describe('Domain Rule Validation', () => {
    it('should validate financial transaction domain rules', async () => {
      const financialRulesHook = defineHook({
        meta: {
          name: 'domain:financial-rules',
          description: 'Validate financial transaction domain rules'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/domain/financial-rules.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Simulate financial transaction validation
          const transaction = event.payload.transaction || {};
          const amount = transaction.amount || 0;
          const currency = transaction.currency || '';
          const accountBalance = transaction.accountBalance || 0;
          
          let violations = [];
          
          // Rule 1: Amount must be positive
          if (amount <= 0) {
            violations.push({
              rule: 'POSITIVE_AMOUNT',
              message: 'Transaction amount must be positive',
              severity: 'ERROR'
            });
          }
          
          // Rule 2: Sufficient balance for withdrawal
          if (transaction.type === 'withdrawal' && amount > accountBalance) {
            violations.push({
              rule: 'SUFFICIENT_BALANCE',
              message: 'Insufficient account balance for withdrawal',
              severity: 'ERROR'
            });
          }
          
          // Rule 3: Currency must be supported
          const supportedCurrencies = ['USD', 'EUR', 'GBP', 'JPY'];
          if (!supportedCurrencies.includes(currency)) {
            violations.push({
              rule: 'SUPPORTED_CURRENCY',
              message: `Currency ${currency} is not supported`,
              severity: 'WARNING'
            });
          }
          
          // Rule 4: Large transactions require approval
          if (amount > 10000) {
            violations.push({
              rule: 'LARGE_TRANSACTION_APPROVAL',
              message: 'Large transactions require manager approval',
              severity: 'INFO'
            });
          }
          
          if (violations.length > 0) {
            domainViolations.push(...violations);
          }
          
          return {
            result: { 
              valid: violations.length === 0,
              violations,
              transactionType: transaction.type,
              amount,
              currency
            },
            status: 'domain-validation-complete'
          };
        }
      });

      manager.addKnowledgeHook(financialRulesHook);

      const event = {
        name: 'financial-transaction',
        payload: {
          transaction: {
            type: 'withdrawal',
            amount: 15000,
            currency: 'USD',
            accountBalance: 5000
          }
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect domain rule violations
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.valid).toBe(false);
      expect(results[0].runResult?.result?.violations.length).toBe(2);
      expect(domainViolations.length).toBe(2);
      expect(domainViolations[0].rule).toBe('SUFFICIENT_BALANCE');
      expect(domainViolations[1].rule).toBe('LARGE_TRANSACTION_APPROVAL');
    });

    it('should validate healthcare patient data domain rules', async () => {
      const healthcareRulesHook = defineHook({
        meta: {
          name: 'domain:healthcare-rules',
          description: 'Validate healthcare patient data domain rules'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/domain/healthcare-rules.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Simulate healthcare data validation
          const patient = event.payload.patient || {};
          const age = patient.age || 0;
          const weight = patient.weight || 0;
          const height = patient.height || 0;
          
          let violations = [];
          
          // Rule 1: Age must be realistic
          if (age < 0 || age > 150) {
            violations.push({
              rule: 'REALISTIC_AGE',
              message: 'Patient age must be between 0 and 150',
              severity: 'ERROR'
            });
          }
          
          // Rule 2: Weight must be positive
          if (weight <= 0) {
            violations.push({
              rule: 'POSITIVE_WEIGHT',
              message: 'Patient weight must be positive',
              severity: 'ERROR'
            });
          }
          
          // Rule 3: Height must be positive
          if (height <= 0) {
            violations.push({
              rule: 'POSITIVE_HEIGHT',
              message: 'Patient height must be positive',
              severity: 'ERROR'
            });
          }
          
          // Rule 4: BMI calculation validation
          if (weight > 0 && height > 0) {
            const bmi = weight / ((height / 100) ** 2);
            if (bmi < 10 || bmi > 100) {
              violations.push({
                rule: 'REALISTIC_BMI',
                message: `BMI ${bmi.toFixed(1)} is outside realistic range (10-100)`,
                severity: 'WARNING'
              });
            }
          }
          
          if (violations.length > 0) {
            domainViolations.push(...violations);
          }
          
          return {
            result: { 
              valid: violations.length === 0,
              violations,
              patientId: patient.id,
              age,
              weight,
              height
            },
            status: 'healthcare-validation-complete'
          };
        }
      });

      manager.addKnowledgeHook(healthcareRulesHook);

      const event = {
        name: 'patient-data',
        payload: {
          patient: {
            id: 'P12345',
            age: 25,
            weight: 70,
            height: 175
          }
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should pass domain validation
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.valid).toBe(true);
      expect(results[0].runResult?.result?.violations.length).toBe(0);
      expect(domainViolations.length).toBe(0);
    });
  });

  describe('Business Process Compliance', () => {
    it('should validate order processing business process', async () => {
      const orderProcessHook = defineHook({
        meta: {
          name: 'process:order-processing',
          description: 'Validate order processing business process'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/process/order-processing.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Simulate order processing validation
          const order = event.payload.order || {};
          const customer = event.payload.customer || {};
          const items = event.payload.items || [];
          
          let processViolations = [];
          
          // Process Step 1: Customer validation
          if (!customer.id || !customer.email) {
            processViolations.push({
              step: 'CUSTOMER_VALIDATION',
              message: 'Customer ID and email are required',
              severity: 'ERROR'
            });
          }
          
          // Process Step 2: Order items validation
          if (items.length === 0) {
            processViolations.push({
              step: 'ORDER_ITEMS',
              message: 'Order must contain at least one item',
              severity: 'ERROR'
            });
          }
          
          // Process Step 3: Inventory check
          const outOfStockItems = items.filter(item => !item.inStock);
          if (outOfStockItems.length > 0) {
            processViolations.push({
              step: 'INVENTORY_CHECK',
              message: `Items out of stock: ${outOfStockItems.map(i => i.name).join(', ')}`,
              severity: 'ERROR'
            });
          }
          
          // Process Step 4: Payment validation
          if (!order.paymentMethod) {
            processViolations.push({
              step: 'PAYMENT_VALIDATION',
              message: 'Payment method is required',
              severity: 'ERROR'
            });
          }
          
          if (processViolations.length > 0) {
            businessProcessResults.push(...processViolations);
          }
          
          return {
            result: { 
              processValid: processViolations.length === 0,
              violations: processViolations,
              orderId: order.id,
              customerId: customer.id,
              itemCount: items.length
            },
            status: 'process-validation-complete'
          };
        }
      });

      manager.addKnowledgeHook(orderProcessHook);

      const event = {
        name: 'order-processing',
        payload: {
          order: {
            id: 'ORD123',
            paymentMethod: 'credit_card'
          },
          customer: {
            id: 'CUST456',
            email: 'customer@example.com'
          },
          items: [
            { name: 'Product A', inStock: true },
            { name: 'Product B', inStock: false }
          ]
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect process violations
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.processValid).toBe(false);
      expect(results[0].runResult?.result?.violations.length).toBe(1);
      expect(businessProcessResults.length).toBe(1);
      expect(businessProcessResults[0].step).toBe('INVENTORY_CHECK');
    });
  });

  describe('Regulatory Requirement Changes', () => {
    it('should detect and handle regulatory requirement changes', async () => {
      const regulatoryHook = defineHook({
        meta: {
          name: 'regulatory:requirement-changes',
          description: 'Detect and handle regulatory requirement changes'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/regulatory/requirement-changes.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Simulate regulatory requirement analysis
          const currentRequirements = event.payload.currentRequirements || [];
          const newRequirements = event.payload.newRequirements || [];
          const effectiveDate = event.payload.effectiveDate || new Date();
          
          let changes = [];
          let complianceIssues = [];
          
          // Check for new requirements
          newRequirements.forEach(req => {
            const existing = currentRequirements.find(cr => cr.id === req.id);
            if (!existing) {
              changes.push({
                type: 'NEW_REQUIREMENT',
                requirement: req,
                action: 'IMPLEMENT',
                priority: 'HIGH'
              });
            }
          });
          
          // Check for modified requirements
          currentRequirements.forEach(current => {
            const updated = newRequirements.find(nr => nr.id === current.id);
            if (updated && updated.version !== current.version) {
              changes.push({
                type: 'UPDATED_REQUIREMENT',
                requirement: updated,
                previousVersion: current.version,
                action: 'UPDATE',
                priority: 'MEDIUM'
              });
            }
          });
          
          // Check for removed requirements
          currentRequirements.forEach(current => {
            const stillExists = newRequirements.find(nr => nr.id === current.id);
            if (!stillExists) {
              changes.push({
                type: 'REMOVED_REQUIREMENT',
                requirement: current,
                action: 'REMOVE',
                priority: 'LOW'
              });
            }
          });
          
          // Check compliance status
          if (changes.length > 0) {
            complianceIssues.push({
              type: 'REGULATORY_CHANGES',
              changes,
              effectiveDate,
              status: 'ACTION_REQUIRED'
            });
          }
          
          if (complianceIssues.length > 0) {
            regulatoryCompliance.push(...complianceIssues);
          }
          
          return {
            result: { 
              changesDetected: changes.length,
              changes,
              complianceStatus: complianceIssues.length > 0 ? 'ACTION_REQUIRED' : 'COMPLIANT',
              effectiveDate
            },
            status: 'regulatory-analysis-complete'
          };
        }
      });

      manager.addKnowledgeHook(regulatoryHook);

      const event = {
        name: 'regulatory-analysis',
        payload: {
          currentRequirements: [
            { id: 'REQ001', version: '1.0', description: 'Data retention policy' },
            { id: 'REQ002', version: '2.0', description: 'Privacy protection' }
          ],
          newRequirements: [
            { id: 'REQ001', version: '1.1', description: 'Data retention policy (updated)' },
            { id: 'REQ003', version: '1.0', description: 'New security requirement' }
          ],
          effectiveDate: new Date('2024-01-01')
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect regulatory changes
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.changesDetected).toBe(3);
      expect(results[0].runResult?.result?.complianceStatus).toBe('ACTION_REQUIRED');
      expect(regulatoryCompliance.length).toBe(1);
      expect(regulatoryCompliance[0].changes.length).toBe(3);
    });
  });

  describe('Industry Standard Compliance', () => {
    it('should validate compliance with industry standards', async () => {
      const industryStandardsHook = defineHook({
        meta: {
          name: 'standards:industry-compliance',
          description: 'Validate compliance with industry standards'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/standards/industry-compliance.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Simulate industry standards compliance check
          const system = event.payload.system || {};
          const standards = event.payload.standards || [];
          
          let complianceResults = [];
          
          standards.forEach(standard => {
            let compliant = true;
            let violations = [];
            
            // ISO 27001 compliance check
            if (standard.name === 'ISO 27001') {
              if (!system.securityPolicy) {
                compliant = false;
                violations.push('Security policy not implemented');
              }
              if (!system.riskAssessment) {
                compliant = false;
                violations.push('Risk assessment not conducted');
              }
              if (!system.incidentResponse) {
                compliant = false;
                violations.push('Incident response plan not established');
              }
            }
            
            // PCI DSS compliance check
            if (standard.name === 'PCI DSS') {
              if (!system.encryption) {
                compliant = false;
                violations.push('Data encryption not implemented');
              }
              if (!system.accessControl) {
                compliant = false;
                violations.push('Access control not implemented');
              }
              if (!system.networkSecurity) {
                compliant = false;
                violations.push('Network security not implemented');
              }
            }
            
            complianceResults.push({
              standard: standard.name,
              version: standard.version,
              compliant,
              violations,
              score: compliant ? 100 : Math.max(0, 100 - (violations.length * 20))
            });
          });
          
          if (complianceResults.length > 0) {
            regulatoryCompliance.push(...complianceResults);
          }
          
          return {
            result: { 
              standardsChecked: standards.length,
              complianceResults,
              overallCompliant: complianceResults.every(r => r.compliant)
            },
            status: 'standards-compliance-complete'
          };
        }
      });

      manager.addKnowledgeHook(industryStandardsHook);

      const event = {
        name: 'standards-compliance',
        payload: {
          system: {
            securityPolicy: true,
            riskAssessment: false,
            incidentResponse: true,
            encryption: true,
            accessControl: false,
            networkSecurity: true
          },
          standards: [
            { name: 'ISO 27001', version: '2013' },
            { name: 'PCI DSS', version: '4.0' }
          ]
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect compliance issues
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.standardsChecked).toBe(2);
      expect(results[0].runResult?.result?.overallCompliant).toBe(false);
      expect(regulatoryCompliance.length).toBe(2);
      expect(regulatoryCompliance[0].compliant).toBe(false);
      expect(regulatoryCompliance[1].compliant).toBe(false);
    });
  });

  describe('Customer-Specific Requirements', () => {
    it('should validate customer-specific business requirements', async () => {
      const customerRequirementsHook = defineHook({
        meta: {
          name: 'customer:specific-requirements',
          description: 'Validate customer-specific business requirements'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/customer/specific-requirements.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Simulate customer-specific requirements validation
          const customer = event.payload.customer || {};
          const service = event.payload.service || {};
          const requirements = event.payload.requirements || [];
          
          let requirementViolations = [];
          
          requirements.forEach(req => {
            let satisfied = false;
            
            // Custom requirement: 24/7 support
            if (req.type === 'SUPPORT_AVAILABILITY') {
              satisfied = service.supportHours === '24/7';
              if (!satisfied) {
                requirementViolations.push({
                  requirement: req.type,
                  description: req.description,
                  expected: '24/7 support',
                  actual: service.supportHours,
                  severity: 'HIGH'
                });
              }
            }
            
            // Custom requirement: SLA response time
            if (req.type === 'SLA_RESPONSE_TIME') {
              satisfied = service.slaResponseTime <= req.threshold;
              if (!satisfied) {
                requirementViolations.push({
                  requirement: req.type,
                  description: req.description,
                  expected: `≤ ${req.threshold}ms`,
                  actual: `${service.slaResponseTime}ms`,
                  severity: 'MEDIUM'
                });
              }
            }
            
            // Custom requirement: Data residency
            if (req.type === 'DATA_RESIDENCY') {
              satisfied = service.dataLocation === req.requiredLocation;
              if (!satisfied) {
                requirementViolations.push({
                  requirement: req.type,
                  description: req.description,
                  expected: req.requiredLocation,
                  actual: service.dataLocation,
                  severity: 'CRITICAL'
                });
              }
            }
          });
          
          if (requirementViolations.length > 0) {
            businessProcessResults.push(...requirementViolations);
          }
          
          return {
            result: { 
              customerId: customer.id,
              requirementsChecked: requirements.length,
              violations: requirementViolations,
              allRequirementsMet: requirementViolations.length === 0
            },
            status: 'customer-requirements-complete'
          };
        }
      });

      manager.addKnowledgeHook(customerRequirementsHook);

      const event = {
        name: 'customer-requirements',
        payload: {
          customer: {
            id: 'CUST789',
            name: 'Enterprise Corp',
            tier: 'premium'
          },
          service: {
            supportHours: 'business hours',
            slaResponseTime: 500,
            dataLocation: 'US'
          },
          requirements: [
            {
              type: 'SUPPORT_AVAILABILITY',
              description: '24/7 customer support required',
              threshold: null
            },
            {
              type: 'SLA_RESPONSE_TIME',
              description: 'Response time must be under 200ms',
              threshold: 200
            },
            {
              type: 'DATA_RESIDENCY',
              description: 'Data must be stored in EU',
              requiredLocation: 'EU'
            }
          ]
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect customer requirement violations
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.requirementsChecked).toBe(3);
      expect(results[0].runResult?.result?.allRequirementsMet).toBe(false);
      expect(results[0].runResult?.result?.violations.length).toBe(3);
      expect(businessProcessResults.length).toBe(3);
      expect(businessProcessResults[0].requirement).toBe('SUPPORT_AVAILABILITY');
      expect(businessProcessResults[1].requirement).toBe('SLA_RESPONSE_TIME');
      expect(businessProcessResults[2].requirement).toBe('DATA_RESIDENCY');
    });
  });
});
