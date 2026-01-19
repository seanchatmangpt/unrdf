/**
 * @file Uptime Reporting and Metrics Collection
 * @module @unrdf/daemon/uptime
 * @description Comprehensive uptime tracking, SLA compliance reporting,
 * and incident logging for daemon service monitoring.
 */

// Uptime Tracker
export {
  UptimeTracker,
  createUptimeTracker,
  TimePeriodSchema,
  MaintenanceWindowSchema,
  UptimeTrackerConfigSchema,
  AvailabilityResultSchema,
  RollingWindowType,
} from './uptime-tracker.mjs';

// SLA Reporter
export {
  SLAReporter,
  createSLAReporter,
  SLATierSchema,
  SLAConfigSchema,
  SLAComplianceResultSchema,
  SLAReportFormat,
  StandardSLATiers,
} from './sla-reporter.mjs';

// Incident Log
export {
  IncidentLog,
  createIncidentLog,
  IncidentSchema,
  IncidentLogConfigSchema,
  ReliabilityMetricsSchema,
  IncidentSummarySchema,
  IncidentSeverity,
  IncidentStatus,
} from './incident-log.mjs';
