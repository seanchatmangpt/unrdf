/**
 * @file Configuration audit trail and visibility
 * @module cli/utils/config-audit
 *
 * Tracks which configuration sources are active
 * FM-CLI-005: Add visibility to config cascading
 */

/**
 * Config source audit trail
 */
class ConfigAudit {
  constructor() {
    this.sources = [];
    this.overrides = [];
    this.effectiveConfig = {};
  }

  /**
   * Log config source load attempt
   */
  logSourceLoad(source, status, data = null, error = null) {
    const entry = {
      timestamp: new Date().toISOString(),
      source,
      status, // 'loaded', 'skipped', 'failed'
      data,
      error: error?.message
    };

    this.sources.push(entry);

    if (status === 'loaded' && data) {
      this.overrides.push({
        ...entry,
        precedence: this.sources.length
      });
    }
  }

  /**
   * Show config sources (debugging)
   */
  getSources() {
    return this.sources;
  }

  /**
   * Show config precedence
   */
  getPrecedence() {
    return this.overrides
      .sort((a, b) => b.precedence - a.precedence)
      .map(o => ({
        source: o.source,
        status: o.status,
        precedence: o.precedence
      }));
  }

  /**
   * Format for user display
   */
  toString(verbose = false) {
    const lines = [];

    lines.push('');
    lines.push('⚙️  Configuration Sources:');

    this.sources.forEach((source, idx) => {
      const icon =
        source.status === 'loaded' ? '✅' :
        source.status === 'skipped' ? '⏭️' :
        '❌';

      lines.push(`   ${idx + 1}. ${icon} ${source.source} (${source.status})`);

      if (verbose && source.error) {
        lines.push(`      Error: ${source.error}`);
      }
    });

    lines.push('');
    lines.push('📋 Precedence (highest to lowest):');

    this.getPrecedence().forEach((p, idx) => {
      lines.push(`   ${idx + 1}. ${p.source} (precedence: ${p.precedence})`);
    });

    lines.push('');

    return lines.join('\n');
  }
}

/**
 * Create and populate config audit trail
 */
export async function auditConfigLoad() {
  const audit = new ConfigAudit();

  // Load system config
  try {
    // TODO: Load from /etc/unrdf/config.json
    audit.logSourceLoad('system', 'skipped');
  } catch (error) {
    audit.logSourceLoad('system', 'failed', null, error);
  }

  // Load home config
  try {
    // TODO: Load from ~/.unrdf/config.json
    audit.logSourceLoad('home', 'skipped');
  } catch (error) {
    audit.logSourceLoad('home', 'failed', null, error);
  }

  // Load local config
  try {
    // TODO: Load from ./unrdf.config.json
    audit.logSourceLoad('local', 'skipped');
  } catch (error) {
    audit.logSourceLoad('local', 'failed', null, error);
  }

  // Load environment config
  try {
    // TODO: Load from UNRDF_* environment variables
    audit.logSourceLoad('environment', 'skipped');
  } catch (error) {
    audit.logSourceLoad('environment', 'failed', null, error);
  }

  return audit;
}

/**
 * Warn about config overrides
 */
export function warnConfigOverrides(audit) {
  const precedence = audit.getPrecedence();

  if (precedence.length > 1) {
    console.warn(
      `⚠️  Configuration from ${precedence[0].source} is overriding earlier sources`
    );
  }
}

export { ConfigAudit, auditConfigLoad };

export default {
  ConfigAudit,
  auditConfigLoad,
  warnConfigOverrides
};
