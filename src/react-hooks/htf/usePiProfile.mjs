/**
 * @fileoverview usePiProfile - React hook for Π-merge visualization
 * Shows how specific Δ-research shards from different families fit together
 */

import { useState, useCallback, useMemo } from 'react';
import {
  computePiMerge,
  DeltaFamilies,
  StandardInvariants
} from './htf-core.mjs';

/**
 * usePiProfile hook - Visualize merged shard profile
 *
 * @param {DeltaShard[]} shards - Shards to merge
 * @param {Object} options - Profile options
 * @param {Function} options.onMergeChange - Callback when merge state changes
 * @param {boolean} options.showMergePoints - Show cross-family connections
 *
 * @returns {Object} Profile state and methods
 * @returns {PiMerge} profile.merge - Merged structure
 * @returns {FamilyProfile[]} profile.byFamily - Profiles by family
 * @returns {MergePoint[]} profile.mergePoints - Cross-family connections
 * @returns {number} profile.coherence - Overall coherence (0-1)
 * @returns {Function} updateMergeStrategy - Adjust merge strategy
 * @returns {Function} suggestReweighting - AI-suggest weight adjustments
 * @returns {Function} generateMergeReport - Export profile visualization
 */
export function usePiProfile(shards = [], options = {}) {
  const {
    onMergeChange = () => {},
    showMergePoints = true
  } = options;

  const [weights, setWeights] = useState({});
  const [mergeStrategy, setMergeStrategy] = useState('balanced'); // 'balanced', 'emphasize-gaps', 'emphasize-methods'

  // Compute Π-merge
  const piMerge = useMemo(() => {
    if (shards.length === 0) {
      return { unifiedId: '', components: [], mergePoints: {}, coherence: 0 };
    }

    // Apply custom weights if set
    const weightedShards = shards.map(shard => ({
      ...shard,
      weight: weights[shard.id] ?? shard.weight
    }));

    return computePiMerge(weightedShards);
  }, [shards, weights]);

  // ====== PROFILE ANALYSIS ======

  /**
   * Group shards by family and analyze each family
   */
  const profileByFamily = useMemo(() => {
    const profiles = {};

    for (const family of Object.keys(DeltaFamilies)) {
      const familyShards = shards.filter(s => s.family === family);
      const totalWeight = familyShards.reduce((sum, s) => sum + (weights[s.id] ?? s.weight), 0);
      const coverage = familyShards.length / (DeltaFamilies[family].shards.length || 1);

      profiles[family] = {
        family,
        label: DeltaFamilies[family].name,
        shards: familyShards,
        count: familyShards.length,
        expectedCount: DeltaFamilies[family].shards.length,
        totalWeight,
        avgWeight: totalWeight / Math.max(familyShards.length, 1),
        coverage, // % of expected shards included
        isComplete: familyShards.length === DeltaFamilies[family].shards.length,
        status: familyShards.length === 0 ? 'empty' :
          coverage < 0.5 ? 'sparse' :
          coverage < 1 ? 'partial' : 'complete'
      };
    }

    return profiles;
  }, [shards, weights]);

  /**
   * Compute cross-family merge points (where families interconnect)
   */
  const mergePointsAnalysis = useMemo(() => {
    const points = [];
    const pointSet = new Set();

    // Find dependencies crossing family boundaries
    for (const shard of shards) {
      for (const depId of (shard.dependencies || [])) {
        const depShard = shards.find(s => s.id === depId);
        if (depShard && depShard.family !== shard.family) {
          const key = [depShard.family, shard.family].sort().join('→');
          if (!pointSet.has(key)) {
            pointSet.add(key);
            points.push({
              from: depShard.family,
              to: shard.family,
              connections: [],
              strength: 0
            });
          }

          // Find the matching point and add connection
          const point = points.find(p => p.from === depShard.family && p.to === shard.family);
          if (point) {
            point.connections.push({
              fromShard: depId,
              toShard: shard.id,
              fromLabel: depShard.label,
              toLabel: shard.label
            });
            point.strength = point.connections.length;
          }
        }
      }
    }

    return points.sort((a, b) => b.strength - a.strength);
  }, [shards]);

  /**
   * Compute coherence metrics
   */
  const coherenceMetrics = useMemo(() => {
    // Family balance: how evenly distributed are weights?
    const familyWeights = Object.values(profileByFamily).map(p => p.totalWeight);
    const avgFamilyWeight = familyWeights.reduce((a, b) => a + b, 0) / Math.max(familyWeights.length, 1);
    const familyVariance = familyWeights.reduce((sum, w) => sum + Math.pow(w - avgFamilyWeight, 2), 0) / Math.max(familyWeights.length, 1);
    const familyBalance = 1 - Math.min(Math.sqrt(familyVariance) / (avgFamilyWeight || 1), 1);

    // Coverage: are all families represented?
    const representedFamilies = Object.values(profileByFamily).filter(p => p.count > 0).length;
    const totalFamilies = Object.keys(DeltaFamilies).length;
    const familyCoverage = representedFamilies / totalFamilies;

    // Integration: how many cross-family connections?
    const totalConnections = mergePointsAnalysis.reduce((sum, p) => sum + p.strength, 0);
    const expectedConnections = (totalFamilies * (totalFamilies - 1)) / 2;
    const integrationStrength = Math.min(totalConnections / Math.max(expectedConnections, 1), 1);

    // Completeness: % of expected shards included
    const totalExpectedShards = Object.values(DeltaFamilies).reduce((sum, f) => sum + f.shards.length, 0);
    const actualShards = shards.length;
    const completeness = actualShards / totalExpectedShards;

    return {
      familyBalance,
      familyCoverage,
      integrationStrength,
      completeness,
      overall: (familyBalance * 0.25 + familyCoverage * 0.25 + integrationStrength * 0.25 + completeness * 0.25)
    };
  }, [profileByFamily, mergePointsAnalysis, shards]);

  // ====== MODIFICATIONS ======

  /**
   * Adjust weight of a shard to influence merge
   */
  const setShard Weight = useCallback((shardId, weight) => {
    setWeights(prev => ({
      ...prev,
      [shardId]: Math.max(0, Math.min(1, weight))
    }));
    onMergeChange({ action: 'adjustWeight', shardId, weight });
  }, [onMergeChange]);

  /**
   * Suggest reweighting to improve coherence
   */
  const suggestReweighting = useCallback(() => {
    const suggestions = {};

    // Emphasize under-represented families
    for (const [family, profile] of Object.entries(profileByFamily)) {
      if (profile.status === 'empty') {
        // Add weight to existing shards to compensate
        profile.shards.forEach(shard => {
          suggestions[shard.id] = (weights[shard.id] ?? shard.weight) * 1.5;
        });
      } else if (profile.status === 'sparse') {
        profile.shards.forEach(shard => {
          suggestions[shard.id] = (weights[shard.id] ?? shard.weight) * 1.2;
        });
      }
    }

    return suggestions;
  }, [profileByFamily, weights]);

  /**
   * Auto-reweight based on strategy
   */
  const applyMergeStrategy = useCallback((strategy = mergeStrategy) => {
    let newWeights = {};

    switch (strategy) {
      case 'balanced': {
        // Equal weight per family
        const totalWeight = shards.reduce((sum, s) => sum + (weights[s.id] ?? s.weight), 0);
        const targetPerFamily = totalWeight / Math.max(Object.keys(DeltaFamilies).length, 1);

        for (const family of Object.keys(DeltaFamilies)) {
          const familyShards = shards.filter(s => s.family === family);
          const perShard = targetPerFamily / Math.max(familyShards.length, 1);
          familyShards.forEach(shard => {
            newWeights[shard.id] = perShard;
          });
        }
        break;
      }

      case 'emphasize-gaps': {
        // Emphasize contribution family (gap, design, eval, impact)
        newWeights = { ...weights };
        shards.forEach(shard => {
          if (shard.family === 'contribution') {
            newWeights[shard.id] = (weights[shard.id] ?? shard.weight) * 1.5;
          } else if (shard.family === 'imrad') {
            newWeights[shard.id] = (weights[shard.id] ?? shard.weight) * 0.7;
          }
        });
        break;
      }

      case 'emphasize-methods': {
        // Emphasize method/design shards
        newWeights = { ...weights };
        shards.forEach(shard => {
          if (['monograph', 'dsr'].includes(shard.family)) {
            newWeights[shard.id] = (weights[shard.id] ?? shard.weight) * 1.5;
          } else if (shard.family === 'narrative') {
            newWeights[shard.id] = (weights[shard.id] ?? shard.weight) * 0.7;
          }
        });
        break;
      }

      case 'emphasize-narrative': {
        // Emphasize narrative/insight shards
        newWeights = { ...weights };
        shards.forEach(shard => {
          if (shard.family === 'narrative') {
            newWeights[shard.id] = (weights[shard.id] ?? shard.weight) * 1.5;
          } else if (shard.family === 'papers') {
            newWeights[shard.id] = (weights[shard.id] ?? shard.weight) * 0.7;
          }
        });
        break;
      }
    }

    setWeights(newWeights);
    setMergeStrategy(strategy);
    onMergeChange({ action: 'applyStrategy', strategy, weights: newWeights });
  }, [shards, weights, mergeStrategy, onMergeChange]);

  /**
   * Recommend missing shards to improve coherence
   */
  const recommendMissingShards = useCallback(() => {
    const recommendations = [];

    for (const [family, profile] of Object.entries(profileByFamily)) {
      if (profile.status === 'empty') {
        recommendations.push({
          priority: 'high',
          family,
          reason: `${DeltaFamilies[family].name} family is completely absent`,
          expectedShards: DeltaFamilies[family].shards
        });
      } else if (profile.status === 'sparse') {
        const missing = DeltaFamilies[family].shards.filter(
          shardName => !profile.shards.some(s => s.id === shardName)
        );
        recommendations.push({
          priority: 'medium',
          family,
          reason: `${DeltaFamilies[family].name} is incomplete (${profile.count}/${profile.expectedCount})`,
          missingShards: missing
        });
      }
    }

    return recommendations;
  }, [profileByFamily]);

  // ====== REPORTING ======

  /**
   * Generate profile report (visualization data)
   */
  const generateProfileReport = useCallback((format = 'json') => {
    const report = {
      merge: piMerge,
      coherence: coherenceMetrics,
      profileByFamily,
      mergePoints: mergePointsAnalysis,
      strategies: {
        current: mergeStrategy,
        available: ['balanced', 'emphasize-gaps', 'emphasize-methods', 'emphasize-narrative']
      },
      recommendations: recommendMissingShards()
    };

    if (format === 'json') {
      return JSON.stringify(report, null, 2);
    } else if (format === 'markdown') {
      return generateMarkdownProfile(report);
    } else if (format === 'html') {
      return generateHTMLProfile(report);
    }

    return report;
  }, [piMerge, coherenceMetrics, profileByFamily, mergePointsAnalysis, mergeStrategy, recommendMissingShards]);

  /**
   * Get visualization data for Sankey/chord diagram
   */
  const getMergeVisualizationData = useCallback(() => {
    return {
      families: Object.keys(DeltaFamilies),
      shards: shards.map(s => ({
        id: s.id,
        family: s.family,
        label: s.label,
        weight: weights[s.id] ?? s.weight
      })),
      connections: mergePointsAnalysis.flatMap(point =>
        point.connections.map(conn => ({
          source: conn.fromShard,
          target: conn.toShard,
          value: 1
        }))
      ),
      familyStats: profileByFamily
    };
  }, [shards, weights, mergePointsAnalysis, profileByFamily]);

  return {
    // State
    state: {
      shards,
      weights,
      mergeStrategy,
      piMerge
    },

    // Analysis
    analysis: {
      profileByFamily,
      mergePoints: mergePointsAnalysis,
      coherence: coherenceMetrics,
      recommendations: recommendMissingShards()
    },

    // Modifications
    setShardWeight,
    suggestReweighting,
    applyMergeStrategy,
    recommendMissingShards,

    // Reporting
    generateProfileReport,
    getMergeVisualizationData,

    // Helpers
    getProfileStats: () => ({
      totalShards: shards.length,
      representedFamilies: Object.values(profileByFamily).filter(p => p.count > 0).length,
      totalFamilies: Object.keys(DeltaFamilies).length,
      overallCoherence: coherenceMetrics.overall,
      familyBalance: coherenceMetrics.familyBalance
    })
  };
}

// ====== HELPERS ======

function generateMarkdownProfile(report) {
  let md = `# Π-Profile Analysis\n\n`;

  md += `## Coherence Metrics\n\n`;
  md += `- **Overall:** ${(report.coherence.overall * 100).toFixed(1)}%\n`;
  md += `- **Family Balance:** ${(report.coherence.familyBalance * 100).toFixed(1)}%\n`;
  md += `- **Family Coverage:** ${(report.coherence.familyCoverage * 100).toFixed(1)}%\n`;
  md += `- **Integration Strength:** ${(report.coherence.integrationStrength * 100).toFixed(1)}%\n`;
  md += `- **Completeness:** ${(report.coherence.completeness * 100).toFixed(1)}%\n\n`;

  md += `## Profile by Family\n\n`;
  for (const [family, profile] of Object.entries(report.profileByFamily)) {
    md += `### ${profile.label} (${profile.status})\n`;
    md += `- Shards: ${profile.count}/${profile.expectedCount}\n`;
    md += `- Weight: ${profile.totalWeight.toFixed(2)}\n`;
    md += `- Coverage: ${(profile.coverage * 100).toFixed(1)}%\n\n`;
  }

  md += `## Cross-Family Connections\n\n`;
  for (const point of report.mergePoints) {
    md += `- **${point.from} → ${point.to}:** ${point.strength} connections\n`;
  }
  md += '\n';

  md += `## Recommendations\n\n`;
  for (const rec of report.recommendations) {
    md += `- **[${rec.priority}]** ${rec.reason}\n`;
  }

  return md;
}

function generateHTMLProfile(report) {
  // Generate simple HTML representation
  const html = `
    <div class="pi-profile">
      <h2>Π-Profile Analysis</h2>
      <div class="coherence-metrics">
        <h3>Coherence Metrics</h3>
        <ul>
          <li>Overall: ${(report.coherence.overall * 100).toFixed(1)}%</li>
          <li>Family Balance: ${(report.coherence.familyBalance * 100).toFixed(1)}%</li>
          <li>Family Coverage: ${(report.coherence.familyCoverage * 100).toFixed(1)}%</li>
          <li>Integration: ${(report.coherence.integrationStrength * 100).toFixed(1)}%</li>
        </ul>
      </div>
    </div>
  `;
  return html;
}

export default usePiProfile;
