/**
 * @fileoverview useLambdaScheduling - React hook for Λ-ordering chapter planning
 * Creates total order across all thesis shards with scheduling, critical path, deadlines
 */

import { useState, useCallback, useEffect, useMemo } from 'react';
import {
  computeLambdaOrder,
  DeltaFamilies,
  CanonicalLambdaOrder,
  validateDeltaShard
} from './htf-core.mjs';

/**
 * useLambdaScheduling hook - Plan chapter/shard schedule with Λ-ordering
 *
 * @param {DeltaShard[]} initialShards - Initial shards to schedule
 * @param {Object} options - Scheduling options
 * @param {Date} options.startDate - Project start date
 * @param {Date} options.deadline - Project deadline
 * @param {number} options.weeksAvailable - Weeks until deadline
 * @param {Function} options.onScheduleChange - Callback when schedule changes
 *
 * @returns {Object} Scheduling state and methods
 * @returns {DeltaShard[]} state.shards - Current shards
 * @returns {LambdaOrder} state.order - Total order (Λ-chain)
 * @returns {ScheduleItem[]} state.schedule - Week-by-week schedule
 * @returns {CriticalPathItem[]} state.criticalPath - Critical path items
 * @returns {number} state.progress - Overall progress (0-1)
 * @returns {Function} addShard - Add new shard
 * @returns {Function} updateShard - Update shard properties
 * @returns {Function} removeShard - Remove shard
 * @returns {Function} reorderShards - Manually adjust order
 * @returns {Function} adjustTimings - Auto-schedule with work-back from deadline
 * @returns {Function} generateScheduleReport - Export as markdown/JSON
 */
export function useLambdaScheduling(initialShards = [], options = {}) {
  const {
    startDate = new Date(),
    deadline = new Date(Date.now() + 26 * 7 * 24 * 60 * 60 * 1000), // 26 weeks
    weeksAvailable = null,
    onScheduleChange = () => {}
  } = options;

  const [shards, setShards] = useState(initialShards);
  const [schedule, setSchedule] = useState([]);
  const [progress, setProgress] = useState({});

  // Calculate available weeks
  const totalWeeks = useMemo(() => {
    if (weeksAvailable) return weeksAvailable;
    const msPerWeek = 7 * 24 * 60 * 60 * 1000;
    return Math.floor((deadline - startDate) / msPerWeek);
  }, [startDate, deadline, weeksAvailable]);

  // Compute Λ-order whenever shards change
  const lambdaOrder = useMemo(() => {
    if (shards.length === 0) return { chain: [], positions: new Map(), criticalPath: [] };
    try {
      return computeLambdaOrder(shards);
    } catch (e) {
      console.error('Failed to compute Lambda order:', e);
      return { chain: [], positions: new Map(), criticalPath: [] };
    }
  }, [shards]);

  // Generate schedule from Λ-order and deadline
  const generatedSchedule = useMemo(() => {
    if (lambdaOrder.chain.length === 0) return [];

    // Critical path determines minimum duration
    const criticalPathLength = lambdaOrder.criticalPath.length || 1;
    const weeksPerShard = Math.max(1, Math.floor(totalWeeks / (shards.length || 1)));

    // Distribute shards across available weeks
    const scheduleItems = [];
    let currentWeek = 0;

    for (const [idx, shardId] of lambdaOrder.chain.entries()) {
      const shard = shards.find(s => s.id === shardId);
      if (!shard) continue;

      // Shards on critical path get more time
      const isCritical = lambdaOrder.criticalPath.includes(shardId);
      const allocatedWeeks = isCritical ? weeksPerShard * 1.5 : weeksPerShard;

      const startWeek = currentWeek;
      const endWeek = currentWeek + allocatedWeeks;
      const dueDate = new Date(startDate.getTime() + endWeek * 7 * 24 * 60 * 60 * 1000);

      scheduleItems.push({
        shardId,
        index: idx + 1,
        family: shard.family,
        label: shard.label,
        startWeek,
        endWeek,
        dueDate,
        weeksDue: Math.ceil(endWeek),
        isCritical,
        dependencies: shard.dependencies || [],
        dependentsReady: checkDependentsReady(shardId, scheduleItems, shards),
        status: 'scheduled' // 'scheduled', 'in_progress', 'completed', 'blocked'
      });

      currentWeek = endWeek;
    }

    return scheduleItems;
  }, [lambdaOrder, shards, totalWeeks, startDate]);

  // Calculate overall progress
  const overallProgress = useMemo(() => {
    if (shards.length === 0) return 0;
    const completed = shards.filter(s => progress[s.id] === 'completed').length;
    return completed / shards.length;
  }, [shards, progress]);

  // ====== SHARD MANAGEMENT ======

  const addShard = useCallback((shard) => {
    const validated = validateDeltaShard(shard);
    setShards(prev => [...prev, validated]);
    onScheduleChange({ action: 'add', shard: validated });
  }, [onScheduleChange]);

  const updateShard = useCallback((shardId, updates) => {
    setShards(prev => prev.map(s =>
      s.id === shardId ? { ...s, ...updates } : s
    ));
    onScheduleChange({ action: 'update', shardId, updates });
  }, [onScheduleChange]);

  const removeShard = useCallback((shardId) => {
    setShards(prev => prev.filter(s => s.id !== shardId));
    const newProgress = { ...progress };
    delete newProgress[shardId];
    setProgress(newProgress);
    onScheduleChange({ action: 'remove', shardId });
  }, [progress, onScheduleChange]);

  // ====== SCHEDULING OPERATIONS ======

  /**
   * Reorder shards manually (adjust Λ-chain)
   */
  const reorderShards = useCallback((newOrder) => {
    // Validate all shardIds exist
    const validOrder = newOrder.filter(id =>
      shards.some(s => s.id === id)
    );

    if (validOrder.length !== newOrder.length) {
      console.warn('Some shard IDs in new order are invalid');
    }

    // Update dependencies to respect new order
    const newShards = shards.map(shard => {
      const oldPosition = lambdaOrder.positions.get(shard.id) || Infinity;
      const newPosition = validOrder.indexOf(shard.id);

      // Dependencies should come before in the chain
      const validDeps = (shard.dependencies || []).filter(depId => {
        const depPosition = validOrder.indexOf(depId);
        return depPosition !== -1 && depPosition < newPosition;
      });

      return {
        ...shard,
        dependencies: validDeps
      };
    });

    setShards(newShards);
    onScheduleChange({ action: 'reorder', newOrder: validOrder });
  }, [shards, lambdaOrder, onScheduleChange]);

  /**
   * Auto-adjust timings: work backward from deadline
   */
  const adjustTimings = useCallback((buffer = 0.15) => {
    // Buffer = fraction of time to reserve for final integration
    const workTime = totalWeeks * (1 - buffer);
    const criticalPath = lambdaOrder.criticalPath;

    if (criticalPath.length === 0) return;

    // Allocate time proportionally to critical path
    const updatedShards = shards.map(shard => {
      const isCritical = criticalPath.includes(shard.id);
      const baseWeight = shard.weight || 0.5;
      const criticalBonus = isCritical ? 1.5 : 1;
      const finalWeight = baseWeight * criticalBonus;

      return { ...shard, weight: Math.min(finalWeight, 1) };
    });

    setShards(updatedShards);
    onScheduleChange({ action: 'adjustTimings', buffer, shards: updatedShards });
  }, [totalWeeks, lambdaOrder, shards, onScheduleChange]);

  // ====== REPORTING ======

  /**
   * Generate scheduling report (markdown or JSON)
   */
  const generateScheduleReport = useCallback((format = 'markdown') => {
    if (format === 'markdown') {
      return generateMarkdownSchedule(generatedSchedule, lambdaOrder, totalWeeks);
    } else if (format === 'json') {
      return JSON.stringify({
        totalWeeks,
        startDate,
        deadline,
        lambdaOrder: lambdaOrder.chain,
        criticalPath: lambdaOrder.criticalPath,
        schedule: generatedSchedule,
        progress: overallProgress
      }, null, 2);
    } else if (format === 'gantt') {
      return generateGanttData(generatedSchedule);
    }
    return '';
  }, [generatedSchedule, lambdaOrder, totalWeeks, startDate, deadline, overallProgress]);

  /**
   * Get critical path analysis
   */
  const criticalPathAnalysis = useMemo(() => {
    return {
      path: lambdaOrder.criticalPath,
      length: lambdaOrder.criticalPath.length,
      duration: Math.ceil(
        (lambdaOrder.criticalPath.length * totalWeeks) / (shards.length || 1)
      ),
      flexibility: totalWeeks - ((lambdaOrder.criticalPath.length * totalWeeks) / (shards.length || 1)),
      items: generatedSchedule.filter(item => item.isCritical)
    };
  }, [lambdaOrder, totalWeeks, shards, generatedSchedule]);

  /**
   * Get upcoming milestones
   */
  const upcomingMilestones = useMemo(() => {
    const now = new Date();
    return generatedSchedule
      .filter(item => item.dueDate > now)
      .sort((a, b) => a.dueDate - b.dueDate)
      .slice(0, 5); // Next 5 milestones
  }, [generatedSchedule]);

  /**
   * Check if shard can start (all dependencies complete)
   */
  const canStartShard = useCallback((shardId) => {
    const shard = shards.find(s => s.id === shardId);
    if (!shard) return false;

    return (shard.dependencies || []).every(
      depId => progress[depId] === 'completed'
    );
  }, [shards, progress]);

  /**
   * Mark shard as complete
   */
  const markComplete = useCallback((shardId) => {
    setProgress(prev => ({ ...prev, [shardId]: 'completed' }));
    onScheduleChange({ action: 'complete', shardId });
  }, [onScheduleChange]);

  return {
    // State
    state: {
      shards,
      lambdaOrder,
      schedule: generatedSchedule,
      progress,
      totalWeeks,
      overallProgress
    },

    // Scheduling info
    scheduling: {
      criticalPath: criticalPathAnalysis,
      upcomingMilestones,
      startDate,
      deadline,
      slack: totalWeeks - criticalPathAnalysis.duration
    },

    // Shard management
    addShard,
    updateShard,
    removeShard,
    reorderShards,

    // Scheduling operations
    adjustTimings,
    canStartShard,
    markComplete,

    // Reporting
    generateScheduleReport,
    getCriticalPath: () => lambdaOrder.criticalPath,
    getScheduleStats: () => ({
      totalShards: shards.length,
      completedShards: shards.filter(s => progress[s.id] === 'completed').length,
      criticalPathLength: lambdaOrder.criticalPath.length,
      availableWeeks: totalWeeks,
      progress: overallProgress
    })
  };
}

// ====== HELPERS ======

function checkDependentsReady(shardId, schedule, shards) {
  const dependents = shards
    .filter(s => (s.dependencies || []).includes(shardId))
    .map(s => s.id);

  return dependents.map(depId => {
    const depShard = shards.find(s => s.id === depId);
    const deps = depShard.dependencies.every(d =>
      schedule.some(item => item.shardId === d)
    );
    return { shardId: depId, ready: deps };
  });
}

function generateMarkdownSchedule(schedule, lambdaOrder, totalWeeks) {
  let md = `# Thesis Schedule (Λ-ordering)\n\n`;
  md += `**Total Duration:** ${totalWeeks} weeks\n\n`;

  md += `## Critical Path\n\n`;
  md += `${lambdaOrder.criticalPath.map(id => `- ${id}`).join('\n')}\n\n`;

  md += `## Schedule by Week\n\n`;

  for (let week = 0; week < totalWeeks; week += 2) {
    md += `### Weeks ${week + 1}-${Math.min(week + 2, totalWeeks)}\n\n`;
    const weekItems = schedule.filter(item =>
      item.startWeek <= week && item.endWeek >= week
    );
    if (weekItems.length > 0) {
      md += weekItems.map(item =>
        `- **${item.label}** (${item.family})${item.isCritical ? ' [CRITICAL]' : ''}`
      ).join('\n') + '\n';
    }
    md += '\n';
  }

  return md;
}

function generateGanttData(schedule) {
  return schedule.map(item => ({
    id: item.shardId,
    name: item.label,
    start: item.startWeek,
    duration: Math.ceil(item.endWeek - item.startWeek),
    progress: 0,
    dependencies: item.dependencies,
    critical: item.isCritical
  }));
}

export default useLambdaScheduling;
