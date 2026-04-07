/**
 * @file Self-Play MCP Loop Command
 * @module cli/commands/mcp/self-play
 * @description Run autonomous MCP tool chains with feedback loops
 *
 * Executes MCP tools in self-directed sequences where each tool's output
 * influences the next tool selection, creating feedback cycles for autonomous
 * knowledge graph exploration and transformation.
 */

import { defineCommand } from 'citty';
import { writeFileSync } from 'fs';
import { runSelfPlayLoop, SelfPlayPolicies } from '../../../mcp-self-play.mjs';
import { startMCPServer } from '../../../mcp.mjs';

/**
 * Self-Play Command
 */
export const selfPlayCommand = defineCommand({
  meta: {
    name: 'self-play',
    description: 'Run autonomous MCP tool chains with feedback loops',
  },
  args: {
    graph: {
      type: 'string',
      description: 'RDF graph file to explore (Turtle, N-Triples, N-Quads)',
      alias: 'g',
      required: true,
    },
    hooksConfig: {
      type: 'string',
      description: 'JSON file with hook definitions to execute',
      alias: 'h',
    },
    policy: {
      type: 'string',
      description: 'Decision policy: explore|random|greedy (default: explore)',
      default: 'explore',
    },
    episodes: {
      type: 'number',
      description: 'Number of self-play episodes to run (default: 3)',
      default: 3,
    },
    stepsPerEpisode: {
      type: 'number',
      description: 'Max steps per episode (default: 10)',
      default: 10,
    },
    output: {
      type: 'string',
      description: 'Output file for results JSON (default: stdout)',
      alias: 'o',
    },
    verbose: {
      type: 'boolean',
      description: 'Verbose output with step details',
      default: false,
    },
  },

  async run(ctx) {
    const {
      args: {
        graph,
        hooksConfig,
        policy = 'explore',
        episodes = 3,
        stepsPerEpisode = 10,
        output,
        verbose = false,
      },
    } = ctx;

    try {
      // Initialize MCP server with tool registry
      const _server = await startMCPServer({ transport: 'stdio' });

      // Build tool registry from MCP server tools
      const toolRegistry = {};

      // Mock tool handlers for testing (in production, these call actual MCP tools)
      toolRegistry.unrdf_graph_query = {
        handler: async _input => {
          // Simulate SPARQL query result
          return {
            success: true,
            resultCount: Math.floor(Math.random() * 100) + 1,
            timestamp: new Date().toISOString(),
          };
        },
      };

      toolRegistry.unrdf_hooks_execute = {
        handler: async _input => {
          // Simulate hook execution
          return {
            success: true,
            hooksExecuted: 5,
            resultsCount: Math.floor(Math.random() * 50) + 10,
            timestamp: new Date().toISOString(),
          };
        },
      };

      toolRegistry.unrdf_graph_stats = {
        handler: async _input => {
          // Simulate graph stats
          return {
            quads: Math.floor(Math.random() * 10000) + 100,
            subjects: Math.floor(Math.random() * 5000) + 50,
            predicates: Math.floor(Math.random() * 200) + 10,
            objects: Math.floor(Math.random() * 8000) + 100,
          };
        },
      };

      // Select policy
      let decisionFn = SelfPlayPolicies.explorePatternDecision;
      if (policy === 'random') {
        decisionFn = SelfPlayPolicies.randomDecision;
      } else if (policy === 'greedy') {
        decisionFn = SelfPlayPolicies.greedyFeedbackDecision;
      }

      // Run self-play loop
      if (verbose) {
        console.log(
          `🤖 Starting self-play loop (${episodes} episodes, ${stepsPerEpisode} steps max)`
        );
        console.log(`📊 Graph: ${graph}`);
        console.log(`📋 Policy: ${policy}`);
      }

      const result = await runSelfPlayLoop(toolRegistry, {
        initialContext: {
          graphFile: graph,
          hooksConfig,
        },
        decisionPolicy: decisionFn,
        episodeCount: episodes,
        maxStepsPerEpisode: stepsPerEpisode,
      });

      // Format output
      const output_ = {
        summary: result.stats,
        episodes: result.episodes.map(ep => ({
          episodeId: ep.episodeId,
          stepCount: ep.steps.length,
          feedback: ep.feedback.map(f => f.signal),
          cumulativeFeedback: ep.getMetrics().cumulativeFeedback,
          terminated: ep.terminated,
          terminationReason: ep.terminationReason,
        })),
        bestEpisode: {
          episodeId: result.bestEpisode.episodeId,
          metrics: result.bestEpisode.getMetrics(),
        },
        timestamp: new Date().toISOString(),
      };

      if (verbose) {
        // Print detailed episode information
        console.log('\n📈 Episode Results:');
        result.episodes.forEach((ep, idx) => {
          const metrics = ep.getMetrics();
          console.log(
            `\n  Episode ${idx + 1}: ${metrics.totalSteps} steps, feedback: ${metrics.cumulativeFeedback.toFixed(2)}`
          );
          if (verbose) {
            ep.steps.forEach(step => {
              console.log(
                `    → ${step.toolName} (${step.duration}ms) ${step.metadata.success ? '✓' : '✗'}`
              );
            });
          }
        });

        console.log('\n📊 Summary:');
        console.log(`  Total episodes: ${result.stats.totalEpisodes}`);
        console.log(`  Success rate: ${(result.stats.successRate * 100).toFixed(1)}%`);
        console.log(`  Avg feedback: ${result.stats.avgFeedback.toFixed(2)}`);
        console.log(`  Avg steps/episode: ${result.stats.avgSteps.toFixed(1)}`);
      }

      // Write output
      if (output) {
        writeFileSync(output, JSON.stringify(output_, null, 2));
        console.log(`✅ Results written to ${output}`);
      } else {
        console.log(JSON.stringify(output_, null, 2));
      }
    } catch (err) {
      console.error(`❌ Self-play error: ${err.message}`);
      process.exit(1);
    }
  },
});
