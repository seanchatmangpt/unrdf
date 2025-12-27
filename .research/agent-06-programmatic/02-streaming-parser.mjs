#!/usr/bin/env node
/**
 * Claude Code Stream-JSON Parser
 *
 * Demonstrates real-time parsing of stream-json output format
 * for incremental processing of Claude responses
 */

import { spawn } from 'child_process';
import readline from 'readline';

/**
 * Parse stream-json output in real-time
 * @param {string} prompt - The prompt to send
 * @param {object} callbacks - Event callbacks
 */
function streamClaude(prompt, callbacks = {}) {
  const {
    onStart = () => {},
    onDelta = () => {},
    onToolUse = () => {},
    onComplete = () => {},
    onError = () => {}
  } = callbacks;

  const proc = spawn('claude', [
    '-p', prompt,
    '--output-format', 'stream-json',
    '--include-partial-messages'
  ]);

  const rl = readline.createInterface({
    input: proc.stdout,
    crlfDelay: Infinity
  });

  let buffer = '';
  let messageId = null;

  rl.on('line', (line) => {
    if (!line.trim()) return;

    try {
      const event = JSON.parse(line);

      switch (event.type) {
        case 'message_start':
          messageId = event.message?.id;
          onStart(event);
          break;

        case 'content_block_start':
          // New content block beginning
          break;

        case 'content_block_delta':
          if (event.delta?.type === 'text_delta') {
            buffer += event.delta.text;
            onDelta(event.delta.text, buffer);
          }
          break;

        case 'content_block_stop':
          // Content block completed
          break;

        case 'message_delta':
          // Message metadata update
          break;

        case 'message_stop':
          onComplete({ messageId, content: buffer });
          break;

        default:
          // Unknown event type
          if (event.type?.includes('tool')) {
            onToolUse(event);
          }
      }
    } catch (e) {
      console.error('Failed to parse line:', line);
      onError(e);
    }
  });

  proc.stderr.on('data', (data) => {
    console.error('stderr:', data.toString());
  });

  proc.on('close', (code) => {
    if (code !== 0) {
      onError(new Error(`Process exited with code ${code}`));
    }
  });

  return proc;
}

/**
 * Example usage with progress tracking
 */
async function demonstrateStreaming() {
  console.log('=== Stream-JSON Demonstration ===\n');
  console.log('Sending prompt: "Explain what recursion is in 2 sentences."\n');

  let charCount = 0;
  let chunkCount = 0;

  streamClaude('Explain what recursion is in 2 sentences.', {
    onStart: (event) => {
      console.log(`📡 Stream started (ID: ${event.message?.id})\n`);
    },

    onDelta: (text, fullText) => {
      process.stdout.write(text);
      charCount += text.length;
      chunkCount++;
    },

    onComplete: ({ messageId, content }) => {
      console.log(`\n\n✅ Stream complete`);
      console.log(`   Message ID: ${messageId}`);
      console.log(`   Total characters: ${charCount}`);
      console.log(`   Chunks received: ${chunkCount}`);
      console.log(`   Content length: ${content.length}`);
    },

    onError: (error) => {
      console.error('\n❌ Error:', error.message);
    }
  });
}

// Main
demonstrateStreaming();
