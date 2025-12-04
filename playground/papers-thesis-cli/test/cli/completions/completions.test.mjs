/**
 * @fileoverview Shell Completions Tests
 *
 * @description
 * Tests for shell completion generators (bash, zsh, fish, powershell).
 *
 * @module test/cli/completions
 * @version 1.0.0
 */

import { describe, it, expect } from 'vitest';
import {
  generateCompletions,
  generateAllCompletions,
  getInstallInstructions,
  detectShell,
  getCompletionFilename,
  SUPPORTED_SHELLS,
  DEFAULT_FILENAMES,
} from '../../../src/cli/completions/index.mjs';

import {
  generateBashCompletions,
  getBashInstallInstructions,
} from '../../../src/cli/completions/bash.mjs';
import {
  generateZshCompletions,
  getZshInstallInstructions,
} from '../../../src/cli/completions/zsh.mjs';
import {
  generateFishCompletions,
  getFishInstallInstructions,
} from '../../../src/cli/completions/fish.mjs';
import {
  generatePowerShellCompletions,
  getPowerShellInstallInstructions,
} from '../../../src/cli/completions/powershell.mjs';

describe('Shell Completions Index', () => {
  describe('SUPPORTED_SHELLS', () => {
    it('should include all expected shells', () => {
      expect(SUPPORTED_SHELLS).toContain('bash');
      expect(SUPPORTED_SHELLS).toContain('zsh');
      expect(SUPPORTED_SHELLS).toContain('fish');
      expect(SUPPORTED_SHELLS).toContain('powershell');
    });
  });

  describe('generateCompletions', () => {
    it('should generate completions for each supported shell', () => {
      for (const shell of SUPPORTED_SHELLS) {
        const script = generateCompletions(shell);
        expect(script).toBeDefined();
        expect(typeof script).toBe('string');
        expect(script.length).toBeGreaterThan(100);
      }
    });

    it('should throw for unsupported shell', () => {
      expect(() => generateCompletions('unsupported')).toThrow('Unsupported shell');
    });

    it('should accept custom CLI name', () => {
      const script = generateCompletions('bash', { cliName: 'myapp' });
      expect(script).toContain('myapp');
    });
  });

  describe('generateAllCompletions', () => {
    it('should generate completions for all shells', () => {
      const all = generateAllCompletions();

      expect(Object.keys(all)).toEqual(SUPPORTED_SHELLS);

      for (const shell of SUPPORTED_SHELLS) {
        expect(all[shell]).toBeDefined();
        expect(typeof all[shell]).toBe('string');
      }
    });
  });

  describe('getInstallInstructions', () => {
    it('should return instructions for each shell', () => {
      for (const shell of SUPPORTED_SHELLS) {
        const instructions = getInstallInstructions(shell);
        expect(instructions).toBeDefined();
        expect(instructions).toContain('Installation');
      }
    });

    it('should throw for unsupported shell', () => {
      expect(() => getInstallInstructions('unsupported')).toThrow();
    });
  });

  describe('getCompletionFilename', () => {
    it('should return correct filename for each shell', () => {
      expect(getCompletionFilename('bash')).toBe('playground-completion.bash');
      expect(getCompletionFilename('zsh')).toBe('_playground');
      expect(getCompletionFilename('fish')).toBe('playground.fish');
      expect(getCompletionFilename('powershell')).toBe('playground-completion.ps1');
    });

    it('should use custom CLI name', () => {
      expect(getCompletionFilename('bash', 'myapp')).toBe('myapp-completion.bash');
      expect(getCompletionFilename('zsh', 'myapp')).toBe('_myapp');
    });
  });

  describe('detectShell', () => {
    it('should return a string or null', () => {
      const shell = detectShell();
      if (shell !== null) {
        expect(SUPPORTED_SHELLS).toContain(shell);
      }
    });
  });
});

describe('Bash Completions', () => {
  describe('generateBashCompletions', () => {
    it('should generate valid bash completion script', () => {
      const script = generateBashCompletions();

      // Check shebang and header
      expect(script).toContain('#!/bin/bash');
      expect(script).toContain('Bash completion');

      // Check completion function
      expect(script).toContain('_playground_completions()');
      expect(script).toContain('COMPREPLY');
      expect(script).toContain('compgen');

      // Check command completions
      expect(script).toContain('papers');
      expect(script).toContain('thesis');
      expect(script).toContain('config');
      expect(script).toContain('meta');

      // Check subcommand completions
      expect(script).toContain('generate');
      expect(script).toContain('list');
      expect(script).toContain('validate');

      // Check argument completions
      expect(script).toContain('--format');
      expect(script).toContain('--output');
      expect(script).toContain('imrad');
      expect(script).toContain('dsr');

      // Check complete registration
      expect(script).toContain('complete -F');
    });

    it('should use custom CLI name', () => {
      const script = generateBashCompletions({ cliName: 'myapp' });
      expect(script).toContain('_myapp_completions');
      expect(script).toContain('complete -F _myapp_completions myapp');
    });
  });

  describe('getBashInstallInstructions', () => {
    it('should contain installation steps', () => {
      const instructions = getBashInstallInstructions();

      expect(instructions).toContain('Installation');
      expect(instructions).toContain('bash_completion');
      expect(instructions).toContain('source');
      expect(instructions).toContain('bashrc');
    });
  });
});

describe('Zsh Completions', () => {
  describe('generateZshCompletions', () => {
    it('should generate valid zsh completion script', () => {
      const script = generateZshCompletions();

      // Check compdef header
      expect(script).toContain('#compdef playground');

      // Check main function
      expect(script).toContain('_playground()');

      // Check _arguments usage
      expect(script).toContain('_arguments');

      // Check command descriptions
      expect(script).toContain("'papers:Manage research papers'");
      expect(script).toContain("'thesis:Manage thesis documents'");

      // Check subcommand functions
      expect(script).toContain('_playground_papers()');
      expect(script).toContain('_playground_thesis()');

      // Check argument completions with descriptions
      expect(script).toContain('--format');
      expect(script).toContain('_files');
    });

    it('should include global options', () => {
      const script = generateZshCompletions();

      expect(script).toContain('--help');
      expect(script).toContain('--verbose');
      expect(script).toContain('--quiet');
      expect(script).toContain('--config');
    });
  });

  describe('getZshInstallInstructions', () => {
    it('should contain installation steps', () => {
      const instructions = getZshInstallInstructions();

      expect(instructions).toContain('Installation');
      expect(instructions).toContain('site-functions');
      expect(instructions).toContain('compinit');
      expect(instructions).toContain('fpath');
    });
  });
});

describe('Fish Completions', () => {
  describe('generateFishCompletions', () => {
    it('should generate valid fish completion script', () => {
      const script = generateFishCompletions();

      // Check header comment
      expect(script).toContain('# Fish completion');

      // Check helper functions
      expect(script).toContain('function __fish_playground_using_command');
      expect(script).toContain('function __fish_playground_needs_command');

      // Check complete commands
      expect(script).toContain('complete -c playground');

      // Check command completions with conditions
      expect(script).toContain('-a "papers"');
      expect(script).toContain('-a "thesis"');
      expect(script).toContain('-d "');

      // Check subcommand completions
      expect(script).toContain('-a "generate"');
      expect(script).toContain('-a "list"');

      // Check global options
      expect(script).toContain('-l help');
      expect(script).toContain('-l verbose');
    });

    it('should include argument choices', () => {
      const script = generateFishCompletions();

      expect(script).toContain('imrad');
      expect(script).toContain('dsr');
      expect(script).toContain('masters');
      expect(script).toContain('phd');
    });
  });

  describe('getFishInstallInstructions', () => {
    it('should contain installation steps', () => {
      const instructions = getFishInstallInstructions();

      expect(instructions).toContain('Installation');
      expect(instructions).toContain('completions');
      expect(instructions).toContain('.config/fish');
    });
  });
});

describe('PowerShell Completions', () => {
  describe('generatePowerShellCompletions', () => {
    it('should generate valid PowerShell completion script', () => {
      const script = generatePowerShellCompletions();

      // Check header comment
      expect(script).toContain('# PowerShell completion');

      // Check command data structure
      expect(script).toContain('$playgroundCommands');
      expect(script).toContain("'papers'");
      expect(script).toContain("'thesis'");

      // Check global options
      expect(script).toContain('$playgroundGlobalOptions');
      expect(script).toContain('--help');
      expect(script).toContain('--format');

      // Check argument completer registration
      expect(script).toContain('Register-ArgumentCompleter');
      expect(script).toContain('-CommandName playground');
      expect(script).toContain('-ScriptBlock');

      // Check completion result creation
      expect(script).toContain('CompletionResult');
    });

    it('should include subcommand definitions', () => {
      const script = generatePowerShellCompletions();

      expect(script).toContain("'generate'");
      expect(script).toContain("'list'");
      expect(script).toContain("'validate'");
      expect(script).toContain("'schedule'");
    });

    it('should include argument choices', () => {
      const script = generatePowerShellCompletions();

      expect(script).toContain("'imrad'");
      expect(script).toContain("'dsr'");
      expect(script).toContain("'masters'");
      expect(script).toContain("'phd'");
    });
  });

  describe('getPowerShellInstallInstructions', () => {
    it('should contain installation steps', () => {
      const instructions = getPowerShellInstallInstructions();

      expect(instructions).toContain('Installation');
      expect(instructions).toContain('$PROFILE');
      expect(instructions).toContain('ExecutionPolicy');
    });
  });
});

describe('Completions Content Validation', () => {
  describe('Command Structure', () => {
    it('should include all CLI commands in all shells', () => {
      const commands = ['papers', 'thesis', 'config', 'meta'];

      for (const shell of SUPPORTED_SHELLS) {
        const script = generateCompletions(shell);

        for (const cmd of commands) {
          expect(script).toContain(cmd);
        }
      }
    });

    it('should include all subcommands', () => {
      const subcommands = {
        papers: ['generate', 'list', 'validate'],
        thesis: ['generate', 'list', 'schedule'],
      };

      for (const shell of SUPPORTED_SHELLS) {
        const script = generateCompletions(shell);

        for (const subs of Object.values(subcommands)) {
          for (const sub of subs) {
            expect(script).toContain(sub);
          }
        }
      }
    });
  });

  describe('Argument Completions', () => {
    it('should include common argument options', () => {
      const args = ['--format', '--output', '--help', '--verbose'];

      for (const shell of SUPPORTED_SHELLS) {
        const script = generateCompletions(shell);

        for (const arg of args) {
          expect(script).toContain(arg.replace('--', ''));
        }
      }
    });

    it('should include format choices', () => {
      const formats = ['json', 'yaml', 'table'];

      for (const shell of SUPPORTED_SHELLS) {
        const script = generateCompletions(shell);

        for (const format of formats) {
          expect(script).toContain(format);
        }
      }
    });

    it('should include paper family choices', () => {
      const families = ['imrad', 'dsr', 'argument', 'contribution'];

      for (const shell of SUPPORTED_SHELLS) {
        const script = generateCompletions(shell);

        for (const family of families) {
          expect(script).toContain(family);
        }
      }
    });
  });
});
