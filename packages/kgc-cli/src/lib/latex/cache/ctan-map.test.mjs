/**
 * @file CTAN Mapping Tests
 * @module kgc-cli/lib/latex/cache/ctan-map.test
 */

import { describe, it, expect } from 'vitest';
import {
  getExtension,
  extractPackageName,
  buildVfsPath,
  buildCtanUrls,
  isLocalFixture,
  getPackageMetadata,
  CTAN_PATH_TEMPLATES,
  VFS_PATH_TEMPLATES,
  PACKAGE_NAME_EXCEPTIONS,
  DEFAULT_CTAN_MIRROR,
} from './ctan-map.mjs';

describe('CTAN Mapping', () => {
  describe('getExtension', () => {
    it('should extract .sty extension', () => {
      expect(getExtension('algorithm2e.sty')).toBe('.sty');
    });

    it('should extract .cls extension', () => {
      expect(getExtension('beamer.cls')).toBe('.cls');
    });

    it('should extract .bib extension', () => {
      expect(getExtension('references.bib')).toBe('.bib');
    });

    it('should return empty string for unknown extension', () => {
      expect(getExtension('file.txt')).toBe('');
    });
  });

  describe('extractPackageName', () => {
    it('should strip extension for simple package', () => {
      expect(extractPackageName('algorithm2e.sty')).toBe('algorithm2e');
    });

    it('should use exception mapping for tikz', () => {
      expect(extractPackageName('tikz.sty')).toBe('pgf');
    });

    it('should use exception mapping for algpseudocode', () => {
      expect(extractPackageName('algpseudocode.sty')).toBe('algorithmicx');
    });

    it('should use exception mapping for graphicx', () => {
      expect(extractPackageName('graphicx.sty')).toBe('graphics');
    });

    it('should strip .cls extension', () => {
      expect(extractPackageName('beamer.cls')).toBe('beamer');
    });

    it('should handle unknown files', () => {
      expect(extractPackageName('custom-package.sty')).toBe('custom-package');
    });
  });

  describe('buildVfsPath', () => {
    it('should build correct path for .sty file', () => {
      expect(buildVfsPath('algorithm2e.sty')).toBe(
        'texmf/tex/latex/algorithm2e/algorithm2e.sty'
      );
    });

    it('should build correct path for .cls file', () => {
      expect(buildVfsPath('beamer.cls')).toBe(
        'texmf/tex/latex/beamer/beamer.cls'
      );
    });

    it('should build correct path for .bib file', () => {
      expect(buildVfsPath('references.bib')).toBe(
        'texmf/bibtex/bib/references/references.bib'
      );
    });

    it('should build correct path for .bst file', () => {
      expect(buildVfsPath('plain.bst')).toBe(
        'texmf/bibtex/bst/plain/plain.bst'
      );
    });

    it('should use package exception for tikz', () => {
      expect(buildVfsPath('tikz.sty')).toBe(
        'texmf/tex/latex/pgf/tikz.sty'
      );
    });

    it('should fallback to work/ for unknown extension', () => {
      expect(buildVfsPath('unknown.txt')).toBe('work/unknown.txt');
    });
  });

  describe('buildCtanUrls', () => {
    it('should build URLs for .sty file', () => {
      const urls = buildCtanUrls('algorithm2e.sty');

      expect(urls).toContain(
        'https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty'
      );
      expect(urls).toContain(
        'https://mirrors.ctan.org/macros/latex/required/algorithm2e/algorithm2e.sty'
      );
      expect(urls).toContain(
        'https://mirrors.ctan.org/macros/latex/base/algorithm2e.sty'
      );
    });

    it('should build URLs for .cls file', () => {
      const urls = buildCtanUrls('beamer.cls');

      expect(urls).toContain(
        'https://mirrors.ctan.org/macros/latex/contrib/beamer/beamer.cls'
      );
      expect(urls).toContain(
        'https://mirrors.ctan.org/macros/latex/base/beamer.cls'
      );
    });

    it('should use custom mirror', () => {
      const urls = buildCtanUrls('test.sty', 'http://localhost:3000');

      expect(urls[0]).toMatch(/^http:\/\/localhost:3000/);
    });

    it('should use package exception for tikz', () => {
      const urls = buildCtanUrls('tikz.sty');

      // tikz.sty -> pgf package
      expect(urls[0]).toMatch(/pgf\/tikz\.sty$/);
    });

    it('should throw on invalid mirror URL', () => {
      expect(() => buildCtanUrls('test.sty', 'not-a-url')).toThrow();
    });
  });

  describe('isLocalFixture', () => {
    it('should detect file:// URLs', () => {
      expect(isLocalFixture('file:///tmp/test.sty')).toBe(true);
    });

    it('should detect localhost URLs', () => {
      expect(isLocalFixture('http://localhost:3000/test.sty')).toBe(true);
    });

    it('should reject CTAN URLs', () => {
      expect(isLocalFixture('https://mirrors.ctan.org/test.sty')).toBe(false);
    });

    it('should reject other remote URLs', () => {
      expect(isLocalFixture('https://example.com/test.sty')).toBe(false);
    });
  });

  describe('getPackageMetadata', () => {
    it('should return complete metadata for .sty file', () => {
      const metadata = getPackageMetadata('algorithm2e.sty');

      expect(metadata).toEqual({
        filename: 'algorithm2e.sty',
        package: 'algorithm2e',
        extension: '.sty',
        vfsPath: 'texmf/tex/latex/algorithm2e/algorithm2e.sty',
      });
    });

    it('should use exception mapping in metadata', () => {
      const metadata = getPackageMetadata('tikz.sty');

      expect(metadata.package).toBe('pgf');
      expect(metadata.vfsPath).toBe('texmf/tex/latex/pgf/tikz.sty');
    });

    it('should handle .cls files', () => {
      const metadata = getPackageMetadata('beamer.cls');

      expect(metadata.extension).toBe('.cls');
      expect(metadata.vfsPath).toMatch(/^texmf\/tex\/latex/);
    });
  });

  describe('Constants', () => {
    it('should export CTAN_PATH_TEMPLATES', () => {
      expect(CTAN_PATH_TEMPLATES).toBeDefined();
      expect(CTAN_PATH_TEMPLATES['.sty']).toBeInstanceOf(Array);
      expect(CTAN_PATH_TEMPLATES['.cls']).toBeInstanceOf(Array);
    });

    it('should export VFS_PATH_TEMPLATES', () => {
      expect(VFS_PATH_TEMPLATES).toBeDefined();
      expect(VFS_PATH_TEMPLATES['.sty']).toMatch(/texmf/);
      expect(VFS_PATH_TEMPLATES['.cls']).toMatch(/texmf/);
    });

    it('should export PACKAGE_NAME_EXCEPTIONS', () => {
      expect(PACKAGE_NAME_EXCEPTIONS).toBeDefined();
      expect(PACKAGE_NAME_EXCEPTIONS['tikz.sty']).toBe('pgf');
      expect(PACKAGE_NAME_EXCEPTIONS['algpseudocode.sty']).toBe('algorithmicx');
    });

    it('should export DEFAULT_CTAN_MIRROR', () => {
      expect(DEFAULT_CTAN_MIRROR).toBe('https://mirrors.ctan.org');
    });
  });
});

export default {};
