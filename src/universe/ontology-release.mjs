/**
 * @file ontology-release.mjs
 * @description OntologyRelease and AllowedOntology classes for ontology version management
 */

import { z } from 'zod';
import { IriSchema, validateIri, computeContentHash } from './rdf-utils.mjs';

/**
 * Zod schema for OntologyRelease
 */
const OntologyReleaseSchema = z.object({
  namespaceIri: IriSchema,
  version: z.string().min(1),
  distributionUrl: z.string().url().optional(),
  contentHash: z.string().regex(/^[a-f0-9]{64}$/i, 'Must be a valid SHA256 hash'),
  releaseDate: z.date().optional(),
});

/**
 * Zod schema for AllowedOntology
 */
const AllowedOntologySchema = z.object({
  namespaceIri: IriSchema,
  name: z.string().min(1),
  description: z.string().optional(),
  releases: z.array(OntologyReleaseSchema).min(1),
});

/**
 * Represents a specific release/version of an ontology
 *
 * @class OntologyRelease
 *
 * @example
 * ```javascript
 * const release = new OntologyRelease({
 *   namespaceIri: 'http://www.w3.org/ns/prov#',
 *   version: '2013-04-30',
 *   distributionUrl: 'https://www.w3.org/ns/prov-o',
 *   contentHash: 'abc123...',
 * });
 * ```
 */
export class OntologyRelease {
  /**
   * Create an ontology release
   *
   * @param {Object} config - Release configuration
   * @param {string} config.namespaceIri - Namespace IRI (e.g., 'http://www.w3.org/ns/prov#')
   * @param {string} config.version - Version identifier (e.g., '2013-04-30')
   * @param {string} [config.distributionUrl] - URL to download ontology
   * @param {string} config.contentHash - SHA256 hash of ontology content
   * @param {Date} [config.releaseDate] - Release date
   */
  constructor(config) {
    const validated = OntologyReleaseSchema.parse(config);

    /** @type {string} Namespace IRI */
    this.namespaceIri = validated.namespaceIri;

    /** @type {string} Version identifier */
    this.version = validated.version;

    /** @type {string|undefined} Distribution URL */
    this.distributionUrl = validated.distributionUrl;

    /** @type {string} Content hash (SHA256) */
    this.contentHash = validated.contentHash;

    /** @type {Date|undefined} Release date */
    this.releaseDate = validated.releaseDate;

    Object.freeze(this); // Immutable
  }

  /**
   * Get a unique identifier for this release
   *
   * @returns {string} Unique release ID
   */
  getReleaseId() {
    return `${this.namespaceIri}@${this.version}`;
  }

  /**
   * Check if this release matches a given content hash
   *
   * @param {string} hash - SHA256 hash to compare
   * @returns {boolean} True if hashes match
   */
  matchesHash(hash) {
    return this.contentHash === hash;
  }

  /**
   * Convert to JSON representation
   *
   * @returns {Object} JSON representation
   */
  toJSON() {
    return {
      namespaceIri: this.namespaceIri,
      version: this.version,
      distributionUrl: this.distributionUrl,
      contentHash: this.contentHash,
      releaseDate: this.releaseDate?.toISOString(),
    };
  }

  /**
   * Create OntologyRelease from JSON
   *
   * @param {Object} json - JSON representation
   * @returns {OntologyRelease} OntologyRelease instance
   */
  static fromJSON(json) {
    return new OntologyRelease({
      ...json,
      releaseDate: json.releaseDate ? new Date(json.releaseDate) : undefined,
    });
  }
}

/**
 * Represents an allowed ontology with one or more releases
 *
 * @class AllowedOntology
 *
 * @example
 * ```javascript
 * const prov = new AllowedOntology({
 *   namespaceIri: 'http://www.w3.org/ns/prov#',
 *   name: 'PROV-O',
 *   description: 'W3C Provenance Ontology',
 *   releases: [
 *     new OntologyRelease({
 *       namespaceIri: 'http://www.w3.org/ns/prov#',
 *       version: '2013-04-30',
 *       contentHash: 'abc123...',
 *     })
 *   ]
 * });
 * ```
 */
export class AllowedOntology {
  /**
   * Create an allowed ontology
   *
   * @param {Object} config - Ontology configuration
   * @param {string} config.namespaceIri - Namespace IRI
   * @param {string} config.name - Human-readable name
   * @param {string} [config.description] - Description
   * @param {Array<OntologyRelease>} config.releases - Available releases
   */
  constructor(config) {
    const validated = AllowedOntologySchema.parse(config);

    /** @type {string} Namespace IRI */
    this.namespaceIri = validated.namespaceIri;

    /** @type {string} Human-readable name */
    this.name = validated.name;

    /** @type {string|undefined} Description */
    this.description = validated.description;

    /** @type {Array<OntologyRelease>} Available releases */
    this.releases = validated.releases;

    Object.freeze(this); // Immutable
  }

  /**
   * Get the latest release (by version string - assumes sorted)
   *
   * @returns {OntologyRelease} Latest release
   */
  getLatestRelease() {
    if (this.releases.length === 0) {
      throw new Error(`No releases available for ${this.namespaceIri}`);
    }

    return this.releases[this.releases.length - 1];
  }

  /**
   * Get a specific release by version
   *
   * @param {string} version - Version identifier
   * @returns {OntologyRelease|undefined} Release if found
   */
  getRelease(version) {
    return this.releases.find((r) => r.version === version);
  }

  /**
   * Check if ontology contains a release with given hash
   *
   * @param {string} hash - SHA256 hash
   * @returns {OntologyRelease|undefined} Matching release if found
   */
  findReleaseByHash(hash) {
    return this.releases.find((r) => r.matchesHash(hash));
  }

  /**
   * Get all release versions
   *
   * @returns {Array<string>} Array of version strings
   */
  getVersions() {
    return this.releases.map((r) => r.version);
  }

  /**
   * Convert to JSON representation
   *
   * @returns {Object} JSON representation
   */
  toJSON() {
    return {
      namespaceIri: this.namespaceIri,
      name: this.name,
      description: this.description,
      releases: this.releases.map((r) => r.toJSON()),
    };
  }

  /**
   * Create AllowedOntology from JSON
   *
   * @param {Object} json - JSON representation
   * @returns {AllowedOntology} AllowedOntology instance
   */
  static fromJSON(json) {
    return new AllowedOntology({
      ...json,
      releases: json.releases.map((r) => OntologyRelease.fromJSON(r)),
    });
  }
}
