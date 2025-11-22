/**
 * Paper domain model
 * @module domain/models/paper
 */

import { PAPER_FAMILIES } from '../constants.mjs';
import { PaperSchema, PaperFamilySchema } from '../types.mjs';

/**
 * Paper class representing an academic paper
 * @class Paper
 */
export class Paper {
  /**
   * Create a new Paper instance
   * @param {string} family - Paper family (IMRaD, Argument, etc.)
   * @param {string} [title] - Optional paper title
   * @param {string} [abstract] - Optional abstract
   * @param {Object} [options] - Additional options
   * @param {string[]} [options.sections] - Custom sections
   * @param {Record<string, unknown>} [options.metadata] - Additional metadata
   */
  constructor(family, title, abstract, options = {}) {
    // Validate family
    const familyResult = PaperFamilySchema.safeParse(family);
    if (!familyResult.success) {
      throw new Error(`Invalid paper family: ${family}. Valid families: ${PAPER_FAMILIES.map((f) => f.name).join(', ')}`);
    }

    /** @type {string} */
    this.family = family;

    /** @type {string|undefined} */
    this.title = title;

    /** @type {string|undefined} */
    this.abstract = abstract;

    /** @type {string[]} */
    this.sections = options.sections ?? this.getDefaultSections();

    /** @type {Record<string, unknown>} */
    this.metadata = options.metadata ?? {};

    /** @type {string} */
    this.createdAt = new Date().toISOString();

    /** @type {string} */
    this.updatedAt = new Date().toISOString();
  }

  /**
   * Get default sections for this paper family
   * @returns {string[]}
   */
  getDefaultSections() {
    const familyDef = PAPER_FAMILIES.find((f) => f.name === this.family);
    return familyDef ? [...familyDef.sections] : [];
  }

  /**
   * Get the family definition
   * @returns {{name: string, label: string, sections: string[]}|undefined}
   */
  getFamilyDefinition() {
    return PAPER_FAMILIES.find((f) => f.name === this.family);
  }

  /**
   * Get the family label
   * @returns {string}
   */
  getLabel() {
    const def = this.getFamilyDefinition();
    return def ? def.label : this.family;
  }

  /**
   * Add a section to the paper
   * @param {string} section - Section name
   * @param {number} [position] - Optional position (index)
   * @returns {Paper} this instance for chaining
   */
  addSection(section, position) {
    if (typeof position === 'number' && position >= 0 && position <= this.sections.length) {
      this.sections.splice(position, 0, section);
    } else {
      this.sections.push(section);
    }
    this.updatedAt = new Date().toISOString();
    return this;
  }

  /**
   * Remove a section from the paper
   * @param {string} section - Section name to remove
   * @returns {boolean} true if section was removed
   */
  removeSection(section) {
    const index = this.sections.indexOf(section);
    if (index > -1) {
      this.sections.splice(index, 1);
      this.updatedAt = new Date().toISOString();
      return true;
    }
    return false;
  }

  /**
   * Check if paper has a specific section
   * @param {string} section - Section name
   * @returns {boolean}
   */
  hasSection(section) {
    return this.sections.includes(section);
  }

  /**
   * Set metadata value
   * @param {string} key - Metadata key
   * @param {unknown} value - Metadata value
   * @returns {Paper} this instance for chaining
   */
  setMetadata(key, value) {
    this.metadata[key] = value;
    this.updatedAt = new Date().toISOString();
    return this;
  }

  /**
   * Get metadata value
   * @param {string} key - Metadata key
   * @returns {unknown}
   */
  getMetadata(key) {
    return this.metadata[key];
  }

  /**
   * Convert to plain object
   * @returns {import('../types.mjs').PaperData}
   */
  toJSON() {
    return {
      family: this.family,
      title: this.title,
      abstract: this.abstract,
      sections: [...this.sections],
      metadata: { ...this.metadata },
      createdAt: this.createdAt,
      updatedAt: this.updatedAt,
    };
  }

  /**
   * Validate the paper data
   * @returns {{success: boolean, data?: import('../types.mjs').PaperData, error?: import('zod').ZodError}}
   */
  validate() {
    return PaperSchema.safeParse(this.toJSON());
  }

  /**
   * Get all available paper families
   * @returns {Array<{name: string, label: string, sections: string[]}>}
   */
  static all() {
    return PAPER_FAMILIES.map((f) => ({ ...f, sections: [...f.sections] }));
  }

  /**
   * Get a specific paper family definition
   * @param {string} name - Family name
   * @returns {{name: string, label: string, sections: string[]}|undefined}
   */
  static getFamily(name) {
    const family = PAPER_FAMILIES.find((f) => f.name === name);
    return family ? { ...family, sections: [...family.sections] } : undefined;
  }

  /**
   * Check if a family name is valid
   * @param {string} name - Family name to check
   * @returns {boolean}
   */
  static isValidFamily(name) {
    return PaperFamilySchema.safeParse(name).success;
  }

  /**
   * Create from JSON object
   * @param {Object} json - JSON object
   * @returns {Paper}
   * @throws {Error} If validation fails
   */
  static fromJSON(json) {
    const result = PaperSchema.safeParse(json);
    if (!result.success) {
      throw new Error(`Invalid paper data: ${result.error.message}`);
    }
    const data = result.data;
    const paper = new Paper(data.family, data.title, data.abstract, {
      sections: data.sections,
      metadata: data.metadata,
    });
    if (data.createdAt) {
      paper.createdAt = data.createdAt;
    }
    if (data.updatedAt) {
      paper.updatedAt = data.updatedAt;
    }
    return paper;
  }

  /**
   * Create a new paper with defaults for a family
   * @param {string} family - Paper family
   * @returns {Paper}
   */
  static create(family) {
    return new Paper(family);
  }
}
