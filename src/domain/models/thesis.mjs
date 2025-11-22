/**
 * Thesis domain model
 * @module domain/models/thesis
 */

import { THESIS_TYPES } from '../constants.mjs';
import { ThesisSchema, ThesisTypeSchema, ScheduleItemSchema } from '../types.mjs';

/**
 * Thesis class for thesis structures
 * @class Thesis
 */
export class Thesis {
  /**
   * Create a new Thesis instance
   * @param {string} type - Thesis type (Masters, PhD, etc.)
   * @param {Object} [options] - Additional options
   * @param {Array<{milestone: string, dueDate: string, completed?: boolean, notes?: string}>} [options.schedule] - Timeline and milestones
   * @param {string} [options.defenseDate] - Defense date
   * @param {string} [options.title] - Thesis title
   * @param {string[]} [options.sections] - Custom sections
   * @param {string[]} [options.committee] - Committee members
   * @param {Record<string, unknown>} [options.metadata] - Additional metadata
   */
  constructor(type, options = {}) {
    // Validate type
    const typeResult = ThesisTypeSchema.safeParse(type);
    if (!typeResult.success) {
      throw new Error(`Invalid thesis type: ${type}. Valid types: ${THESIS_TYPES.map((t) => t.name).join(', ')}`);
    }

    /** @type {string} */
    this.type = type;

    /** @type {string|undefined} */
    this.title = options.title;

    /** @type {Array<{milestone: string, dueDate: string, completed: boolean, notes?: string}>} */
    this.schedule = this._validateSchedule(options.schedule ?? []);

    /** @type {string|undefined} */
    this.defenseDate = options.defenseDate;

    /** @type {string[]} */
    this.sections = options.sections ?? this.getDefaultSections();

    /** @type {string[]} */
    this.committee = options.committee ?? [];

    /** @type {Record<string, unknown>} */
    this.metadata = options.metadata ?? {};

    /** @type {string} */
    this.createdAt = new Date().toISOString();

    /** @type {string} */
    this.updatedAt = new Date().toISOString();
  }

  /**
   * Validate and normalize schedule items
   * @private
   * @param {Array<Object>} schedule - Schedule items
   * @returns {Array<{milestone: string, dueDate: string, completed: boolean, notes?: string}>}
   */
  _validateSchedule(schedule) {
    return schedule.map((item) => {
      const result = ScheduleItemSchema.safeParse(item);
      if (!result.success) {
        throw new Error(`Invalid schedule item: ${result.error.message}`);
      }
      return result.data;
    });
  }

  /**
   * Get default sections for this thesis type
   * @returns {string[]}
   */
  getDefaultSections() {
    const typeDef = THESIS_TYPES.find((t) => t.name === this.type);
    return typeDef ? [...typeDef.sections] : [];
  }

  /**
   * Get the type definition
   * @returns {{name: string, label: string, sections: string[]}|undefined}
   */
  getTypeDefinition() {
    return THESIS_TYPES.find((t) => t.name === this.type);
  }

  /**
   * Get the type label
   * @returns {string}
   */
  getLabel() {
    const def = this.getTypeDefinition();
    return def ? def.label : this.type;
  }

  /**
   * Add a milestone to the schedule
   * @param {string} milestone - Milestone description
   * @param {string} dueDate - Due date string
   * @param {Object} [options] - Additional options
   * @param {boolean} [options.completed=false] - Whether completed
   * @param {string} [options.notes] - Optional notes
   * @returns {Thesis} this instance for chaining
   */
  addMilestone(milestone, dueDate, options = {}) {
    const item = ScheduleItemSchema.parse({
      milestone,
      dueDate,
      completed: options.completed ?? false,
      notes: options.notes,
    });
    this.schedule.push(item);
    this.updatedAt = new Date().toISOString();
    return this;
  }

  /**
   * Mark a milestone as completed
   * @param {string} milestone - Milestone name
   * @returns {boolean} true if milestone was found and updated
   */
  completeMilestone(milestone) {
    const item = this.schedule.find((s) => s.milestone === milestone);
    if (item) {
      item.completed = true;
      this.updatedAt = new Date().toISOString();
      return true;
    }
    return false;
  }

  /**
   * Get remaining (incomplete) milestones
   * @returns {Array<{milestone: string, dueDate: string, completed: boolean, notes?: string}>}
   */
  getRemainingMilestones() {
    return this.schedule.filter((s) => !s.completed);
  }

  /**
   * Get completed milestones
   * @returns {Array<{milestone: string, dueDate: string, completed: boolean, notes?: string}>}
   */
  getCompletedMilestones() {
    return this.schedule.filter((s) => s.completed);
  }

  /**
   * Calculate progress percentage
   * @returns {number} Progress as percentage (0-100)
   */
  getProgress() {
    if (this.schedule.length === 0) return 0;
    const completed = this.schedule.filter((s) => s.completed).length;
    return Math.round((completed / this.schedule.length) * 100);
  }

  /**
   * Add a committee member
   * @param {string} member - Committee member name
   * @returns {Thesis} this instance for chaining
   */
  addCommitteeMember(member) {
    if (!this.committee.includes(member)) {
      this.committee.push(member);
      this.updatedAt = new Date().toISOString();
    }
    return this;
  }

  /**
   * Remove a committee member
   * @param {string} member - Committee member name
   * @returns {boolean} true if member was removed
   */
  removeCommitteeMember(member) {
    const index = this.committee.indexOf(member);
    if (index > -1) {
      this.committee.splice(index, 1);
      this.updatedAt = new Date().toISOString();
      return true;
    }
    return false;
  }

  /**
   * Set the defense date
   * @param {string} date - Defense date string
   * @returns {Thesis} this instance for chaining
   */
  setDefenseDate(date) {
    this.defenseDate = date;
    this.updatedAt = new Date().toISOString();
    return this;
  }

  /**
   * Check if defense date is in the past
   * @returns {boolean|null} null if no defense date set
   */
  isDefensePast() {
    if (!this.defenseDate) return null;
    return new Date(this.defenseDate) < new Date();
  }

  /**
   * Set metadata value
   * @param {string} key - Metadata key
   * @param {unknown} value - Metadata value
   * @returns {Thesis} this instance for chaining
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
   * @returns {import('../types.mjs').ThesisData}
   */
  toJSON() {
    return {
      type: this.type,
      title: this.title,
      schedule: this.schedule.map((s) => ({ ...s })),
      defenseDate: this.defenseDate,
      sections: [...this.sections],
      committee: [...this.committee],
      metadata: { ...this.metadata },
      createdAt: this.createdAt,
      updatedAt: this.updatedAt,
    };
  }

  /**
   * Validate the thesis data
   * @returns {{success: boolean, data?: import('../types.mjs').ThesisData, error?: import('zod').ZodError}}
   */
  validate() {
    return ThesisSchema.safeParse(this.toJSON());
  }

  /**
   * Get all available thesis types
   * @returns {Array<{name: string, label: string, sections: string[]}>}
   */
  static all() {
    return THESIS_TYPES.map((t) => ({ ...t, sections: [...t.sections] }));
  }

  /**
   * Get a specific thesis type definition
   * @param {string} name - Type name
   * @returns {{name: string, label: string, sections: string[]}|undefined}
   */
  static getType(name) {
    const type = THESIS_TYPES.find((t) => t.name === name);
    return type ? { ...type, sections: [...type.sections] } : undefined;
  }

  /**
   * Check if a type name is valid
   * @param {string} name - Type name to check
   * @returns {boolean}
   */
  static isValidType(name) {
    return ThesisTypeSchema.safeParse(name).success;
  }

  /**
   * Create from JSON object
   * @param {Object} json - JSON object
   * @returns {Thesis}
   * @throws {Error} If validation fails
   */
  static fromJSON(json) {
    const result = ThesisSchema.safeParse(json);
    if (!result.success) {
      throw new Error(`Invalid thesis data: ${result.error.message}`);
    }
    const data = result.data;
    const thesis = new Thesis(data.type, {
      title: data.title,
      schedule: data.schedule,
      defenseDate: data.defenseDate,
      sections: data.sections,
      committee: data.committee,
      metadata: data.metadata,
    });
    if (data.createdAt) {
      thesis.createdAt = data.createdAt;
    }
    if (data.updatedAt) {
      thesis.updatedAt = data.updatedAt;
    }
    return thesis;
  }

  /**
   * Create a new thesis with defaults for a type
   * @param {string} type - Thesis type
   * @returns {Thesis}
   */
  static create(type) {
    return new Thesis(type);
  }
}
