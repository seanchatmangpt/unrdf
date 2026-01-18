/**
 * @file Entity Validation Schemas
 * @description Auto-generated Zod schemas from RDF ontology
 * @generated [TIMESTAMP]
 */
import { z } from 'zod';

/**
 * Post validation schema
 */
export const PostSchema = z.object({
  authorId: z.string(),
  content: z.string(),
  id: z.string(),
  title: z.string(),
});

/**
 * @typedef {z.infer<typeof PostSchema>} Post
 */

/**
 * User validation schema
 */
export const UserSchema = z.object({
  bio: z.string().optional(),
  email: z.string(),
  id: z.string(),
  username: z.string(),
});

/**
 * @typedef {z.infer<typeof UserSchema>} User
 */

// Export all schemas
export const entitySchemas = {
  Post: PostSchema,
  User: UserSchema,
};
