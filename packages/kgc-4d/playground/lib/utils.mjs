import { clsx } from "clsx";
import { twMerge } from "tailwind-merge";

/**
 * Merge Tailwind CSS classes
 * @param {...any} inputs - Class inputs
 * @returns {string} Merged classes
 */
export function cn(...inputs) {
  return twMerge(clsx(inputs));
}
