/**
 * KGEN Array Filters - P0 Essential Array Processing
 *
 * The 20% of array filters that provide 80% of value
 * Deterministic implementations with comprehensive error handling
 */

/**
 * Join array elements with separator
 * @param {any} arr - Input array
 * @param {string} separator - Separator string
 * @returns {string} Joined string
 */
export const join = (arr, separator = ',') => {
  if (!Array.isArray(arr)) {
    if (arr === null || arr === undefined) return '';
    return String(arr);
  }
  return arr.join(String(separator));
};

/**
 * Split string into array
 * @param {any} str - Input string
 * @param {string} separator - Separator to split on
 * @returns {Array} Array of strings
 */
export const split = (str, separator = ',') => {
  if (str === null || str === undefined) return [];
  return String(str).split(String(separator));
};

/**
 * Get unique values from array (deterministic order)
 * @param {any} arr - Input array
 * @returns {Array} Array with unique values
 */
export const unique = (arr) => {
  if (!Array.isArray(arr)) return arr;
  return [...new Set(arr)];
};

/**
 * Sort array (deterministic, stable sort)
 * @param {any} arr - Input array
 * @param {string} key - Optional key to sort by for objects
 * @returns {Array} Sorted array
 */
export const sort = (arr, key = null) => {
  if (!Array.isArray(arr)) return arr;

  const sorted = [...arr].sort((a, b) => {
    let aVal = key ? a[key] : a;
    let bVal = key ? b[key] : b;

    // Handle numeric comparison for numbers, string comparison for strings
    if (typeof aVal === 'number' && typeof bVal === 'number') {
      return aVal - bVal;
    }

    // Convert to strings for consistent comparison of mixed types
    aVal = String(aVal);
    bVal = String(bVal);

    if (aVal < bVal) return -1;
    if (aVal > bVal) return 1;
    return 0;
  });

  return sorted;
};

/**
 * Reverse array
 * @param {any} arr - Input array
 * @returns {Array} Reversed array
 */
export const reverse = (arr) => {
  if (!Array.isArray(arr)) return arr;
  return [...arr].reverse();
};

/**
 * Get first n elements
 * @param {any} arr - Input array
 * @param {number} n - Number of elements to take
 * @returns {any} First element if n=1, array otherwise
 */
export const first = (arr, n = 1) => {
  if (!Array.isArray(arr)) return arr;
  if (n === 1) return arr[0];
  return arr.slice(0, Math.max(0, n));
};

/**
 * Get last n elements
 * @param {any} arr - Input array
 * @param {number} n - Number of elements to take
 * @returns {any} Last element if n=1, array otherwise
 */
export const last = (arr, n = 1) => {
  if (!Array.isArray(arr)) return arr;
  if (n === 1) return arr[arr.length - 1];
  return arr.slice(-Math.max(0, n));
};

/**
 * Get array length
 * @param {any} arr - Input array
 * @returns {number} Array length
 */
export const length = (arr) => {
  if (!Array.isArray(arr)) return 0;
  return arr.length;
};

/**
 * Check if array is empty
 * @param {any} arr - Input array
 * @returns {boolean} True if empty
 */
export const isEmpty = (arr) => {
  if (!Array.isArray(arr)) return true;
  return arr.length === 0;
};

/**
 * Filter array by property value
 * @param {any} arr - Input array
 * @param {string} key - Property key to filter by
 * @param {any} value - Value to match
 * @returns {Array} Filtered array
 */
export const filterBy = (arr, key, value) => {
  if (!Array.isArray(arr)) return [];
  return arr.filter(item => {
    const itemValue = key ? item[key] : item;
    return itemValue === value;
  });
};

/**
 * Group array by property value
 * @param {any} arr - Input array
 * @param {string} key - Property key to group by
 * @returns {Object} Grouped object
 */
export const groupBy = (arr, key) => {
  if (!Array.isArray(arr)) return {};

  return arr.reduce((groups, item) => {
    const groupKey = key ? item[key] : item;
    const groupKeyStr = String(groupKey);

    if (!groups[groupKeyStr]) {
      groups[groupKeyStr] = [];
    }
    groups[groupKeyStr].push(item);
    return groups;
  }, {});
};

/**
 * Map array to property values
 * @param {any} arr - Input array
 * @param {string} key - Property key to extract
 * @returns {Array} Array of values
 */
export const mapBy = (arr, key) => {
  if (!Array.isArray(arr)) return [];
  return arr.map(item => key ? item[key] : item);
};

/**
 * Sort array by property (deterministic)
 * @param {any} arr - Input array
 * @param {string} key - Property key to sort by
 * @returns {Array} Sorted array
 */
export const sortBy = (arr, key) => {
  if (!Array.isArray(arr)) return arr;

  return [...arr].sort((a, b) => {
    const aVal = String(key ? a[key] : a);
    const bVal = String(key ? b[key] : b);

    if (aVal < bVal) return -1;
    if (aVal > bVal) return 1;
    return 0;
  });
};

/**
 * Chunk array into groups of specified size
 * @param {any} arr - Input array
 * @param {number} size - Chunk size
 * @returns {Array} Array of chunks
 */
export const chunk = (arr, size = 1) => {
  if (!Array.isArray(arr)) return [];
  const chunkSize = Math.max(1, Math.floor(size));
  const chunks = [];

  for (let i = 0; i < arr.length; i += chunkSize) {
    chunks.push(arr.slice(i, i + chunkSize));
  }

  return chunks;
};

/**
 * Flatten nested arrays
 * @param {any} arr - Input array
 * @param {number} depth - Depth to flatten (default: Infinity)
 * @returns {Array} Flattened array
 */
export const flatten = (arr, depth = Infinity) => {
  if (!Array.isArray(arr)) return arr;
  return arr.flat(depth);
};

/**
 * Get random element (deterministic based on content hash)
 * @param {any} arr - Input array
 * @returns {any} Deterministic "random" element
 */
export const sample = (arr) => {
  if (!Array.isArray(arr) || arr.length === 0) return null;

  // Create deterministic "random" index based on array content
  const contentHash = JSON.stringify(arr).split('').reduce((hash, char) => {
    hash = ((hash << 5) - hash) + char.charCodeAt(0);
    return hash & hash; // Convert to 32-bit integer
  }, 0);

  const index = Math.abs(contentHash) % arr.length;
  return arr[index];
};

/**
 * Check if array contains value
 * @param {any} arr - Input array
 * @param {any} value - Value to search for
 * @returns {boolean} True if contains value
 */
export const contains = (arr, value) => {
  if (!Array.isArray(arr)) return false;
  return arr.includes(value);
};

/**
 * Get intersection of two arrays
 * @param {any} arr1 - First array
 * @param {any} arr2 - Second array
 * @returns {Array} Intersection array
 */
export const intersect = (arr1, arr2) => {
  if (!Array.isArray(arr1) || !Array.isArray(arr2)) return [];
  return arr1.filter(item => arr2.includes(item));
};

/**
 * Get difference between two arrays (items in arr1 but not arr2)
 * @param {any} arr1 - First array
 * @param {any} arr2 - Second array
 * @returns {Array} Difference array
 */
export const difference = (arr1, arr2) => {
  if (!Array.isArray(arr1)) return [];
  if (!Array.isArray(arr2)) return [...arr1];
  return arr1.filter(item => !arr2.includes(item));
};

/**
 * Concatenate arrays
 * @param {any} arr1 - First array
 * @param {any} arr2 - Second array
 * @returns {Array} Concatenated array
 */
export const concat = (arr1, arr2) => {
  const a1 = Array.isArray(arr1) ? arr1 : [arr1];
  const a2 = Array.isArray(arr2) ? arr2 : [arr2];
  return [...a1, ...a2];
};

/**
 * Zip two arrays together into pairs
 * @param {any} arr1 - First array
 * @param {any} arr2 - Second array
 * @returns {Array} Array of [item1, item2] pairs
 */
export const zip = (arr1, arr2) => {
  if (!Array.isArray(arr1) || !Array.isArray(arr2)) return [];
  const minLength = Math.min(arr1.length, arr2.length);
  const result = [];

  for (let i = 0; i < minLength; i++) {
    result.push([arr1[i], arr2[i]]);
  }

  return result;
};

/**
 * Unzip array of pairs into two arrays
 * @param {any} arr - Array of pairs
 * @returns {Array} [array1, array2]
 */
export const unzip = (arr) => {
  if (!Array.isArray(arr)) return [[], []];
  const result1 = [];
  const result2 = [];

  for (const item of arr) {
    if (Array.isArray(item) && item.length >= 2) {
      result1.push(item[0]);
      result2.push(item[1]);
    }
  }

  return [result1, result2];
};

/**
 * Transpose a 2D array (matrix)
 * @param {any} matrix - 2D array
 * @returns {Array} Transposed matrix
 */
export const transpose = (matrix) => {
  if (!Array.isArray(matrix) || matrix.length === 0) return [];
  if (!Array.isArray(matrix[0])) return matrix;

  const rows = matrix.length;
  const cols = matrix[0].length;
  const result = [];

  for (let col = 0; col < cols; col++) {
    const newRow = [];
    for (let row = 0; row < rows; row++) {
      if (Array.isArray(matrix[row]) && col < matrix[row].length) {
        newRow.push(matrix[row][col]);
      }
    }
    result.push(newRow);
  }

  return result;
};

/**
 * Get minimum value from array
 * @param {any} arr - Input array
 * @returns {any} Minimum value
 */
export const min = (arr) => {
  if (!Array.isArray(arr) || arr.length === 0) return null;
  return Math.min(...arr.filter(x => typeof x === 'number' && !isNaN(x)));
};

/**
 * Get maximum value from array
 * @param {any} arr - Input array
 * @returns {any} Maximum value
 */
export const max = (arr) => {
  if (!Array.isArray(arr) || arr.length === 0) return null;
  return Math.max(...arr.filter(x => typeof x === 'number' && !isNaN(x)));
};

/**
 * Sum all numeric values in array, optionally by property
 * @param {any} arr - Input array
 * @param {string} key - Optional property key to sum by
 * @returns {number} Sum of values
 */
export const sum = (arr, key = null) => {
  if (!Array.isArray(arr)) return 0;

  if (key) {
    // Sum by property
    return arr
      .map(item => item && typeof item === 'object' ? item[key] : 0)
      .filter(x => typeof x === 'number' && !isNaN(x))
      .reduce((sum, val) => sum + val, 0);
  }

  // Sum numeric values directly
  return arr
    .filter(x => typeof x === 'number' && !isNaN(x))
    .reduce((sum, val) => sum + val, 0);
};

/**
 * Calculate average of numeric values in array
 * @param {any} arr - Input array
 * @returns {number} Average value
 */
export const avg = (arr) => {
  if (!Array.isArray(arr) || arr.length === 0) return 0;
  const numbers = arr.filter(x => typeof x === 'number' && !isNaN(x));
  return numbers.length > 0 ? sum(numbers) / numbers.length : 0;
};

// Collection of all array filters for easy import
export const arrayFilters = {
  join,
  split,
  unique,
  sort,
  reverse,
  first,
  last,
  length,
  isEmpty,
  filterBy,
  groupBy,
  mapBy,
  map: mapBy, // Alias for compatibility
  sortBy,
  chunk,
  flatten,
  sample,
  contains,
  intersect,
  difference,
  concat,
  zip,
  unzip,
  transpose,
  min,
  max,
  sum,
  avg
};

export default arrayFilters;