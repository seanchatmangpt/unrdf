Feature: Core Filters System with Complete Mocking (Migration from Unjucks)
  As a template developer migrating from unjucks
  I want all 47 core filters to work with mocked dependencies
  So that template rendering is fast, isolated, and deterministic

  Background:
    Given a mocked filter registry with deterministic implementations
    And stubbed external filter dependencies (date libraries, formatters, validators)
    And deterministic test data generators for each filter category
    And interaction tracking for behavior verification

  @P0 @filters @text-processing @migration
  Scenario Outline: Text transformation filters with mocked dependencies
    Given a mock filter "<filter>" with stubbed implementation
    And deterministic input "<input>"
    And no external text processing libraries
    When I apply the filter through the mock
    Then the output should be "<expected>"
    And the mock was called with correct parameters
    And no external dependencies were accessed
    And execution time should be under 1ms

    Examples:
      | filter      | input              | expected            |
      | upper       | hello world        | HELLO WORLD         |
      | lower       | HELLO WORLD        | hello world         |
      | capitalize  | hello world        | Hello World         |
      | title       | hello-world test   | Hello-World Test    |
      | camelCase   | hello-world_test   | helloWorldTest      |
      | snakeCase   | HelloWorldTest     | hello_world_test    |
      | kebabCase   | HelloWorldTest     | hello-world-test    |
      | pascalCase  | hello-world        | HelloWorld          |
      | swapCase    | Hello World        | hELLO wORLD         |
      | reverse     | hello              | olleh               |

  @P0 @filters @string-manipulation @migration
  Scenario Outline: String manipulation filters with mocked utilities
    Given a mock filter "<filter>" with deterministic behavior
    And stubbed string utility functions
    And input string "<input>" with parameters "<params>"
    When the filter is applied with mocked utilities
    Then the result should be "<expected>"
    And all utility calls should go through mocks
    And the behavior should be verified

    Examples:
      | filter    | input           | params        | expected          |
      | truncate  | Hello World     | 5             | Hello...          |
      | pad       | test            | 8, "*"        | **test**          |
      | trim      |   hello world   |               | hello world       |
      | ltrim     |   hello world   |               | hello world       |
      | rtrim     | hello world     |               | hello world       |
      | repeat    | abc             | 3             | abcabcabc         |
      | replace   | hello world     | "world", "JS" | hello JS          |
      | slice     | hello world     | 6             | world             |
      | substring | hello world     | 0, 5          | hello             |

  @P0 @filters @array-processing @migration
  Scenario: Array manipulation filters with mocked collections
    Given a mocked array filter registry
    And stubbed array utility functions
    And test arrays with deterministic data
    When I apply array filters with mocked dependencies
    Then all operations should work through mocks
    And results should be deterministic

    Examples:
      | filter     | input_array           | params     | expected_result        |
      | join       | ["a", "b", "c"]       | "|"        | "a|b|c"               |
      | first      | [1, 2, 3, 4, 5]       |            | 1                     |
      | last       | [1, 2, 3, 4, 5]       |            | 5                     |
      | length     | [1, 2, 3]             |            | 3                     |
      | reverse    | [1, 2, 3]             |            | [3, 2, 1]             |
      | sort       | [3, 1, 2]             |            | [1, 2, 3]             |
      | unique     | [1, 2, 2, 3]          |            | [1, 2, 3]             |
      | slice      | [1, 2, 3, 4, 5]       | 1, 3       | [2, 3]                |
      | includes   | [1, 2, 3]             | 2          | true                  |
      | map        | [1, 2, 3]             | "x => x*2" | [2, 4, 6]             |
      | filter     | [1, 2, 3, 4]          | "x => x>2" | [3, 4]                |

  @P0 @filters @date-time @migration
  Scenario: Date and time filters with completely mocked date libraries
    Given mocked date formatting libraries (moment, date-fns, etc.)
    And frozen time at "2024-01-01T12:00:00.000Z"
    And stubbed timezone operations
    And mocked locale data
    When I use date filters with frozen time
    Then all date operations should be deterministic
    And no real date libraries should be called
    And timezone calculations should be mocked

    Examples:
      | filter       | input                    | format          | expected_output      |
      | date         | "2024-01-01"            | "YYYY-MM-DD"    | "2024-01-01"        |
      | dateFormat   | "2024-01-01T12:00:00Z"  | "MMM DD, YYYY"  | "Jan 01, 2024"      |
      | timeAgo      | "2024-01-01T11:00:00Z"  |                 | "1 hour ago"        |
      | duration     | 3661                     |                 | "1h 1m 1s"          |
      | timestamp    | "2024-01-01T12:00:00Z"  |                 | 1704110400000       |
      | toDate       | 1704110400000            |                 | "2024-01-01T12:00:00.000Z" |

  @P0 @filters @number-formatting @migration
  Scenario: Number formatting filters with mocked formatters
    Given mocked number formatting libraries (Intl, numeral, etc.)
    And stubbed locale formatting functions
    And deterministic number inputs
    When I apply number filters with mocked formatters
    Then all formatting should go through mocks
    And locale-specific behavior should be stubbed

    Examples:
      | filter      | input     | params              | expected_output |
      | number      | "123.456" |                     | 123.456         |
      | round       | 123.456   | 2                   | 123.46          |
      | floor       | 123.456   |                     | 123             |
      | ceil        | 123.456   |                     | 124             |
      | abs         | -123.456  |                     | 123.456         |
      | currency    | 123.45    | "USD"               | "$123.45"       |
      | percent     | 0.1234    | 2                   | "12.34%"        |
      | filesize    | 1024      |                     | "1.0 KB"        |
      | ordinal     | 1         |                     | "1st"           |
      | scientific  | 123456    |                     | "1.23e+5"       |

  @P0 @filters @encoding @migration
  Scenario: Encoding and escaping filters with mocked encoders
    Given mocked encoding libraries (he, qs, base64, etc.)
    And stubbed escape/unescape functions
    And test strings with special characters
    When I apply encoding filters
    Then all encoding should use mocked libraries
    And security-sensitive operations should be verified

    Examples:
      | filter       | input                  | expected_output           |
      | escape       | <script>alert(1)</script> | &lt;script&gt;alert(1)&lt;/script&gt; |
      | unescape     | &lt;div&gt;           | <div>                     |
      | urlencode    | hello world           | hello%20world             |
      | urldecode    | hello%20world         | hello world               |
      | base64encode | hello world           | aGVsbG8gd29ybGQ=          |
      | base64decode | aGVsbG8gd29ybGQ=      | hello world               |
      | jsonEncode   | {"key": "value"}      | "{\"key\":\"value\"}"     |
      | jsonDecode   | "{\"key\":\"value\"}" | {"key": "value"}          |

  @P0 @filters @validation @migration
  Scenario: Validation and testing filters with mocked validators
    Given mocked validation libraries (validator, joi, etc.)
    And stubbed regex engines
    And test data for validation scenarios
    When I use validation filters
    Then all validation logic should be mocked
    And regex operations should be deterministic

    Examples:
      | filter     | input                    | expected_result |
      | isEmpty    | ""                       | true            |
      | isEmpty    | "hello"                  | false           |
      | isNumber   | "123"                    | true            |
      | isNumber   | "abc"                    | false           |
      | isEmail    | "test@example.com"       | true            |
      | isEmail    | "invalid-email"          | false           |
      | isUrl      | "https://example.com"    | true            |
      | isUrl      | "not-a-url"              | false           |
      | matches    | "hello"                  | "h.*o"          | true |
      | startsWith | "hello world"            | "hello"         | true |
      | endsWith   | "hello world"            | "world"         | true |

  @P0 @filters @markdown @migration
  Scenario: Markdown and markup filters with mocked parsers
    Given mocked markdown libraries (marked, markdown-it, etc.)
    And stubbed HTML sanitizers
    And test markdown content
    When I apply markup filters
    Then all parsing should use mocked libraries
    And HTML output should be sanitized through mocks

    Examples:
      | filter     | input                    | expected_output              |
      | markdown   | "# Hello World"          | "<h1>Hello World</h1>"       |
      | markdown   | "**bold text**"          | "<p><strong>bold text</strong></p>" |
      | striptags  | "<p>Hello <b>World</b></p>" | "Hello World"             |
      | nl2br      | "Line 1\nLine 2"         | "Line 1<br>Line 2"          |

  @P0 @filters @default-values @migration
  Scenario: Default value and fallback filters with mocked conditions
    Given mocked condition evaluators
    And test data with missing/null values
    When I apply default value filters
    Then fallback logic should be deterministic
    And all conditions should be evaluated through mocks

    Examples:
      | filter       | input     | fallback    | expected_output |
      | default      | null      | "fallback"  | "fallback"      |
      | default      | ""        | "fallback"  | "fallback"      |
      | default      | "value"   | "fallback"  | "value"         |
      | or           | false     | true        | true            |
      | or           | 0         | 42          | 42              |
      | ifEmpty      | []        | [1,2,3]     | [1,2,3]         |

  @P0 @filters @performance @migration
  Scenario: Filter performance with mocked heavy operations
    Given mocked CPU-intensive filter operations
    And large test datasets (1000+ items)
    And performance monitoring enabled
    When I apply filters to large datasets
    Then all operations should complete within time limits
    And memory usage should be tracked through mocks
    And no actual heavy computations should occur

  @P0 @filters @chaining @migration
  Scenario: Filter chaining with complete mock isolation
    Given multiple chained filters with mocked implementations
    And complex filter pipeline: input → filter1 → filter2 → filter3 → output
    When I execute the filter chain
    Then each filter should receive input from the previous mock
    And the entire chain should be deterministic
    And intermediate values should be verifiable
    And the final output should match expected golden value

    Example:
      | input         | chain                                    | expected_output    |
      | "hello-world" | camelCase → upper → truncate(8) → lower | "hellowor"         |
      | "123,456,789" | split(",") → map(number) → sum           | 123456789          |

  @P0 @filters @error-handling @migration
  Scenario: Filter error handling with mocked failures
    Given filters that can encounter error conditions
    And mocked error scenarios for each filter type
    When filters encounter mocked errors:
      | filter_type | error_condition        | expected_behavior     |
      | date        | invalid_date_string    | return_fallback       |
      | number      | non_numeric_input      | return_nan_or_zero    |
      | json        | malformed_json         | throw_parse_error     |
      | regex       | invalid_pattern        | throw_regex_error     |
      | url         | malformed_url          | return_false          |
    Then error handling should be consistent
    And error messages should be deterministic
    And no real external libraries should be called during errors

  @P0 @filters @i18n @migration
  Scenario: Internationalization filters with mocked locales
    Given mocked i18n libraries and locale data
    And stubbed translation functions
    And test content in multiple languages
    When I use i18n filters
    Then all locale operations should be mocked
    And translation lookups should be deterministic
    And currency/date formatting should respect mocked locales

    Examples:
      | filter      | input    | locale  | expected_output    |
      | currency    | 123.45   | "en-US" | "$123.45"         |
      | currency    | 123.45   | "de-DE" | "123,45 €"        |
      | translate   | "hello"  | "es"    | "hola"            |
      | pluralize   | 1        | "en"    | "item"            |
      | pluralize   | 5        | "en"    | "items"           |