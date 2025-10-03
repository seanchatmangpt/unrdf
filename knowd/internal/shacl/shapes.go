package shacl

// Shape represents a SHACL shape definition.
type Shape struct {
	ID                string           `json:"id"`
	Type              string           `json:"type"`
	TargetClass       string           `json:"targetClass"`
	TargetNode        string           `json:"targetNode"`
	TargetSubjectsOf  string           `json:"targetSubjectsOf"`
	Properties        []*PropertyShape `json:"properties"`
	Constraints       []*Constraint    `json:"constraints"`
	Closed            bool             `json:"closed"`
	IgnoredProperties []string         `json:"ignoredProperties"`
}

// PropertyShape represents a SHACL property shape with advanced constraints.
type PropertyShape struct {
	ID                           string   `json:"id"`
	Path                         string   `json:"path"`
	MinCount                     int      `json:"minCount"`
	MaxCount                     int      `json:"maxCount"`
	Datatype                     string   `json:"datatype"`
	Class                        string   `json:"class"`
	NodeKind                     string   `json:"nodeKind"`
	MinLength                    int      `json:"minLength"`
	MaxLength                    int      `json:"maxLength"`
	Pattern                      string   `json:"pattern"`
	LanguageIn                   []string `json:"languageIn"`
	In                           []string `json:"in"`
	QualifiedMinCount            int      `json:"qualifiedMinCount"`
	QualifiedMaxCount            int      `json:"qualifiedMaxCount"`
	QualifiedValueShapesDisjoint bool     `json:"qualifiedValueShapesDisjoint"`
	Or                           []*Shape `json:"or"`
	Xone                         []*Shape `json:"xone"`
	Flags                        *bool    `json:"flags"`
}

// Constraint represents a SHACL constraint with advanced features.
type Constraint struct {
	Type       string                 `json:"type"`
	Components map[string]interface{} `json:"components"`
	Severity   string                 `json:"severity"`
	Message    string                 `json:"message"`
}

// PropertyPath represents complex SHACL property paths.
type PropertyPath struct {
	Type        string          `json:"type"`
	Predicate   string          `json:"predicate"`
	Sequence    []*PropertyPath `json:"sequence"`
	Alternative *PropertyPath   `json:"alternative"`
	ZeroOrMore  *PropertyPath   `json:"zeroOrMore"`
	OneOrMore   *PropertyPath   `json:"oneOrMore"`
	ZeroOrOne   *PropertyPath   `json:"zeroOrOne"`
	Inverse     *PropertyPath   `json:"inverse"`
}

// ValidationReport represents a comprehensive SHACL validation report.
type ValidationReport struct {
	Type                string                 `json:"type"`
	Conforms            bool                   `json:"conforms"`
	Results             []*Violation           `json:"results"`
	Severity            map[string]interface{} `json:"severity,omitempty"`
	ProcessingRulesUsed bool                   `json:"processingRulesUsed"`
	ValidationRulesUsed bool                   `json:"validationRulesUsed"`
}

// Violation represents a detailed SHACL validation violation.
type Violation struct {
	Type                      string                 `json:"@type"`
	FocusNode                 string                 `json:"focusNode"`
	ResultPath                string                 `json:"resultPath"`
	SourceConstraintComponent string                 `json:"sourceConstraintComponent"`
	SourceShape               string                 `json:"sourceShape"`
	Severity                  string                 `json:"severity"`
	Message                   string                 `json:"message"`
	Component                 string                 `json:"component"`
	Value                     string                 `json:"value,omitempty"`
	Details                   map[string]interface{} `json:"details,omitempty"`
}
