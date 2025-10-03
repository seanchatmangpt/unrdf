// Package version provides build version information for knowd.
package version

import "fmt"

// Version holds the current version string.
// This will be set at build time via -ldflags.
var Version = "dev"

// Commit holds the git commit hash.
// This will be set at build time via -ldflags.
var Commit = "unknown"

// BuildInfo returns a formatted string with version and commit information.
func BuildInfo() string {
	return fmt.Sprintf("knowd %s (commit: %s)", Version, Commit)
}

// FullVersion returns detailed version information.
func FullVersion() string {
	return fmt.Sprintf("Version: %s\nCommit: %s", Version, Commit)
}
