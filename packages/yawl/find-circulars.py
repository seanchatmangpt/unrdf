#!/usr/bin/env python3
"""
Simple circular dependency detector for YAWL package
Checks for bidirectional import patterns: A→B and B→A
"""

import os
import re
from pathlib import Path
from collections import defaultdict

def extract_imports(file_path):
    """Extract all relative imports from a file"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()

        imports = set()
        # Match: import ... from './path' or '../path'
        pattern = r"import\s+.*?from\s+['\"](\.[^'\"]+)['\"]"
        matches = re.findall(pattern, content)

        # Also match: export ... from './path'
        export_pattern = r"export\s+.*?from\s+['\"](\.[^'\"]+)['\"]"
        matches.extend(re.findall(export_pattern, content))

        for match in matches:
            imports.add(match)

        return imports
    except Exception as e:
        return set()

def resolve_import(import_path, from_file, src_dir):
    """Resolve relative import to absolute path"""
    from_dir = os.path.dirname(from_file)
    resolved = os.path.normpath(os.path.join(from_dir, import_path))

    # Add .mjs if missing
    if not resolved.endswith('.mjs'):
        resolved += '.mjs'

    # Make relative to src_dir
    try:
        rel_path = os.path.relpath(resolved, src_dir)
        return rel_path if not rel_path.startswith('..') else None
    except:
        return None

def build_dependency_graph(src_dir):
    """Build dependency graph from all .mjs files"""
    graph = {}

    # Find all .mjs files
    mjs_files = list(Path(src_dir).rglob('*.mjs'))

    for file_path in mjs_files:
        rel_path = os.path.relpath(file_path, src_dir)
        imports = extract_imports(file_path)

        resolved_imports = set()
        for imp in imports:
            resolved = resolve_import(imp, file_path, src_dir)
            if resolved and resolved != rel_path:
                resolved_imports.add(resolved)

        graph[rel_path] = resolved_imports

    return graph

def find_circular_dependencies(graph):
    """Find all bidirectional import pairs"""
    circulars = []

    checked = set()

    for file_a, imports_a in graph.items():
        for file_b in imports_a:
            if file_b not in graph:
                continue

            # Create canonical pair key
            pair = tuple(sorted([file_a, file_b]))
            if pair in checked:
                continue
            checked.add(pair)

            # Check if B also imports A
            if file_a in graph.get(file_b, set()):
                circulars.append((file_a, file_b))

    return circulars

def main():
    src_dir = '/home/user/unrdf/packages/yawl/src'

    print("🔍 YAWL Circular Dependency Analysis\n")
    print(f"Analyzing: {src_dir}\n")

    # Build graph
    graph = build_dependency_graph(src_dir)
    print(f"📊 Total modules: {len(graph)}")

    # Calculate statistics
    total_imports = sum(len(imports) for imports in graph.values())
    avg_imports = total_imports / len(graph) if graph else 0
    print(f"📈 Total imports: {total_imports}")
    print(f"📊 Average imports per module: {avg_imports:.2f}\n")

    # Find circulars
    circulars = find_circular_dependencies(graph)

    if not circulars:
        print("✅ No circular dependencies found!\n")
        return 0
    else:
        print(f"❌ Found {len(circulars)} circular dependency pair(s):\n")
        for i, (file_a, file_b) in enumerate(circulars, 1):
            print(f"🔴 Circular #{i}:")
            print(f"  {file_a}")
            print(f"    ↕")
            print(f"  {file_b}\n")
        return 1

if __name__ == '__main__':
    exit(main())
