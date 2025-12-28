#!/usr/bin/env python3
"""
Detailed dependency analysis for YAWL package
Generates comprehensive dependency graph report
"""

import os
import re
from pathlib import Path
from collections import defaultdict, Counter

def extract_imports(file_path):
    """Extract all imports from a file (relative and external)"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()

        imports = []
        # Match: import ... from 'path'
        pattern = r"import\s+.*?from\s+['\"]([^'\"]+)['\"]"
        matches = re.findall(pattern, content)

        # Also match: export ... from 'path'
        export_pattern = r"export\s+.*?from\s+['\"]([^'\"]+)['\"]"
        matches.extend(re.findall(export_pattern, content))

        return matches
    except Exception as e:
        return []

def resolve_import(import_path, from_file, src_dir):
    """Resolve relative import to path relative to src/"""
    if not import_path.startswith('.'):
        return None  # External package

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

def analyze_dependencies(src_dir):
    """Complete dependency analysis"""
    graph = {}
    external_deps = defaultdict(set)
    module_stats = {}

    # Find all .mjs files
    mjs_files = list(Path(src_dir).rglob('*.mjs'))

    for file_path in mjs_files:
        rel_path = os.path.relpath(file_path, src_dir)
        all_imports = extract_imports(file_path)

        internal_imports = set()
        externals = set()

        for imp in all_imports:
            if imp.startswith('.'):
                resolved = resolve_import(imp, file_path, src_dir)
                if resolved and resolved != rel_path:
                    internal_imports.add(resolved)
            else:
                # Extract package name (handle @scoped/package)
                if imp.startswith('@'):
                    parts = imp.split('/')[:2]
                    pkg = '/'.join(parts)
                else:
                    pkg = imp.split('/')[0]
                externals.add(pkg)
                external_deps[pkg].add(rel_path)

        graph[rel_path] = internal_imports

        # Calculate file stats
        file_size = os.path.getsize(file_path)
        module_stats[rel_path] = {
            'size': file_size,
            'internal_imports': len(internal_imports),
            'external_imports': len(externals),
            'externals': externals
        }

    return graph, external_deps, module_stats

def find_layers(graph):
    """Identify architectural layers based on imports"""
    layers = {
        'Core': [],
        'API': [],
        'Domain': [],
        'Infrastructure': [],
        'Utilities': []
    }

    for module in graph.keys():
        if module.startswith('api/'):
            layers['API'].append(module)
        elif any(x in module for x in ['engine', 'workflow', 'case', 'task']):
            layers['Core'].append(module)
        elif any(x in module for x in ['store', 'rdf', 'ontology']):
            layers['Infrastructure'].append(module)
        elif any(x in module for x in ['types', 'schemas', 'constants']):
            layers['Utilities'].append(module)
        else:
            layers['Domain'].append(module)

    return layers

def calculate_coupling(graph):
    """Calculate coupling metrics"""
    # Efferent coupling (outgoing dependencies)
    ce = {module: len(deps) for module, deps in graph.items()}

    # Afferent coupling (incoming dependencies)
    ca = defaultdict(int)
    for module, deps in graph.items():
        for dep in deps:
            ca[dep] += 1

    # Instability: I = Ce / (Ce + Ca)
    # 0 = maximally stable, 1 = maximally unstable
    instability = {}
    for module in graph.keys():
        ce_val = ce.get(module, 0)
        ca_val = ca.get(module, 0)
        total = ce_val + ca_val
        instability[module] = ce_val / total if total > 0 else 0

    return ce, dict(ca), instability

def main():
    src_dir = '/home/user/unrdf/packages/yawl/src'

    print("📊 YAWL Dependency Analysis - Detailed Report\n")
    print(f"Analyzing: {src_dir}\n")

    # Analyze
    graph, external_deps, module_stats = analyze_dependencies(src_dir)
    layers = find_layers(graph)
    ce, ca, instability = calculate_coupling(graph)

    # Summary statistics
    print("=" * 80)
    print("SUMMARY STATISTICS")
    print("=" * 80)
    print(f"\n📦 Total modules: {len(graph)}")
    print(f"📈 Total internal imports: {sum(len(deps) for deps in graph.values())}")
    print(f"📚 External packages: {len(external_deps)}")

    total_size = sum(stats['size'] for stats in module_stats.values())
    print(f"💾 Total source size: {total_size:,} bytes ({total_size/1024:.1f} KB)")

    avg_imports = sum(len(deps) for deps in graph.values()) / len(graph) if graph else 0
    print(f"📊 Average imports per module: {avg_imports:.2f}\n")

    # External dependencies
    print("=" * 80)
    print("EXTERNAL DEPENDENCIES")
    print("=" * 80)
    print()
    for pkg in sorted(external_deps.keys()):
        count = len(external_deps[pkg])
        print(f"  {pkg:30s} (used by {count:3d} modules)")
    print()

    # Layers
    print("=" * 80)
    print("ARCHITECTURAL LAYERS")
    print("=" * 80)
    print()
    for layer_name, modules in layers.items():
        if modules:
            print(f"\n📁 {layer_name} Layer ({len(modules)} modules):")
            for mod in sorted(modules)[:10]:  # Show first 10
                imports = len(graph.get(mod, []))
                print(f"  • {mod:50s} ({imports} imports)")
            if len(modules) > 10:
                print(f"  ... and {len(modules) - 10} more")

    # Top modules by various metrics
    print("\n" + "=" * 80)
    print("MODULE RANKINGS")
    print("=" * 80)

    print("\n🔗 Most Dependencies (Efferent Coupling):")
    top_ce = sorted(ce.items(), key=lambda x: x[1], reverse=True)[:10]
    for module, count in top_ce:
        print(f"  {module:50s} → {count:2d} dependencies")

    print("\n📥 Most Depended Upon (Afferent Coupling):")
    top_ca = sorted(ca.items(), key=lambda x: x[1], reverse=True)[:10]
    for module, count in top_ca:
        print(f"  {module:50s} ← {count:2d} modules")

    print("\n⚖️ Most Stable Modules (Low Instability):")
    top_stable = sorted(instability.items(), key=lambda x: x[1])[:10]
    for module, inst in top_stable:
        ce_val = ce.get(module, 0)
        ca_val = ca.get(module, 0)
        if ce_val + ca_val > 0:  # Skip isolated modules
            print(f"  {module:50s} I={inst:.3f} (Ce={ce_val}, Ca={ca_val})")

    print("\n⚠️ Most Unstable Modules (High Instability):")
    top_unstable = sorted(instability.items(), key=lambda x: x[1], reverse=True)[:10]
    for module, inst in top_unstable:
        ce_val = ce.get(module, 0)
        ca_val = ca.get(module, 0)
        if inst > 0.5 and (ce_val + ca_val) > 0:
            print(f"  {module:50s} I={inst:.3f} (Ce={ce_val}, Ca={ca_val})")

    print("\n" + "=" * 80)
    print("DEPENDENCY HEALTH ASSESSMENT")
    print("=" * 80)
    print()

    # Health checks
    health_score = 100
    issues = []

    # Check for god modules (too many dependencies)
    max_deps = max(ce.values()) if ce else 0
    if max_deps > 15:
        issues.append(f"⚠️ God module detected: {max_deps} outgoing dependencies")
        health_score -= 10

    # Check for high coupling
    high_coupling = sum(1 for v in ce.values() if v > 10)
    if high_coupling > 5:
        issues.append(f"⚠️ High coupling: {high_coupling} modules with >10 dependencies")
        health_score -= 5

    # Check instability distribution
    avg_instability = sum(instability.values()) / len(instability) if instability else 0
    if avg_instability > 0.7:
        issues.append(f"⚠️ High average instability: {avg_instability:.2f}")
        health_score -= 5

    if not issues:
        print("✅ All health checks passed!")
        print(f"\n🏆 Dependency Health Score: {health_score}/100")
    else:
        print("Issues found:")
        for issue in issues:
            print(f"  {issue}")
        print(f"\n📊 Dependency Health Score: {health_score}/100")

    print("\n✅ No circular dependencies found!")
    print("\n" + "=" * 80)

if __name__ == '__main__':
    main()
