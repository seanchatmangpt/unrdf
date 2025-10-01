#!/usr/bin/env python3
import re
import os

def main():
    # locate input KGC-CONVO.md in sibling 'src' directory
    script_dir = os.path.abspath(os.path.dirname(__file__))
    infile = os.path.abspath(os.path.join(script_dir, '..', 'src', 'KGC-CONVO.md'))
    # output to 'src' directory
    outdir = os.path.abspath(os.path.join(script_dir, '..', 'src'))
    chapters = [
        {'pattern': r'^Abstract$', 'filename': '01-abstract.md'},
        {'pattern': r'^Part I:', 'filename': '02-partI-theoretical-foundation.md'},
        {'pattern': r'^1\. ', 'filename': '03-section1-limits-of-newtonian-computation.md'},
        {'pattern': r'^2\. ', 'filename': '04-section2-relativistic-paradigm.md'},
        {'pattern': r'^3\. ', 'filename': '05-section3-geometry-of-knowledge.md'},
        {'pattern': r'^Part II:', 'filename': '06-partII-architectural-realization.md'},
        {'pattern': r'^4\. ', 'filename': '07-section4-substrate-rdf-framework.md'},
        {'pattern': r'^5\. ', 'filename': '08-section5-pillars-of-autonomic-governance.md'},
        {'pattern': r'^Part III:', 'filename': '09-partIII-high-performance-applications.md'},
        {'pattern': r'^6\. ', 'filename': '10-section6-case-study-uhft.md'},
        {'pattern': r'^7\. ', 'filename': '11-section7-mechanics-of-determinism.md'},
        {'pattern': r'^Part IV:', 'filename': '12-partIV-strategic-imperative.md'},
        {'pattern': r'^8\. ', 'filename': '13-section8-dark-matter-thesis.md'},
        {'pattern': r'^9\. ', 'filename': '14-section9-blue-ocean-strategy.md'},
        {'pattern': r'^10\. ', 'filename': '15-section10-ipo-generator.md'},
    ]
    regexes = [(re.compile(ch['pattern']), ch['filename']) for ch in chapters]
    with open(os.path.join(outdir, 'KGC-CONVO.md'), encoding='utf-8') as f:
        lines = f.readlines()
    out_file = None
    for line in lines:
        matched = False
        for regex, fname in regexes:
            if regex.match(line):
                if out_file:
                    out_file.close()
                out_path = os.path.join(outdir, fname)
                out_file = open(out_path, 'w', encoding='utf-8')
                out_file.write(line)
                matched = True
                break
        if not matched and out_file:
            out_file.write(line)
    if out_file:
        out_file.close()

if __name__ == '__main__':
    main()