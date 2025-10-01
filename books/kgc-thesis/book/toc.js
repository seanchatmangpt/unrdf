// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded affix "><a href="preface.html">Preface</a></li><li class="chapter-item expanded affix "><li class="spacer"></li><li class="chapter-item expanded affix "><li class="part-title">Part I: Theoretical Foundations</li><li class="chapter-item expanded "><a href="chapter-01/index.html"><strong aria-hidden="true">1.</strong> Chapter 1: Field-Theoretic Foundations</a></li><li class="chapter-item expanded "><a href="chapter-02/index.html"><strong aria-hidden="true">2.</strong> Chapter 2: Related Work and Formal Comparisons</a></li><li class="chapter-item expanded "><a href="chapter-03/index.html"><strong aria-hidden="true">3.</strong> Chapter 3: Knowledge Geometry Calculus - Formal Foundations</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="chapter-hyperdimensional/index.html"><strong aria-hidden="true">3.1.</strong> 3.5 Hyperdimensional Computing Mathematics</a></li></ol></li><li class="chapter-item expanded "><a href="chapter-04/index.html"><strong aria-hidden="true">4.</strong> Chapter 4: Knowledge Hooks - Predicate Algebra</a></li><li class="chapter-item expanded affix "><li class="spacer"></li><li class="chapter-item expanded affix "><li class="part-title">Part II: System Architecture and Validation</li><li class="chapter-item expanded "><a href="chapter-05/index.html"><strong aria-hidden="true">5.</strong> Chapter 5: Reference Implementation</a></li><li class="chapter-item expanded "><a href="chapter-06/index.html"><strong aria-hidden="true">6.</strong> Chapter 6: Empirical Evaluation</a></li><li class="chapter-item expanded "><a href="chapter-07/index.html"><strong aria-hidden="true">7.</strong> Chapter 7: Ultra-High-Frequency Trading Case Study</a></li><li class="chapter-item expanded affix "><li class="spacer"></li><li class="chapter-item expanded affix "><li class="part-title">Part III: Economic and Strategic Foundations</li><li class="chapter-item expanded "><a href="chapter-08/index.html"><strong aria-hidden="true">8.</strong> Chapter 8: Dark Matter 80/20 Economic Thesis</a></li><li class="chapter-item expanded "><a href="chapter-09/index.html"><strong aria-hidden="true">9.</strong> Chapter 9: Blue Ocean Strategic Positioning</a></li><li class="chapter-item expanded "><a href="chapter-10/index.html"><strong aria-hidden="true">10.</strong> Chapter 10: KGEN Case Study - Autonomic IPO Generator</a></li><li class="chapter-item expanded affix "><li class="spacer"></li><li class="chapter-item expanded affix "><li class="part-title">Part IV: Applications and Future Directions</li><li class="chapter-item expanded "><a href="chapter-11/index.html"><strong aria-hidden="true">11.</strong> Chapter 11: Enterprise Applications</a></li><li class="chapter-item expanded "><a href="chapter-12/index.html"><strong aria-hidden="true">12.</strong> Chapter 12: Limitations and Future Work</a></li><li class="chapter-item expanded "><a href="chapter-13/index.html"><strong aria-hidden="true">13.</strong> Chapter 13: The Autonomic Enterprise</a></li><li class="chapter-item expanded affix "><li class="spacer"></li><li class="chapter-item expanded affix "><li class="part-title">Appendices</li><li class="chapter-item expanded "><a href="appendix-a-proofs.html"><strong aria-hidden="true">14.</strong> Appendix A: Complete Proofs</a></li><li class="chapter-item expanded "><a href="appendix-b-complexity.html"><strong aria-hidden="true">15.</strong> Appendix B: Complexity Analysis</a></li><li class="chapter-item expanded "><a href="appendix-c-metrics.html"><strong aria-hidden="true">16.</strong> Appendix C: Implementation Metrics</a></li><li class="chapter-item expanded affix "><li class="spacer"></li><li class="chapter-item expanded affix "><li class="part-title">Reference Materials</li><li class="chapter-item expanded "><a href="notation-reference.html"><strong aria-hidden="true">17.</strong> Calculus Notation Reference for AI Swarms</a></li><li class="chapter-item expanded "><a href="glossary.html"><strong aria-hidden="true">18.</strong> Glossary</a></li><li class="chapter-item expanded "><a href="index.html"><strong aria-hidden="true">19.</strong> Index</a></li><li class="chapter-item expanded "><a href="references.html"><strong aria-hidden="true">20.</strong> References</a></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString().split("#")[0].split("?")[0];
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
