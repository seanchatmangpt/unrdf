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
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded "><a href="introduction.html"><strong aria-hidden="true">1.</strong> Introduction to Design for Lean Six Sigma</a></li><li class="chapter-item expanded "><a href="define/Introduction.html"><strong aria-hidden="true">2.</strong> Define Phase</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="define/Charter.html"><strong aria-hidden="true">2.1.</strong> Charter</a></li><li class="chapter-item expanded "><a href="define/MGPP.html"><strong aria-hidden="true">2.2.</strong> MGPP</a></li><li class="chapter-item expanded "><a href="define/Risk-Management.html"><strong aria-hidden="true">2.3.</strong> Risk Management</a></li><li class="chapter-item expanded "><a href="define/Communication-Plan.html"><strong aria-hidden="true">2.4.</strong> Communication Plan</a></li></ol></li><li class="chapter-item expanded "><a href="measure/Introduction.html"><strong aria-hidden="true">3.</strong> Measure Phase</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="measure/Voice-of-the-Customer.html"><strong aria-hidden="true">3.1.</strong> Voice of the Customer</a></li><li class="chapter-item expanded "><a href="measure/Quality-Function-Deployment.html"><strong aria-hidden="true">3.2.</strong> Quality Function Deployment</a></li><li class="chapter-item expanded "><a href="measure/Target-Costing.html"><strong aria-hidden="true">3.3.</strong> Target Costing</a></li><li class="chapter-item expanded "><a href="measure/Scorecards.html"><strong aria-hidden="true">3.4.</strong> Scorecards</a></li><li class="chapter-item expanded "><a href="measure/Intro-to-Minitab.html"><strong aria-hidden="true">3.5.</strong> Intro to Minitab</a></li><li class="chapter-item expanded "><a href="measure/Basic-Statistics.html"><strong aria-hidden="true">3.6.</strong> Basic Statistics</a></li><li class="chapter-item expanded "><a href="measure/Understanding-Variation-and-Control-Charts.html"><strong aria-hidden="true">3.7.</strong> Understanding Variation and Control Charts</a></li><li class="chapter-item expanded "><a href="measure/Measurement-Systems-Analysis.html"><strong aria-hidden="true">3.8.</strong> Measurement Systems Analysis</a></li><li class="chapter-item expanded "><a href="measure/Process-Capability.html"><strong aria-hidden="true">3.9.</strong> Process Capability</a></li></ol></li><li class="chapter-item expanded "><a href="explore/Introduction.html"><strong aria-hidden="true">4.</strong> Explore Phase</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="explore/Concept-Generation.html"><strong aria-hidden="true">4.1.</strong> Concept Generation</a></li><li class="chapter-item expanded "><a href="explore/TRIZ-for-New-Product-Design.html"><strong aria-hidden="true">4.2.</strong> TRIZ for New Product Design</a></li><li class="chapter-item expanded "><a href="explore/Transactional-TRIZ.html"><strong aria-hidden="true">4.3.</strong> Transactional TRIZ</a></li><li class="chapter-item expanded "><a href="explore/Concept-Selection-Pugh-and-AHP.html"><strong aria-hidden="true">4.4.</strong> Concept Selection – Pugh and AHP</a></li><li class="chapter-item expanded "><a href="explore/Statistical-Tolerance-Design.html"><strong aria-hidden="true">4.5.</strong> Statistical Tolerance Design</a></li><li class="chapter-item expanded "><a href="explore/Monte-Carlo-Simulation.html"><strong aria-hidden="true">4.6.</strong> Monte Carlo Simulation</a></li><li class="chapter-item expanded "><a href="explore/Hypothesis-Testing.html"><strong aria-hidden="true">4.7.</strong> Hypothesis Testing</a></li><li class="chapter-item expanded "><a href="explore/Confidence-Intervals.html"><strong aria-hidden="true">4.8.</strong> Confidence Intervals</a></li><li class="chapter-item expanded "><a href="explore/Testing-Means-Medians-and-Variances.html"><strong aria-hidden="true">4.9.</strong> Testing Means, Medians, and Variances</a></li><li class="chapter-item expanded "><a href="explore/Proportion-and-Chi-Square.html"><strong aria-hidden="true">4.10.</strong> Proportion and Chi-Square</a></li><li class="chapter-item expanded "><a href="explore/Simple-and-Multiple-Regression.html"><strong aria-hidden="true">4.11.</strong> Simple and Multiple Regression</a></li><li class="chapter-item expanded "><a href="explore/Multi-Vari-Analysis.html"><strong aria-hidden="true">4.12.</strong> Multi-Vari Analysis</a></li><li class="chapter-item expanded "><a href="explore/Design-FMEA.html"><strong aria-hidden="true">4.13.</strong> Design FMEA</a></li></ol></li><li class="chapter-item expanded "><a href="develop/Introduction.html"><strong aria-hidden="true">5.</strong> Develop Phase</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="develop/Detailed-Design.html"><strong aria-hidden="true">5.1.</strong> Detailed Design</a></li><li class="chapter-item expanded "><a href="develop/2-Way-ANOVA.html"><strong aria-hidden="true">5.2.</strong> 2-Way ANOVA</a></li><li class="chapter-item expanded "><a href="develop/Intro-to-Design-of-Experiments.html"><strong aria-hidden="true">5.3.</strong> Intro to Design of Experiments</a></li><li class="chapter-item expanded "><a href="develop/Full-Factorial-DOE.html"><strong aria-hidden="true">5.4.</strong> Full-Factorial DOE</a></li><li class="chapter-item expanded "><a href="develop/Fractional-Factorial-DOE.html"><strong aria-hidden="true">5.5.</strong> Fractional Factorial DOE</a></li><li class="chapter-item expanded "><a href="develop/DOE-Catapult-Simulation.html"><strong aria-hidden="true">5.6.</strong> DOE Catapult Simulation</a></li><li class="chapter-item expanded "><a href="develop/Key-Lean-Concepts.html"><strong aria-hidden="true">5.7.</strong> Key Lean Concepts</a></li><li class="chapter-item expanded "><a href="develop/Lean-Design.html"><strong aria-hidden="true">5.8.</strong> Lean Design</a></li><li class="chapter-item expanded "><a href="develop/Design-for-Manufacture-and-Assembly.html"><strong aria-hidden="true">5.9.</strong> Design for Manufacture and Assembly</a></li><li class="chapter-item expanded "><a href="develop/Intro-to-Reliability.html"><strong aria-hidden="true">5.10.</strong> Intro to Reliability</a></li><li class="chapter-item expanded "><a href="develop/Design-of-Experiments-with-Curvature.html"><strong aria-hidden="true">5.11.</strong> Design of Experiments with Curvature</a></li><li class="chapter-item expanded "><a href="develop/Conjoint-Analysis.html"><strong aria-hidden="true">5.12.</strong> Conjoint Analysis</a></li><li class="chapter-item expanded "><a href="develop/Mixture-Designs.html"><strong aria-hidden="true">5.13.</strong> Mixture Designs</a></li><li class="chapter-item expanded "><a href="develop/Robust-Design.html"><strong aria-hidden="true">5.14.</strong> Robust Design</a></li><li class="chapter-item expanded "><a href="develop/Helicopter-RSM-Simulation.html"><strong aria-hidden="true">5.15.</strong> Helicopter RSM Simulation</a></li></ol></li><li class="chapter-item expanded "><a href="implement/Introduction.html"><strong aria-hidden="true">6.</strong> Implement Phase</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="implement/Prototype-and-Pilot.html"><strong aria-hidden="true">6.1.</strong> Prototype and Pilot</a></li><li class="chapter-item expanded "><a href="implement/Process-Control.html"><strong aria-hidden="true">6.2.</strong> Process Control</a></li><li class="chapter-item expanded "><a href="implement/Implementation-Planning.html"><strong aria-hidden="true">6.3.</strong> Implementation Planning</a></li></ol></li><li class="chapter-item expanded "><a href="DMEDI-Capstone.html"><strong aria-hidden="true">7.</strong> DMEDI Capstone</a></li></ol>';
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
