import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';

import styles from './index.module.css';

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero hero--primary', styles.heroBanner)}>
      <div className="container">
        <Heading as="h1" className="hero__title">
          {siteConfig.title}
        </Heading>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <div className={styles.buttons}>
          <Link
            className="button button--secondary button--lg"
            to="/docs/guides/getting-started">
            Get Started - 5min ⏱️
          </Link>
        </div>
      </div>
    </header>
  );
}

export default function Home(): JSX.Element {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout
      title={`${siteConfig.title} Documentation`}
      description="Universal RDF Framework for Knowledge Graph Construction">
      <HomepageHeader />
      <main>
        <section className={styles.features}>
          <div className="container">
            <div className="row">
              <div className="col col--4">
                <h3>🔮 Latest Versions</h3>
                <p>
                  Built with React 19, Docusaurus 3.6+, and all the latest dependencies.
                  Production-ready and fully tested.
                </p>
              </div>
              <div className="col col--4">
                <h3>📚 Comprehensive Docs</h3>
                <p>
                  250,000+ lines of documentation, 61 KGC-4D concept files,
                  complete API reference for all 18 packages.
                </p>
              </div>
              <div className="col col--4">
                <h3>🎮 Interactive Demos</h3>
                <p>
                  Live playgrounds for KGC-4D visualization and 40+ React hooks.
                  Try features directly in your browser.
                </p>
              </div>
            </div>
          </div>
        </section>
      </main>
    </Layout>
  );
}
