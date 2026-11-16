import { chromium } from '@playwright/test';
import { spawn } from 'child_process';
import { resolve } from 'path';

const unrdfDir = '/Users/sac/unrdf';

async function runBenchmark() {
  console.log('Starting HTTP server...');
  const server = spawn('python3', ['-m', 'http.server', '8080'], { cwd: unrdfDir });

  // wait a second for server to start
  await new Promise(r => setTimeout(r, 1000));

  console.log('Launching browser...');
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();
  
  console.log('Navigating to demo...');
  await page.goto('http://localhost:8080/examples/browser-vanilla.html');
  
  console.log('Generating 10,000+ ontology nodes...');
  const turtleLines = [];
  for (let i = 0; i < 10000; i++) {
    turtleLines.push(`<http://example.org/subject${i}> <http://example.org/predicate> "value${i}" .`);
  }
  const turtleData = turtleLines.join('\n');
  
  await page.fill('#turtleInput', turtleData);
  
  console.log('Clicking parse and monitoring frame rate...');
  
  // Use requestAnimationFrame in the page to measure FPS
  await page.evaluate(() => {
    window.fpsMeasurements = [];
    let lastTime = performance.now();
    let frames = 0;
    
    function measure(time) {
      frames++;
      if (time - lastTime >= 1000) {
        window.fpsMeasurements.push(frames);
        frames = 0;
        lastTime = time;
      }
      if (window.keepMeasuring) {
        requestAnimationFrame(measure);
      }
    }
    
    window.keepMeasuring = true;
    requestAnimationFrame(measure);
  });
  
  // Set up dialog handler
  const dialogPromise = new Promise((resolve) => {
    page.on('dialog', async dialog => {
      console.log('Dialog:', dialog.message());
      await dialog.accept();
      resolve();
    });
  });

  await page.click('#btnParse');
  await dialogPromise;
  
  await page.evaluate(() => {
    window.keepMeasuring = false;
  });
  
  const fpsMeasurements = await page.evaluate(() => window.fpsMeasurements);
  console.log('FPS Measurements:', fpsMeasurements);
  
  let steady = true;
  for (const fps of fpsMeasurements) {
    if (fps < 30) {
      steady = false;
      console.warn('Frame rate dropped below 30 FPS:', fps);
    }
  }
  
  if (steady) {
    console.log('✅ Frame-rate held steady while parsing 10,000+ ontology nodes natively.');
  } else {
    console.log('❌ Frame-rate dropped during parsing.');
  }
  
  await browser.close();
  server.kill();
  process.exit(steady ? 0 : 1);
}

runBenchmark().catch(err => {
  console.error(err);
  process.exit(1);
});
