
const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const packages = execSync('find packages -name "package.json"').toString().trim().split('\n');

packages.forEach(pkg => {
  try {
    const pkgContent = JSON.parse(fs.readFileSync(pkg, 'utf8'));
    if (pkgContent.types) {
      const typesPath = path.resolve(path.dirname(pkg), pkgContent.types);
      if (!fs.existsSync(typesPath)) {
        console.error(`Error: Package ${pkg} has types field pointing to non-existent file: ${typesPath}`);
      }
    } else {
      // Optional: check if it should have types but doesn't
      // console.warn(`Warning: Package ${pkg} does not have a types field.`);
    }
  } catch (e) {
    console.error(`Error parsing ${pkg}: ${e.message}`);
  }
});
