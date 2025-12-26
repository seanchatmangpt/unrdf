#!/bin/bash
# Interactive Package Development
# Select a package and start development mode

set -e

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}📦 UNRDF Package Development${NC}\n"

# List all packages
PACKAGES=($(ls packages/ 2>/dev/null || echo ""))

if [ ${#PACKAGES[@]} -eq 0 ]; then
  echo -e "${RED}❌ No packages found in packages/ directory${NC}"
  exit 1
fi

echo -e "${YELLOW}Select a package to work on:${NC}\n"

# Display packages with numbers
for i in "${!PACKAGES[@]}"; do
  PACKAGE="${PACKAGES[$i]}"
  if [ -f "packages/$PACKAGE/package.json" ]; then
    VERSION=$(grep '"version"' "packages/$PACKAGE/package.json" | head -1 | sed 's/.*: "\(.*\)".*/\1/')
    echo -e "  ${GREEN}$((i+1)))${NC} $PACKAGE ${BLUE}(v$VERSION)${NC}"
  fi
done

echo ""
read -p "Enter package number (1-${#PACKAGES[@]}): " SELECTION

# Validate selection
if ! [[ "$SELECTION" =~ ^[0-9]+$ ]] || [ "$SELECTION" -lt 1 ] || [ "$SELECTION" -gt ${#PACKAGES[@]} ]; then
  echo -e "${RED}❌ Invalid selection${NC}"
  exit 1
fi

# Get selected package
PACKAGE="${PACKAGES[$((SELECTION-1))]}"
PACKAGE_DIR="packages/$PACKAGE"

echo -e "\n${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${GREEN}📦 Working on: $PACKAGE${NC}"
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

# Show package info
if [ -f "$PACKAGE_DIR/package.json" ]; then
  echo -e "${BLUE}Package Information:${NC}"
  echo -e "  Location: ${YELLOW}$PACKAGE_DIR${NC}"
  echo -e "  Version:  ${YELLOW}$(grep '"version"' "$PACKAGE_DIR/package.json" | head -1 | sed 's/.*: "\(.*\)".*/\1/')${NC}"
  echo -e "  Main:     ${YELLOW}$(grep '"main"' "$PACKAGE_DIR/package.json" | head -1 | sed 's/.*: "\(.*\)".*/\1/' || echo 'N/A')${NC}"
  echo ""
fi

# Development mode options
echo -e "${BLUE}Development Mode Options:${NC}\n"
echo -e "  ${GREEN}1)${NC} Watch mode (auto-rebuild on changes)"
echo -e "  ${GREEN}2)${NC} Test watch mode (auto-test on changes)"
echo -e "  ${GREEN}3)${NC} Build once"
echo -e "  ${GREEN}4)${NC} Test once"
echo -e "  ${GREEN}5)${NC} Open in editor (code .)"
echo -e "  ${GREEN}6)${NC} Show file structure"
echo ""
read -p "Select option (1-6): " MODE

cd "$PACKAGE_DIR"

case $MODE in
  1)
    echo -e "\n${YELLOW}Starting watch mode...${NC}"
    pnpm dev || pnpm build --watch || {
      echo -e "${RED}❌ Watch mode not available. Building once...${NC}"
      pnpm build
    }
    ;;
  2)
    echo -e "\n${YELLOW}Starting test watch mode...${NC}"
    pnpm test:watch || pnpm vitest watch
    ;;
  3)
    echo -e "\n${YELLOW}Building package...${NC}"
    time pnpm build
    echo -e "${GREEN}✅ Build complete${NC}"
    ;;
  4)
    echo -e "\n${YELLOW}Running tests...${NC}"
    time pnpm test
    ;;
  5)
    echo -e "\n${YELLOW}Opening in VS Code...${NC}"
    code .
    ;;
  6)
    echo -e "\n${YELLOW}File structure:${NC}"
    tree -L 2 -I node_modules || find . -type f -not -path "*/node_modules/*" | head -20
    ;;
  *)
    echo -e "${RED}Invalid option${NC}"
    exit 1
    ;;
esac
