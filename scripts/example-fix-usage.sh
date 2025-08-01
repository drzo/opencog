#!/bin/bash
#
# Example usage of OpenCog Fixing Tools
# =====================================
#
# This script demonstrates how to use the fixing tools to resolve
# various issues in the OpenCog project.

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

echo -e "${BLUE}╔═══════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║        OpenCog Fixing Tools - Example Usage               ║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════════════════════════╝${NC}"
echo

# Function to print section headers
print_section() {
    echo
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}▶ $1${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo
}

# Function to run command with nice output
run_command() {
    local description="$1"
    local command="$2"
    
    echo -e "${YELLOW}→ $description${NC}"
    echo -e "${BLUE}  Command: $command${NC}"
    echo
    
    if eval "$command"; then
        echo -e "${GREEN}  ✓ Success${NC}"
    else
        echo -e "${RED}  ✗ Failed${NC}"
        return 1
    fi
}

# Change to project root
cd "$PROJECT_ROOT"

# Example 1: Dry run to see what would be fixed
print_section "Example 1: Dry Run Analysis"
echo "First, let's see what issues exist without making any changes:"
run_command "Analyzing all issues (dry run)" \
    "python scripts/fix-all.py --dry-run"

# Example 2: Fix only CMake issues
print_section "Example 2: Fix CMake Issues"
echo "Fix only CMake-related issues:"
run_command "Analyzing CMake files" \
    "python scripts/fix-cmake.py --analyze"

read -p "Press Enter to apply CMake fixes (or Ctrl+C to skip)..."
run_command "Applying CMake fixes" \
    "python scripts/fix-cmake.py --fix"

# Example 3: Fix a specific test
print_section "Example 3: Fix Specific Test"
echo "Fix a specific failing test (GhostSyntaxUTest):"
run_command "Fixing GhostSyntaxUTest" \
    "python scripts/fix-tests.py --test GhostSyntaxUTest --dry-run"

# Example 4: Fix Scheme/Guile issues
print_section "Example 4: Fix Scheme Test Issues"
echo "Fix Scheme-specific test issues:"
if command -v guile &> /dev/null; then
    run_command "Analyzing Scheme tests" \
        "guile scripts/fix-scheme-tests.scm --test-dir tests/ghost --verbose"
else
    echo -e "${YELLOW}  ⚠ Guile not found, skipping Scheme fixes${NC}"
fi

# Example 5: Interactive mode
print_section "Example 5: Interactive Mode"
echo "Run in interactive mode to approve each fix:"
echo -e "${YELLOW}This would normally prompt for each fix:${NC}"
echo "  python scripts/fix-all.py --interactive --type test"
echo -e "${BLUE}(Skipping actual execution for demo)${NC}"

# Example 6: Fix everything with verification
print_section "Example 6: Complete Fix with Verification"
echo "Fix all issues and verify the results:"
echo -e "${YELLOW}This is the recommended approach for a complete fix:${NC}"
echo "  python scripts/fix-all.py --verify"
echo
read -p "Run complete fix? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    run_command "Fixing all issues with verification" \
        "python scripts/fix-all.py --verify"
else
    echo -e "${BLUE}Skipped complete fix${NC}"
fi

# Show generated reports
print_section "Generated Reports"
echo "The following reports have been generated:"
echo

if [ -f "fix-summary-report.md" ]; then
    echo -e "${GREEN}✓ fix-summary-report.md${NC}"
    echo "  Overall summary of all fixes applied"
fi

if [ -f "test-fixing-report.md" ]; then
    echo -e "${GREEN}✓ test-fixing-report.md${NC}"
    echo "  Detailed test fixing information"
fi

if [ -d "fix-logs" ]; then
    echo -e "${GREEN}✓ fix-logs/${NC}"
    echo "  Detailed logs from each fixer"
    echo "  Files: $(ls -1 fix-logs | wc -l)"
fi

if [ -d ".fix-backups" ]; then
    echo -e "${GREEN}✓ .fix-backups/${NC}"
    echo "  Backup files before modifications"
    echo "  Files: $(ls -1 .fix-backups | wc -l)"
fi

# Tips section
print_section "Tips and Best Practices"
echo "1. Always run with --dry-run first to preview changes"
echo "2. Use --interactive mode for careful review of each fix"
echo "3. Run --verify after fixes to ensure everything builds"
echo "4. Check the generated reports for detailed information"
echo "5. Backup files are saved in .fix-backups/ directory"
echo "6. Logs are saved in fix-logs/ for debugging"
echo
echo -e "${YELLOW}Pro tip:${NC} For CI/CD integration, use:"
echo "  python scripts/fix-all.py --dry-run || echo 'Issues found'"
echo

# Summary
print_section "Summary"
echo -e "${GREEN}The OpenCog Fixing Tools provide:${NC}"
echo "  • Automated detection of common issues"
echo "  • Safe fixing with backup and rollback"
echo "  • Detailed reporting and logging"
echo "  • Support for multiple languages (C++, Python, Scheme)"
echo "  • Integration with CMake and test frameworks"
echo
echo -e "${BLUE}For more information, see: scripts/README-FIXING-TOOLS.md${NC}"
echo

# Cleanup option
echo
read -p "Clean up generated files? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "Cleaning up..."
    rm -f fix-summary-report.md test-fixing-report.md
    rm -rf fix-logs .fix-backups
    echo -e "${GREEN}✓ Cleanup complete${NC}"
fi

echo
echo -e "${GREEN}═══════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}Example usage demonstration complete!${NC}"
echo -e "${GREEN}═══════════════════════════════════════════════════════════${NC}" 