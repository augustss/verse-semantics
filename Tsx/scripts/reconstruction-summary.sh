#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
RESET='\033[0m'

show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --parseset        Test parseset files (default)"
    echo "  --real-world      Test real-world Verse files"
    echo "  --both           Test both parseset and real-world files"
    echo "  --help           Show this help message"
    echo ""
}

test_parseset() {
    echo -e "${BOLD}Testing Parseset Files${RESET}"
    echo "=================================================="
    echo

    local total_files=0
    local perfect_files=0

    for file in tests/*.parseset; do
        if [[ -f "$file" ]]; then
            echo -e "${BLUE}Testing: $(basename $file)${RESET}"
            local result=$(node scripts/test-runner.js --reconstruct --quiet "$file" 2>&1 | grep "RECONSTRUCTION")
            echo "  $result"
            total_files=$((total_files + 1))
            if [[ $result == *"100.0% perfect matches"* ]]; then
                perfect_files=$((perfect_files + 1))
            fi
        fi
    done

    echo
    echo -e "${BOLD}Parseset Summary:${RESET}"
    echo "----------------"
    echo "Files tested: $total_files"
    echo "Perfect reconstruction: $perfect_files/$total_files"

    # Overall results
    echo
    echo -e "${BOLD}Overall Parseset Results:${RESET}"
    node scripts/test-runner.js --reconstruct --quiet tests/ 2>&1 | tail -3
}

test_real_world() {
    echo -e "${BOLD}Testing Real-World Verse Files${RESET}"
    echo "=================================================="
    echo

    if [[ ! -d "verse-files-flat" ]]; then
        echo -e "${RED}Error: verse-files-flat directory not found${RESET}"
        echo "Please ensure real-world Verse files are available."
        return 1
    fi

    # Count files
    local file_count=$(find verse-files-flat -name "*.verse" | wc -l | tr -d ' ')
    echo "Found $file_count real-world Verse files"
    echo

    if command -v node scripts/test-verse-files.js >/dev/null 2>&1; then
        echo "Running reconstruction analysis..."
        node scripts/test-verse-files.js --reconstruct --summary
    else
        echo -e "${YELLOW}Warning: test-verse-files.js not found, using alternative method${RESET}"
        echo "Please run: node scripts/test-verse-files.js --reconstruct --summary"
    fi
}

# Parse command line arguments
case "${1:-}" in
    --real-world)
        test_real_world
        ;;
    --both)
        test_parseset
        echo
        echo
        test_real_world
        ;;
    --help)
        show_help
        ;;
    --parseset|"")
        test_parseset
        ;;
    *)
        echo -e "${RED}Unknown option: $1${RESET}"
        show_help
        exit 1
        ;;
esac