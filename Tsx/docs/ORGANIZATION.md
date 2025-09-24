# Project Organization

This document describes the organization of the Verse Parser project after cleanup.

## Directory Structure

```
verse-parser/
├── src/                    # Source code
├── scripts/                # Production scripts
├── tests/                  # Parseset test files
├── docs/                   # Important documentation
├── archive/                # Archived experimental work
├── verse-files-flat/       # Real-world Verse files for testing
├── dist/                   # Compiled output
└── node_modules/           # Dependencies
```

## Key Files

### Root Level
- `README.md` - Main project documentation
- `package.json` - Project configuration
- `tsconfig.json` - TypeScript configuration
- `.gitignore` - Git ignore patterns

### Scripts (`scripts/`)
- `test-runner.js` - Main test runner for parseset files
- `compare-reconstruction.js` - Side-by-side reconstruction comparison
- `reconstruction-analysis.js` - Detailed reconstruction analysis for real-world files
- `reconstruction-summary.sh` - Quick reconstruction overview
- `test-verse-files.js` - Real-world Verse file testing
- `enhanced-test-runner.js` - Enhanced test runner with additional features
- `run-final-tests.js` - Final test suite runner

### Documentation (`docs/`)
- `GRAMMAR.md` - Verse grammar specification
- `DELIMITERS.md` - Token delimiter documentation

### Archive (`archive/`)
- `docs/` - Experimental documentation (12 files)
- `experimental-scripts/` - Development/debug scripts (49 files)

## Cleanup Summary

### Moved to Archive
**Documentation (12 files):**
- Implementation summaries and roadmaps
- Architecture documentation
- Development progress reports
- Analysis documents

**Scripts (49 files):**
- Debug and tracing scripts (`debug-*.js`, `trace-*.js`)
- Test files (`test-*.js`, `test-*.ts`)
- Experimental reconstructors (`enhanced-reconstructor.js`, etc.)
- Benchmarking and profiling tools
- Validation and deployment test scripts

### Kept in Root/Scripts
**Production Scripts (8 files):**
- Core test runner and comparison tools
- Real-world file analysis tools
- Reconstruction testing utilities
- Summary and reporting scripts

**Essential Documentation:**
- `README.md` - Complete project guide
- Grammar and delimiter specifications

## Benefits

1. **Clean Root Directory**: Only essential files remain
2. **Organized Scripts**: Production tools in `scripts/`, experimental work archived
3. **Preserved History**: All experimental work saved in `archive/`
4. **Clear Structure**: Easy to find production tools vs development experiments
5. **Maintainable**: Clear separation between stable tools and experimental work

The project now has a clean, professional structure suitable for production use while preserving all experimental work for reference.