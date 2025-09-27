# Verse Language Documentation

Comprehensive documentation for the Verse programming language, built with MkDocs and deployable to GitHub Pages.

## Installation

### Automated Setup (Recommended)

The easiest way to get started is using the automated setup script:

```bash
# Make the script executable (first time only)
chmod +x setup_and_build.sh

# Run the setup script
./setup_and_build.sh
```

This script will:
- Check for Python 3.8+ and install it if missing
- Create a Python virtual environment
- Install all required dependencies (MkDocs, Material theme, Pygments)
- Install the custom Verse syntax highlighter
- Build the documentation site

### Manual Setup

If you prefer to set up manually:

```bash
# Create virtual environment
python3 -m venv venv

# Activate virtual environment
source venv/bin/activate  # On macOS/Linux
# or
venv\Scripts\activate     # On Windows

# Install dependencies
pip install mkdocs mkdocs-material pygments

# Install custom Verse syntax highlighter
python install_verse_lexer.py

# Build the documentation
mkdocs build
```

## Quick Start

After installation, you can:

```bash
# Start the development server
source venv/bin/activate
mkdocs serve
```

Then open http://localhost:8000 in your browser.

### Build Static Site

```bash
# Build the documentation site
source venv/bin/activate
mkdocs build
```

The static site will be generated in the `site/` directory.


## GitHub Pages Deployment

This documentation can be automatically deployed to GitHub Pages:

### Setup Instructions

1. **Update Configuration**: Edit `mkdocs.yml` and replace `YOUR_USERNAME` with your GitHub username:
   ```yaml
   site_url: https://YOUR_USERNAME.github.io/verse-paper/
   repo_url: https://github.com/YOUR_USERNAME/verse-paper
   ```

2. **Enable GitHub Pages**:
   - Go to your repository Settings → Pages
   - Under "Build and deployment", select Source: **GitHub Actions**

3. **Push Changes**: The GitHub Action workflow will automatically deploy when you push to main/master.

The site will be available at: `https://YOUR_USERNAME.github.io/verse-paper/`

## Project Structure

```
Book/
├── docs/                    # Documentation source files
│   ├── index.md            # Documentation home
│   ├── 00_overview.md      # Language overview
│   ├── 01_builtins.md      # Built-in types
│   ├── 02_composites.md    # Classes, structs, interfaces
│   ├── 03_functions.md     # Functions
│   ├── 04_operators.md     # Operators
│   ├── 05_expressions.md   # Expression system
│   ├── 06_control.md       # Control flow
│   ├── 07_modules.md       # Modules and paths
│   ├── 08_failure.md       # Failure system
│   ├── 09_effects.md       # Effect system
│   ├── 10_concurrency.md   # Concurrency
│   ├── 11_mutability.md    # Mutability
│   ├── 12_types.md         # Type system
│   ├── 13_persistable.md   # Persistable types
│   ├── 14_access.md        # Access specifiers
│   ├── 15_evolution.md     # Code evolution
│   └── 16_grammar.md       # Language grammar
├── libs/                    # Custom Pygments library
│   └── pygments/
│       └── lexers/
│           └── verse.py    # Verse syntax highlighter
├── mkdocs.yml              # MkDocs configuration
├── requirements.txt        # Python dependencies
└── site/                   # Built documentation (generated)
```

## Features

### Verse Syntax Highlighting

The included Pygments lexer provides full syntax highlighting support for Verse code:

```verse
using { /Fortnite.com/Devices }

GameController := class<public>:
    var Score:int = 0

    UpdateScore(Points:int)<transacts>:void=
        set Score = Score + Points

    IsWinning()<decides>:logic=
        Score > 100
```


## Requirements

- Python 3.8 or higher
- MkDocs 1.6+ with Material theme
- Pygments 2.16+ for syntax highlighting

All dependencies are automatically installed by the setup script.

## Customization

### Theme Configuration

Edit `mkdocs.yml` to customize:
- Site name and metadata
- Color scheme and theme settings
- Navigation structure
- Plugin configuration

### Syntax Highlighting Style

The Pygments style can be changed in `mkdocs.yml`:

```yaml
markdown_extensions:
  - pymdownx.highlight:
      pygments_style: monokai  # Change style here
```

## Troubleshooting

### Installation Issues

If the setup script fails:

```bash
# Clean up and retry
rm -rf venv site
./setup_and_build.sh
```

### Port Already in Use

```bash
# Kill existing MkDocs server
pkill -f "mkdocs serve"

# Or use different port
venv/bin/mkdocs serve -a localhost:8001
```

### Verse Syntax Highlighting Not Working

If Verse code blocks aren't highlighted:

```bash
# Reinstall the Verse lexer
source venv/bin/activate
python install_verse_lexer.py
mkdocs build
```

## Contributing

1. Edit documentation files in `docs/`
2. Test locally with `mkdocs serve`
3. Build and verify with `mkdocs build`

