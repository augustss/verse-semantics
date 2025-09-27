# Verse Language Documentation Site

Bits of a Verse documentation...

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


## Project Structure

```
Site/
├── docs/                    # Documentation source files
│   ├── index.md            # Home page
│   ├── effects.md          # Effects system documentation
│   ├── verse_reference.md  # Complete language reference
│   ├── VerseLanguageDoc.md # Additional language docs
│   ├── datatypes.md        # Data types reference
│   └── mutability.md       # Mutability guide
├── libs/                    # Custom Pygments library
│   └── pygments/
│       └── lexers/
│           └── verse.py    # Verse syntax highlighter
├── mkdocs.yml              # MkDocs configuration
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

