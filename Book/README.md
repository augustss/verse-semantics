# Verse Language Documentation Site

Bits of a Verse documentation...

## Quick Start

```bash
# Start the development server
mkdocs serve
```

Then open http://localhost:8000 in your browser.

### Build Static Site

```bash
# Build the documentation site
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
- MkDocs with Material theme (installed via pipx or pip)
- Pygments for syntax highlighting

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

### Port Already in Use

```bash
# Kill existing MkDocs server
pkill -f "mkdocs serve"

# Or use different port
mkdocs serve -a localhost:8001
```

## Contributing

1. Edit documentation files in `docs/`
2. Test locally with `mkdocs serve`
3. Build and verify with `mkdocs build`

