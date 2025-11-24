# Verse Language Documentation

Comprehensive documentation for the Verse programming language, built with MkDocs and deployed to GitHub Pages.

## Quick Start

### Setup and Build

```bash
./setup_and_build.sh
```

This installs all dependencies and builds the documentation.

### Local Development

```bash
source venv/bin/activate
mkdocs serve
```

Open http://localhost:8000 in your browser.

### Build Static Site

```bash
source venv/bin/activate
mkdocs build
```

Output is generated in `site/` directory.

## Manual Setup

If you prefer manual installation:

```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python install_verse_lexer.py
mkdocs build
```

## GitHub Pages Deployment

The site auto-deploys via GitHub Actions when you push to main/master.

Site URL: `https://YOUR_USERNAME.github.io/verse-paper/`

To enable:
1. Go to repository Settings → Pages
2. Set Source to **GitHub Actions**

## Requirements

- Python 3.8 or higher
- MkDocs with Material theme
- Pygments for syntax highlighting

All dependencies are in `requirements.txt`.
