#!/usr/bin/env python3
"""
Install the Verse lexer directly into Pygments lexers directory
"""
import os
import shutil
import sys
from pathlib import Path

def install_verse_lexer():
    # Find the pygments installation
    try:
        import pygments
        pygments_path = Path(pygments.__file__).parent
    except ImportError:
        print("ERROR: Pygments is not installed")
        return False

    # Path to our verse lexer
    verse_lexer_source = Path("libs/pygments/lexers/verse.py")

    if not verse_lexer_source.exists():
        print(f"ERROR: Verse lexer not found at {verse_lexer_source}")
        return False

    # Destination in pygments lexers directory
    lexers_dir = pygments_path / "lexers"
    verse_lexer_dest = lexers_dir / "verse.py"

    # Copy the lexer file
    print(f"Copying verse.py to {verse_lexer_dest}")
    shutil.copy2(verse_lexer_source, verse_lexer_dest)

    # Update the __init__.py file to include our lexer
    init_file = lexers_dir / "__init__.py"

    # Read the current content
    with open(init_file, 'r') as f:
        content = f.read()

    # Check if already registered
    if 'VerseLexer' not in content:
        print("Registering VerseLexer in Pygments...")

        # Find the LEXERS dictionary in __init__.py
        import_line = "from pygments.lexers.verse import VerseLexer\n"

        # Add import at the top with other imports
        if "from pygments.lexers." in content:
            # Find a good place to insert the import
            lines = content.split('\n')
            for i, line in enumerate(lines):
                if line.startswith('from pygments.lexers.') and 'import' in line:
                    # Insert after this line
                    lines.insert(i + 1, import_line.strip())
                    content = '\n'.join(lines)
                    break

        # Write back
        with open(init_file, 'w') as f:
            f.write(content)

    print("✓ Verse lexer installed successfully!")

    # Test the installation
    try:
        from pygments.lexers import get_lexer_by_name
        lexer = get_lexer_by_name('verse')
        print("✓ Verse lexer is working!")
        return True
    except Exception as e:
        print(f"WARNING: Could not load verse lexer: {e}")
        print("You may need to manually register it or restart Python")
        return True

if __name__ == "__main__":
    success = install_verse_lexer()
    sys.exit(0 if success else 1)