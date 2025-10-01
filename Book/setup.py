#!/usr/bin/env python3
"""
Setup script for Verse lexer Pygments plugin
"""
from setuptools import setup, find_packages

setup(
    name='verse-lexer',
    version='1.0.0',
    packages=['libs.pygments.lexers'],
    package_dir={'': '.'},
    entry_points={
        'pygments.lexers': [
            'verse = libs.pygments.lexers.verse:VerseLexer',
        ],
    },
    install_requires=[
        'pygments>=2.16',
    ],
    description='Verse language lexer for Pygments',
    author='Verse Documentation Team',
    python_requires='>=3.8',
)