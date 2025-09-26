from setuptools import setup, find_packages

setup(
    name="pygments-verse",
    version="0.1.0",
    description="Pygments lexer for the Verse programming language",
    author="Verse Documentation Team",
    packages=find_packages(),
    install_requires=[
        "pygments>=2.0",
    ],
    entry_points={
        "pygments.lexers": [
            "verse = pygments.lexers.verse:VerseLexer",
        ],
    },
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: BSD License",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Topic :: Text Processing :: Markup",
    ],
)