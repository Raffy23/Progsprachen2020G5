#!/bin/env bash

pandoc README.md --pdf-engine=xelatex -o README.pdf
pandoc calculator/README.md --pdf-engine=xelatex -o calculator/README.pdf
pandoc editor/README.md --pdf-engine=xelatex -o editor/README.pdf
pandoc programming_language/README.md --pdf-engine=xelatex -o programming_language/README.pdf

