pandoc -t beamer --pdf-engine=xelatex --template=./default.beamer --highlight-style=zenburn -s -V theme=metropolis -V fontsize=9pt -V monofont="DejaVu Sans Mono" -V monofontoptions=Scale=0.9 1_introduction_to_ada.md -o 1_introduction_to_ada.pdf
pandoc --pdf-engine=xelatex 1_introduction_to_ada_TP.md -o 1_introduction_to_ada_TP.pdf
