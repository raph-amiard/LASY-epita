all: PDFs/1_introduction_to_ada.pdf PDFs/1_introduction_to_ada_TP.pdf PDFs/2_introduction_to_rust.pdf PDFs/2_introduction_to_rust_TP.pdf PDFs/3_introduction_to_ada_2.pdf PDFs/3_introduction_to_ada_2_TP.pdf PDFs/4_introduction_to_rust_2.pdf

PDFs/1_introduction_to_ada.pdf: src/1_introduction_to_ada.md
	pandoc -t beamer --pdf-engine=xelatex --template=./default.beamer --highlight-style=zenburn -s -V theme=metropolis -V fontsize=9pt -V monofont="DejaVu Sans Mono" -V monofontoptions=Scale=0.9 src/1_introduction_to_ada.md -o PDFs/1_introduction_to_ada.pdf


PDFs/1_introduction_to_ada_TP.pdf: src/1_introduction_to_ada_TP.md
	pandoc --pdf-engine=xelatex src/1_introduction_to_ada_TP.md -o PDFs/1_introduction_to_ada_TP.pdf

PDFs/2_introduction_to_rust.pdf: src/2_introduction_to_rust.md
	pandoc -t beamer --pdf-engine=xelatex --template=./default.beamer --highlight-style=zenburn -s -V theme=metropolis -V fontsize=9pt -V monofont="DejaVu Sans Mono" -V monofontoptions=Scale=0.9 src/2_introduction_to_rust.md -o PDFs/2_introduction_to_rust.pdf

PDFs/2_introduction_to_rust_TP.pdf: src/2_introduction_to_rust_TP.md
	pandoc --pdf-engine=xelatex src/2_introduction_to_rust_TP.md -o PDFs/2_introduction_to_rust_TP.pdf

PDFs/3_introduction_to_ada_2.pdf: src/3_introduction_to_ada_2.md
	pandoc -t beamer --pdf-engine=xelatex --template=./default.beamer --highlight-style=zenburn -s -V theme=metropolis -V fontsize=9pt -V monofont="DejaVu Sans Mono" -V monofontoptions=Scale=0.9 src/3_introduction_to_ada_2.md -o PDFs/3_introduction_to_ada_2.pdf

PDFs/3_introduction_to_ada_2_TP.pdf: src/3_introduction_to_ada_2_TP.md
	pandoc src/3_introduction_to_ada_2_TP.md -o PDFs/3_introduction_to_ada_2_TP.pdf

PDFs/4_introduction_to_rust_2.pdf: src/4_introduction_to_rust_2.md
	pandoc -t beamer --pdf-engine=xelatex --template=./default.beamer --highlight-style=zenburn -s -V theme=metropolis -V fontsize=9pt -V monofont="DejaVu Sans Mono" -V monofontoptions=Scale=0.9 src/4_introduction_to_rust_2.md -o PDFs/4_introduction_to_rust_2.pdf

clean:
	rm -rf PDFs/*
