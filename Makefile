COURS_SRC=$(wildcard src/cours/*.md)
COURS_PDFS := $(patsubst src/cours/%.md,PDFs/cours/%.pdf,$(COURS_SRC))

all: $(COURS_PDFS)

PDFs/cours/%.pdf: src/cours/%.md
	pandoc -t beamer --pdf-engine=xelatex --template=./default.beamer --highlight-style=zenburn -s -V theme=metropolis -V fontsize=9pt -V monofont="DejaVu Sans Mono" -V monofontoptions=Scale=0.9 $< -o $@

clean:
	rm -rf PDFs/cours/*
