COURS_SRC=$(wildcard src/cours/*.md)
COURS_PDFS := $(patsubst src/cours/%.md,PDFs/cours/%.pdf,$(COURS_SRC))
TP_SRC=$(wildcard src/TP/*.md)
TP_PDFS := $(patsubst src/TP/%.md,PDFs/TP/%.pdf,$(TP_SRC))

all: $(COURS_PDFS) $(TP_PDFS)

PDFs/TP/%.pdf: src/TP/%.md
	pandoc $< -o $@

PDFs/cours/%.pdf: src/cours/%.md
	pandoc -t beamer --pdf-engine=xelatex --template=./default.beamer --highlight-style=zenburn -s -V theme=metropolis -V fontsize=9pt -V monofont="DejaVu Sans Mono" -V monofontoptions=Scale=0.9 $< -o $@

clean:
	rm -rf PDFs/cours/*
	rm -rf PDFs/TP/*
