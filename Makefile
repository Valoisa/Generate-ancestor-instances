.PHONY: main.pdf all

all: main.pdf

main.pdf: main.tex
	pdflatex --file-line-error-style "main.tex"

