PREFIX    = ~/.local/bin

.PHONY: insideplot

insideplot:
	cp insideplot.sh $(PREFIX)/insideplot
	chmod +x $(PREFIX)/insideplot

inside:
	stack setup
	stack build
	stack install

all: insideplot inside
