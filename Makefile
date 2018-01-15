WEBPREFIX           = ~/inside-web

.PHONY: insideweb

insideweb:
	mkdir -p $(WEBPREFIX)
	cp web/inside.html $(WEBPREFIX)/.
	cp web/reloader.js $(WEBPREFIX)/.

inside:
	stack install

all: insideweb inside
