WEBPREFIX           = ~/inside-web

.PHONY: insideweb

insideweb:
	cp web/inside.html $(WEBPREFIX)/.
	cp web/reloader.hs $(WEBPREFIX)/.

inside:
	stack install

all: insideweb inside
