hebi.zip: dist
	zip -r hebi.zip dist

dist: Hebi.app
	raco distribute dist Hebi.app

Hebi.app: hebi.rkt
	raco exe --gui -o Hebi.app hebi.rkt

.PHONY: clean
clean:
	rm -r Hebi.app dist hebi.zip
