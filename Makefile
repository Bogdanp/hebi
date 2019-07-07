Hebi.zip: Hebi.app
	zip -r hebi.zip dist

Hebi.app: build
	raco distribute dist Hebi.app

build: hebi.rkt
	raco exe --gui -o Hebi.app hebi.rkt
