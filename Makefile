all: EarTraining.jsexe

clean:
	rm -rf EarTraining.jsexe EarTraining.js_hi EarTraining.js_o JSMIDI.js_hi JSMIDI.js_o

EarTraining.jsexe: EarTraining.hs JSMIDI.hs
	ghcjs -O `for p in .cabal-sandbox/*-packages.conf.d; do echo "-package-db=$$p"; done` --make EarTraining
