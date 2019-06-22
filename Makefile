all: EarTraining.jsexe

clean:
	rm -rf EarTraining.jsexe EarTraining.js_hi EarTraining.js_o JSMIDI.js_hi JSMIDI.js_o

EarTraining.jsexe: EarTraining.hs JSMIDI.hs
	ghcjs -O $<
