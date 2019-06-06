all: EarTraining.jsexe

EarTraining.jsexe: EarTraining.hs
	ghcjs -O $<
