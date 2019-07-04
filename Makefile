all: EarTraining.jsexe

clean:
	rm -r obj/* EarTraining.jsexe

EarTraining.jsexe: EarTraining.hs JSMIDI.hs
	ghcjs -O -hidir obj -odir obj `for p in .cabal-sandbox/*-packages.conf.d; do echo "-package-db=$$p"; done` --make EarTraining
