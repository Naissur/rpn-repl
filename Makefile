all:
	mkdir -p dist/ && cd src/ && ghc Main.hs -outputdir ../dist -o ../dist/RPN
