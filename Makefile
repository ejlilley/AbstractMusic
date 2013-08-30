music_tests: Tests.hs
	mkdir -p build
	ghc Tests.hs -odir build/ -outputdir build/ -o music_tests
