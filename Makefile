music_tests: Tests.hs
	mkdir -p build
	ghc Tests.hs -odir build/ -outputdir build/ -o music_tests

readlily: readlily.hs Music.hs LilyParse.hs LilyConvert.hs Tuning.hs
	mkdir -p build
	ghc readlily.hs -odir build/ -outputdir build/ -o readlily
