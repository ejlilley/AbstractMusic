music_tests: Tests.hs
	mkdir -p build
	ghc Tests.hs -odir build/ -outputdir build/ -o music_tests

readlily: readlily.hs Music.hs LilyParse.hs LilyConvert.hs Tuning.hs
	mkdir -p build
	ghc readlily.hs -odir build/ -outputdir build/ -o readlily

per-tonos: per-tonos.hs
	mkdir -p build
	ghc per-tonos.hs -odir build/ -outputdir build/ -o per-tonos
