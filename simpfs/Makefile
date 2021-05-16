proj=simpfs

$(proj): Main.hs SimpfsEncoder.hs SimpfsDecoder.hs SimpNode.hs
	ghc -dynamic -Wname-shadowing $< -o simpfs

.PHONY=clean
clean:
	rm -rf *.hi *.ho *.o $(proj)
