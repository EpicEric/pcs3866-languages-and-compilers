build/basicc: build basicc/*.pony
	ponyc -o build basicc --debug

build:
	mkdir build

clean:
	rm -rf build/

run: build/basicc
	build/basicc

.PHONY: all clean run
