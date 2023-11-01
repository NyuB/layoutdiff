build: build/index.html

build/index.html: src/*.elm
	elm make --output build/index.html src/Main.elm

test: src/*.elm tests/*.elm
	elm-test --seed 336948560956134 --fuzz 100

fmt: src/*.elm tests/*.elm
	elm-format --yes src
	elm-format --yes tests