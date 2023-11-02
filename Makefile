default: fmt test build

build: page/standalone/index.html page/embedded/main.js

page/standalone/index.html: src/*.elm
	elm make --output page/standalone/index.html src/Main.elm

page/embedded/main.js: src/*.elm
	elm make --output page/embedded/main.js src/Main.elm

test: src/*.elm tests/*.elm
	elm-test --seed 336948560956134 --fuzz 100

fmt: src/*.elm tests/*.elm
	elm-format --yes src
	elm-format --yes tests