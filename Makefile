default: fmt test build

build: page/main.js

page/main.js: src/*.elm
	elm make --output page/main.js src/Main.elm

test: src/*.elm tests/*.elm
	elm-test --seed 336948560956134 --fuzz 100

fmt: src/*.elm tests/*.elm
	elm-format --yes src
	elm-format --yes tests
	tidy -modify -indent -quiet --tidy-mark no page/index.html