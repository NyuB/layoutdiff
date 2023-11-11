default: fmt test review-check build

build: page/main.js

page/main.js: src/*.elm
	elm make --output page/main.js src/Main.elm

review-fix:
	elm-review --fix

review-check:
	elm-review

test: src/*.elm tests/*.elm
	elm-test --seed 336948560956134 --fuzz 100

fmt: src/*.elm tests/*.elm page/index.html
	elm-format --yes src
	elm-format --yes tests
	elm-format --yes review/src
	tidy -modify -indent -quiet --tidy-mark no page/index.html

fmt-check: src/*.elm tests/*.elm
	elm-format --validate src
	elm-format --validate tests
	elm-format --validate review/src
