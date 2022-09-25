.SILENT: test play docs bisect clean
.PHONY: test play docs bisect clean build

build:
	dune build

test:
	dune exec pacman-test

play:
	dune exec pacman

bisect:
	dune exec --instrument-with bisect_ppx pacman-test
	bisect-ppx-report html
	# open _coverage/index.html to see coverage info
	
docs: 
	dune build @doc
	dune build @doc-private

clean:
	find . -type f \( -iname \*.coverage \) -delete