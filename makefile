all: points masses graphobj testuniformcentered testgraphs testalign example

points: points.ml 
	ocamlbuild points.byte

masses: masses.ml 
	ocamlbuild masses.byte

graphobj: graphobj.ml
	ocamlbuild graphobj.byte

testuniformcentered: testuniformcentered.ml
	ocamlbuild testuniformcentered.byte

testgraphs: testgraphs.ml
	ocamlbuild testgraphs.byte

testalign: testalign.ml
	ocamlbuild testalign.byte

example: example.ml
	ocamlbuild example.byte