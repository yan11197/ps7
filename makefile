all: points masses testuniformcentered graphobj

points: points.ml 
	ocamlbuild points.byte

masses: masses.ml 
	ocamlbuild masses.byte

testuniformcentered: testuniformcentered.ml
	ocamlbuild testuniformcentered.byte

graphobj: graphobj.ml
	ocamlbuild graphobj.byte