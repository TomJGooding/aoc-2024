.PHONY: test
test: main example.txt
	./main example.txt

.PHONY: solve
solve: main input.txt
	./main input.txt

main: main.ml
	ocamlc -o main main.ml
