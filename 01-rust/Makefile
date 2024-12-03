.PHONY: test
test: main example.txt
	./main example.txt

.PHONY: solve
solve: main input.txt
	./main input.txt

main: main.rs
	rustc main.rs
