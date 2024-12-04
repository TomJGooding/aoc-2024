.PHONY: test
test: main example.txt
	./main example.txt

.PHONY: solve
solve: main input.txt
	./main input.txt

main: main.go
	go build -o main main.go
