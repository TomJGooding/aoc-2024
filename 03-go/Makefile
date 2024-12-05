.PHONY: test
test: main example-01.txt example-02.txt
	./main example-01.txt
	./main example-02.txt

.PHONY: solve
solve: main input.txt
	./main input.txt

main: main.go
	go build -o main main.go
