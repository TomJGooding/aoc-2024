CXXFLAGS=-Wall -Wextra

.PHONY: test
test: main example.txt
	./main example.txt

.PHONY: solve
solve: main input.txt
	./main input.txt

main: main.cpp
	$(CXX) $(CXXFLAGS) -o main main.cpp
