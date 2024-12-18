.PHONY: test
test: day_09.beam example.txt
	erl -noshell -run day_09 main example.txt

.PHONY: solve
solve: day_09.beam input.txt
	erl -noshell -run day_09 main input.txt

day_09.beam: day_09.erl
	erlc day_09.erl
