.PHONY: test
test: main.jar example.txt
	java -jar main.jar example.txt

.PHONY: solve
solve: main.jar input.txt
	java -jar main.jar input.txt

main.jar: main.kt
	kotlinc main.kt -include-runtime -d main.jar
