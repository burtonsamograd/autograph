all: autograph test

autograph: autograph.lisp build.sh
	./build.sh

test: autograph
	./autograph example.ag > example.css

clean:
	rm -rf autograph quicklisp* *~ example.css
