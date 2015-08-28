all: autograph

autograph: autograph.lisp build.sh
	./build.sh

clean:
	rm -rf autograph quicklisp* *~
