.PHONY: clean install test

curseless: src/*.lisp
	./build.sh

test:
	./test.sh

clean:
	rm -rf curseless

install: curseless
	test -n "$(BINDIR)"  # $$BINDIR
	cp curseless ${BINDIR}
