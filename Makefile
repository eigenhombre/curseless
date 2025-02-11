.PHONY: clean install test

rectumon: src/*.lisp
	./build.sh

test:
	./test.sh

clean:
	rm -rf rectumon

install: rectumon
	test -n "$(BINDIR)"  # $$BINDIR
	cp rectumon ${BINDIR}
