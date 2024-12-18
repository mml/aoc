export CHEZSCHEMELIBDIRS=~/.akku/lib::~/.akku/libobj:../lib::.
SCHEME=/opt/chez/bin/scheme

%.sexpr: %.txt pre.pl
	./pre.pl $< > $@

test2: test2.sexpr
	$(SCHEME) --program puz2.ss test2.sexpr

test1: test.sexpr
	$(SCHEME) --program puz1.ss test.sexpr

puz1: input.sexpr
	$(SCHEME) --program puz1.ss input.sexpr

debug1:
	$(SCHEME) --debug-on-exception puz1.ss

fast1: whole-puz1.so input.sexpr
	$(SCHEME) --program $< input.input.sexpr

puz2: input.sexpr
	$(SCHEME) --program puz2.ss input.sexpr

# Calculate all values of A that suffice
calc: input.sexpr
	./calc.ss

# Run the debugger
go: input.sexpr
	$(SCHEME) --program puz2.ss go

debug2:
	$(SCHEME) --debug-on-exception puz2.ss

fast2: whole-puz2.so
	$(SCHEME) --program $< input.txt

## helpers for compilation

build:
	mkdir -p build

clean:
	rm -rf build

.PRECIOUS: build/%.wpo
build/%.wpo: %.ss build
	echo '(require-nongenerative-clause #t) (compile-imported-libraries #t) (generate-wpo-files #t) (compile-program "$<")' | $(SCHEME) -q --optimize-level 3

build/%.so: %.ss build
	echo '(require-nongenerative-clause #t) (compile-imported-libraries #t) (generate-wpo-files #t) (compile-program "$<" "$@")' | $(SCHEME) -q --optimize-level 3

build/whole-%.so: build/%.wpo build
	echo '(compile-whole-program "$<" "$@")' | $(SCHEME) -q --optimize-level 3
