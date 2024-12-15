export CHEZSCHEMELIBDIRS=../lib::.
SCHEME=/opt/chez/bin/scheme
INPUTS=test0.ss test.ss input.ss

%.ss: %.txt pre.pl
	./pre.pl $(patsubst %.txt,%,$<) < $< > $@

test0: $(INPUTS)
	$(SCHEME) --program puz1.ss test0.ss

test1: $(INPUTS)
	$(SCHEME) --program puz1.ss test.ss

puz1: $(INPUTS)
	$(SCHEME) --program puz1.ss input.ss

debug1:
	$(SCHEME) --debug-on-exception puz1.ss

fast1: build/whole-puz1.so
	$(SCHEME) --program $< input.ss

test2:
	$(SCHEME) --program puz2.ss test.txt

puz2:
	$(SCHEME) --program puz2.ss input.txt

debug2:
	$(SCHEME) --debug-on-exception puz2.ss

fast2: build/whole-puz2.so
	$(SCHEME) --program $< input.txt

## helpers for compilation

build:
	mkdir -p build

clean:
	rm -rf build

.PRECIOUS: build/%.wpo
build/%.wpo: %.ss build
	echo '(require-nongenerative-clause #t) (compile-imported-libraries #t) (generate-wpo-files #t) (compile-program "$<" "$(patsubst %.wpo,%.so,$@))' | $(SCHEME) -q --optimize-level 3

build/%.so: %.ss build
	echo '(require-nongenerative-clause #t) (compile-imported-libraries #t) (generate-wpo-files #t) (compile-program "$<" "$@")' | $(SCHEME) -q --optimize-level 3

build/whole-%.so: build/%.wpo build
	echo '(compile-whole-program "$<" "$@")' | $(SCHEME) -q --optimize-level 3
