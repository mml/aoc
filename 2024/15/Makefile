export CHEZSCHEMELIBDIRS=../lib::.
SCHEME=/opt/chez/bin/scheme
INPUTS=test0.ss test.ss input.ss
WIDE_INPUTS=wide-test0.ss wide-test.ss wide-test2.ss wide-input.ss

%.ss: %.txt pre.pl
	./pre.pl $(patsubst %.txt,%,$<) < $< > $@

wide-%.ss: %.txt widen.pl pre.pl
	./widen.pl < $< | ./pre.pl $(patsubst %.txt,%,$<) > $@

test0: $(WIDE_INPUTS)
	$(SCHEME) --program puz2.ss wide-test2.ss

test1: $(INPUTS)
	$(SCHEME) --program puz1.ss test.ss

puz1: $(INPUTS)
	$(SCHEME) --program puz1.ss input.ss

debug1: $(INPUTS)
	$(SCHEME) --debug-on-exception puz1.ss

fast1: build/whole-puz1.so $(INPUTS)
	$(SCHEME) --program $< input.ss

test2: $(WIDE_INPUTS)
	$(SCHEME) --program puz2.ss wide-test.ss

puz2: $(WIDE_INPUTS)
	$(SCHEME) --program puz2.ss wide-input.ss

debug2: $(WIDE_INPUTS)
	$(SCHEME) --debug-on-exception puz2.ss

fast2: build/whole-puz2.so $(WIDE_INPUTS)
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
