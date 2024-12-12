export CHEZSCHEMELIBDIRS=../lib::.
SCHEME=/opt/chez/bin/scheme
TEST_CASE=test4.txt

test1:
	$(SCHEME) --debug-on-exception --program puz1.ss $(TEST_CASE)

puz1:
	$(SCHEME) --program puz1.ss input.txt

debug1:
	$(SCHEME) --debug-on-exception puz1.ss

fast1: whole-puz1.so
	$(SCHEME) --program $< input.txt

test2:
	$(SCHEME) --program puz2.ss $(TEST_CASE)

puz2:
	$(SCHEME) --program puz2.ss input.txt

debug2:
	$(SCHEME) --debug-on-exception puz2.ss

fast2: whole-puz2.so
	$(SCHEME) --program $< input.txt

## helpers for compilation

clean:
	rm -f *.so *.wpo

.PRECIOUS: %.wpo
%.wpo: %.ss
	echo '(generate-wpo-files #t) (compile-program "$<")' | $(SCHEME) -q --optimize-level 3

%.so: %.ss
	echo '(generate-wpo-files #t) (compile-program "$<")' | $(SCHEME) -q --optimize-level 3

whole-%.so: %.wpo
	echo '(compile-whole-program "$<" "$@")' | $(SCHEME) -q --optimize-level 3
