export CHEZSCHEMELIBDIRS=../..::.
SCHEME=/opt/chez/bin/scheme

%.wpo: %.ss
	echo '(compile-imported-libraries #t) (generate-wpo-files #t) (compile-program "$<")' | $(SCHEME) -q --optimize-level 3

%.so: %.ss
	echo '(compile-imported-libraries #t) (generate-wpo-files #t) (compile-program "$<")' | $(SCHEME) -q --optimize-level 3

test1:
	$(SCHEME) --program puz1.ss test.txt

puz1:
	$(SCHEME) --program puz1.ss input.txt

whole-%.so: %.wpo
	echo '(compile-whole-program "$<" "$@")' | $(SCHEME) -q --optimize-level 3

fast1: whole-puz1.so puz1.wpo
	$(SCHEME) --program $<

clean:
	rm -f *.so *.wpo
