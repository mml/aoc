SCHEME=/opt/chez/bin/scheme

%.so: %.ss
	echo '(compile-program "$<")' | $(SCHEME) -q --optimize-level 3

test2:
	$(SCHEME) --program puz2.ss test.txt

puz2:
	$(SCHEME) --program puz2.ss input.txt

fast2: puz2.so
	$(SCHEME) --program $< input.txt

test1:
	$(SCHEME) --program puz1.ss test.txt

puz1:
	$(SCHEME) --program puz1.ss input.txt
