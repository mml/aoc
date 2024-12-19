LIBDIRS=../lib::build:~/.akku/lib::~/.akku/libobj
#SCHEME=/opt/chez/bin/scheme
SCHEME=scheme
RUN_SCHEME=$(SCHEME) --libdirs $(LIBDIRS)

test1:
	$(RUN_SCHEME) --program puz1.ss test.txt

puz1:
	$(RUN_SCHEME) --program puz1.ss input.txt

debug1:
	$(RUN_SCHEME) --debug-on-exception puz1.ss

fast1: build/whole-puz1.so
	$(RUN_SCHEME) --program $< input.txt

test2:
	$(RUN_SCHEME) --program puz2.ss test.txt

puz2:
	$(RUN_SCHEME) --program puz2.ss input.txt

debug2:
	$(RUN_SCHEME) --debug-on-exception puz2.ss

fast2: build/whole-puz2.so
	$(RUN_SCHEME) --program $< input.txt

## helpers for compilation

build:
	mkdir -p build

clean:
	rm -rf build

.PRECIOUS: build/%.wpo
build/%.wpo: %.ss build
	echo '(require-nongenerative-clause #t) \
		(compile-imported-libraries #t) \
		(generate-wpo-files #t) \
		(compile-program "$<" "$(patsubst %.wpo,%.so,$@)")' |\
		$(RUN_SCHEME) -q --optimize-level 3

build/%.so: %.ss build
	echo '(require-nongenerative-clause #t) \
		(compile-imported-libraries #t) \
		(generate-wpo-files #t) \
		(compile-program "$<" "$@")' |\
		$(RUN_SCHEME) -q --optimize-level 3

build/whole-%.so: build/%.wpo build
	echo '(compile-whole-program "$<" "$@")' |\
		$(RUN_SCHEME) -q --optimize-level 3
