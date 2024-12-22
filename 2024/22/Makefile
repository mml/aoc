CFLAGS += -Wall
LIBDIRS=../lib::build:~/.akku/lib::~/.akku/libobj
SCHEME=scheme
RUN_SCHEME=$(SCHEME) --libdirs $(LIBDIRS)

test1: puz1
	./$< < test.txt

run1: puz1
	./$< < input.txt

test2:
	$(RUN_SCHEME) --program puz2.ss < test2.txt

puz2::
	$(RUN_SCHEME) --program puz2.ss < input.txt

fast2: build/whole-puz2.so
	$(SCHEME) --program $< < input.txt

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
