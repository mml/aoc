%1.ss: %.txt part1.pl
	./part1.pl < $< > $@

test1: test1.ss
	scheme --script $<

run1: input1.ss
	scheme --script $<
