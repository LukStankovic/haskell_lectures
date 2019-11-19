DOT = dot

GHC = ghc
GHCFLAGS = -odir obj -hidir obj
GHCFLAGS += -Wall

GHCFLAGS	+= \
	-hide-all-packages \
	-package HUnit \
	-package base \
	-package QuickCheck \
	-package test-framework \
	-package test-framework-hunit \
	-package test-framework-quickcheck2 \

#
# To support cabal sandbox
#
ifneq ($(wildcard .cabal-sandbox/*-packages.conf.d),)
GHCFLAGS += \
	-no-user-package-db \
	-package-db $(wildcard .cabal-sandbox/*-packages.conf.d)
endif

SRC = $(wildcard *.hs)

.PHONY : all
all : fsm

.PHONY : clean
clean :
	rm -rf fsm match HW06Tests obj match.c *.dot *.pdf

.PHONY : figs
figs : epsilon.pdf a.pdf ab.pdf a-or-b.pdf ab-star.pdf

.PHONY : test
test : HW06Tests
	./HW06Tests

fsm : $(SRC)
	$(GHC) $(GHCFLAGS) -o $@ --make Main.hs

HW06Tests : $(SRC)
	$(GHC) $(GHCFLAGS) -o $@ --make HW06Tests.hs

match : main.c match.c
	gcc -o $@ $^

%.pdf : %.dot
	$(DOT) -Tpdf -o $@ $< || rm -f $@

%-nfa.dot %-dfa.dot %-min-dfa.dot : %.txt fsm
	./fsm --nfa-dot=$*-nfa.dot --dfa-dot=$*-dfa.dot --min-dfa-dot=$*-min-dfa.dot $(shell cat $<)

epsilon.dot : fsm
	./fsm --nfa-dot=$@ ""

a.dot : fsm
	./fsm --nfa-dot=$@ "a"

ab.dot : fsm
	./fsm --nfa-dot=$@ "ab"

a-or-b.dot : fsm
	./fsm --nfa-dot=$@ "a|b"

ab-star.dot : fsm
	./fsm --nfa-dot=$@ "(ab)*"
