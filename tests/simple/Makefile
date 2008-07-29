# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

HPCDIR=$(TOP)/../utils/hpc
HPC   = $(HPCDIR)/hpc-std

build-tix-tar::
	rm -Rf .hpc hpc001.o a.out
	$(TOP)/../compiler/ghc-inplace -fhpc hpc001.hs
	./a.out
	mv a.out.tix hpc_sample.tix
	tar cf tixs.tar hpc_sample.tix .hpc

