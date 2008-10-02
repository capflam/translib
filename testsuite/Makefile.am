# $Id$

ETEST = $(top_builddir)/testsuite/etest

ERL_FILES = test_lambda_expr.erl	\
			test_recfun.erl			\
			test_multiparser.erl

REAL_TESTS = 	test_lambda_expr	\
				test_recfun			\
				test_multiparser

TESTS_ENVIRONMENT= ERL_COMPILER_OPTIONS="{chain, [lambda_expr, recfun]}"
TESTS = $(REAL_TESTS)

EXTRA_DIST = $(ERL_FILES)

CLEANFILES = $(TESTS) erl_crash.dump

$(TESTS): $(ETEST)
	@(test ! -e $@ -o $(ETEST) -nt $@ && echo "Create: $@" &&	\
	  cp $(ETEST) $@ && chmod 755 $@) || :
