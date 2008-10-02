# $Id$

EXTRA_DIST = 	gen_trans.erl 	\
				lambda_expr.erl \
				recfun.erl		\
				multiparser.erl

all-local:
	@$(ERL) -noshell -pa @top_builddir@/ebin	\
		-eval "case make:all() of				\
		         up_to_date -> halt(0);			\
		         Err ->							\
		           io:format(\"~p~n\", [Err]),	\
		           halt(1)						\
		       end"
