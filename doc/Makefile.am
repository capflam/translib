# $Id$

EXTRA_DIST = overview.edoc stylesheet.css

stylesheet.css:
	(test "$(srcdir)" != "." &&\
	 (test ! -e "$@" || test "$(srcdir)/$@" -nt "$@") &&\
	 cp "$(srcdir)/$@" "$@") || :

erlang.png:

ERL_SRCS = $(top_srcdir)/src/*.erl

HTML_FILES = 	index.html					\
		     	modules-frame.html			\
		     	overview-summary.html		\
	    		packages-frame.html			\
		     	gen_trans.html				\
				lambda_expr.html			\
				recfun.html					\
				clone_module.html			\
				transhell.html

$(HTML_FILES): overview.edoc $(ERL_SRCS) stylesheet.css
	$(ERL) -noshell \
		-run edoc_run application "'$(PACKAGE_NAME)'" '"$(top_srcdir)"' \
		'[{def, {vsn, "$(PACKAGE_VERSION)"}}, {dir, "."}, {stylesheet, "stylesheet.css"}, {preprocess, true}, no_packages]' \
		-s erlang halt
	@for f in $(HTML_FILES); do	\
		$(SED) -i.bak \
		-e 's|</title>|</title><meta http-equiv="Content-Type" content="text/html; charset=UTF-8">|' \
		-e 's|\xa9|\&copy;|g' $$f && \
		rm -f $$f.bak; \
	done

all-local: $(HTML_FILES)
	test -f $(top_builddir)/doc/stylesheet.css || cp $(top_srcdir)/doc/stylesheet.css $(top_builddir)/doc/stylesheet.css

clean-local:
	(test "$(srcdir)" != "." && rm -f stylesheet.css) || :

htmldocdir = $(ERLANG_INSTALL_LIB_DIR_translib)/doc/html

htmldoc_DATA = $(HTML_FILES) stylesheet.css erlang.png

CLEANFILES = $(HTML_FILES) edoc-info erlang.png
