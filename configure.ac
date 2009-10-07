dnl $Id$

dnl ------------------------------------------------------------------
dnl Autoconf initialisation.
dnl ------------------------------------------------------------------

AC_INIT([translib], [0.2.1], [christopher.faulet@capflam.org], [translib])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([ac-aux])

AM_INIT_AUTOMAKE([foreign])

AC_CANONICAL_HOST

AC_PREREQ([2.60])
AC_REVISION([$Revision$])

dnl ------------------------------------------------------------------
dnl Prepare fancy console output, taken from GNU shtools.
dnl ------------------------------------------------------------------

term_bold=''
term_norm=''

case $TERM in
xterm|xterm*|vt220|vt220*)
	dnl For the most important terminal types we directly know the
	dnl sequences.
	term_bold=`awk 'BEGIN { printf("%c%c%c%c", 27, 91, 49, 109); }' </dev/null 2>/dev/null`
	term_norm=`awk 'BEGIN { printf("%c%c%c", 27, 91, 109); }' </dev/null 2>/dev/null`
	;;
vt100|vt100*|cygwin)
	term_bold=`awk 'BEGIN { printf("%c%c%c%c%c%c", 27, 91, 49, 109, 0, 0); }' </dev/null 2>/dev/null`
	term_norm=`awk 'BEGIN { printf("%c%c%c%c%c", 27, 91, 109, 0, 0); }' </dev/null 2>/dev/null`
	;;
* )
	dnl For all others, we try to use a possibly existing `tput' or
	dnl `tcout' utility.
	paths=`echo $PATH | sed -e 's/:/ /g'`
	for tool in tput tcout; do
		for dir in $paths; do
			if test -r "$dir/$tool" ; then
				for seq in bold md smso; do
					bold="`$dir/$tool $seq 2>/dev/null`"
					if test ".$bold" != . ; then
						term_bold="$bold"
						break
					fi
				done
				if test ".$term_bold" != . ; then
					for seq in sgr0 me rmso init reset; do
						norm="`$dir/$tool $seq 2>/dev/null`"
						if test ".$norm" != . ; then
							term_norm="$norm"
							break
						fi
					done
				fi
				break
			fi
		done
		if test ".$term_bold" != . && test ".$term_norm" != . ; then
			break;
		fi
	done
	;;
esac

bold () {
        text=`echo $seo "$1" | sed -e "s/%B/${term_bold}/g" -e "s/%b/${term_norm}/g" 2>/dev/null`
        echo $seo "$text"
}


dnl ------------------------------------------------------------------
dnl Compilateur/outils
dnl ------------------------------------------------------------------

AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_SED



dnl ------------------------------------------------------------------
dnl Options.
dnl ------------------------------------------------------------------

append_to_EMKOPTS () {
	if test -z "[$]EMKOPTS"; then
		EMKOPTS="[$]1"
	else
		EMKOPTS="[$]{EMKOPTS% }, [$]1"
	fi
}

expand_var () {
	local v=`eval echo '$'[$]1`
	while test "`echo [$]v | grep [[$]] > /dev/null && echo nok`"; do
		v=`eval echo [$]v`
	done
	echo [$]v
}

AC_ARG_ENABLE([debug],
	AC_HELP_STRING([--enable-debug],
	    [turn on debugging [[default=auto]]]),,
	enable_debug=$default_enable_debug)
if test "x${enable_debug}" = "xyes"; then
	append_to_EMKOPTS "debug_info"
	append_to_EMKOPTS "{d, debug}"
fi

dnl Print any warnings.
CFLAGS="${CFLAGS% } -Wall"
append_to_EMKOPTS "report_warnings"
append_to_EMKOPTS "{warn_format, 1}"
append_to_EMKOPTS "warn_export_vars"
append_to_EMKOPTS "warn_shadow_vars"
append_to_EMKOPTS "warn_unused_import"


dnl Documentation.
AC_ARG_ENABLE([documentation],
	AC_HELP_STRING([--enable-documentation],
	    [build documentation [[default=yes]]]),,
	enable_documentation="yes")
AM_CONDITIONAL(ENABLE_DOCUMENTATION, test "x${enable_documentation}" = "xyes")

dnl ------------------------------------------------------------------
dnl Erlang environment.
dnl ------------------------------------------------------------------

echo
bold "%BErlang environment%b"

dnl erl(1) is used to compile Erlang modules (regardless of the host).
#AC_ERLANG_PATH_ERL
#AC_ERLANG_PATH_ERLC

AC_ARG_WITH([erlang],
        AC_HELP_STRING([--with-erlang=PREFIX (optional)],
            [prefix where erlang is installed]),,
        with_erlang="")

if test "x${with_erlang}" = "x"; then
        AC_ERLANG_PATH_ERL
        AC_ERLANG_PATH_ERLC
        AC_ERLANG_SUBST_ROOT_DIR
        AC_ERLANG_SUBST_LIB_DIR
else
        AC_ERLANG_PATH_ERL([], [${with_erlang}/bin])
        AC_ERLANG_PATH_ERLC([], [${with_erlang}/bin])
        ERLANG_ROOT_DIR="${with_erlang}"
        ERLANG_LIB_DIR="${ERLANG_ROOT_DIR}/lib"
        AC_SUBST(ERLANG_ROOT_DIR)
        AC_SUBST(ERLANG_LIB_DIR)
fi


AC_ERLANG_CHECK_LIB([kernel], [], [AC_MSG_ERROR([kernel was not found!])])
AC_ERLANG_CHECK_LIB([stdlib], [], [AC_MSG_ERROR([stdlib was not found!])])
AC_ERLANG_CHECK_LIB([syntax_tools], [], [AC_MSG_ERROR([syntax_tools was not found!])])
AC_ERLANG_CHECK_LIB([eunit], [], [AC_MSG_WARN([eunit was not found!])])


dnl Determine directories for installation.
if test "x${prefix}" = "xNONE" -a "x${ERLANG_INSTALL_LIB_DIR}" = "x"; then
	# Inside Erlang lib dir
	ERLANG_INSTALL_LIB_DIR="${ERLANG_LIB_DIR}"
elif test "x${prefix}" != "xNONE"; then
	# Under $prefix
	ERLANG_INSTALL_LIB_DIR="${prefix}/lib"
fi

AC_ERLANG_SUBST_INSTALL_LIB_DIR
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(translib, AC_PACKAGE_VERSION)


dnl escript(1) is used by the testsuite.
AC_ARG_VAR([ESCRIPT], [Erlang/OTP interpreter command [autodetected]])

dnl Check for escript(1).
dnl Should we look first In Erlang's ROOT directory?
dnl     Eg: [${ERLANG_ROOT_DIR}/bin${PATH_SEPARATOR}${PATH}]
if test "x${ESCRIPT}" = "x"; then
        AC_PATH_PROG([ESCRIPT], [escript], [],
                     [${ERLANG_ROOT_DIR}/bin${PATH_SEPARATOR}${PATH}])
fi

dnl Do we have all the tools for the testsuite to be used.
if test "x${ERLANG_LIB_DIR_eunit}" != "xnot found" -a "x${ESCRIPT}" != "x"; then
    AC_MSG_WARN([${ERLANG_LIB_DIR_eunit}])
	enable_testsuite="yes"
else
	enable_testsuite="no"
fi
AM_CONDITIONAL([ENABLE_TESTSUITE], [test "x${enable_testsuite}" = "xyes"])


AC_SUBST(EMKOPTS)
dnl ------------------------------------------------------------------
dnl Autoconf output.
dnl ------------------------------------------------------------------

echo
AC_CONFIG_FILES([
    include/Makefile
    src/Makefile
    src/Emakefile
    ebin/Makefile
    ebin/translib.app
    doc/Makefile
    doc/edoc-info
	testsuite/Makefile
	Makefile
])

if test "x${enable_testsuite}" = "xyes"; then
   AC_CONFIG_FILES([
        testsuite/etest
   ], [chmod +x testsuite/etest])
fi

AC_OUTPUT

dnl --------------------------------------------------
dnl Configuration report
dnl --------------------------------------------------

echo
bold " %B== translib AC_PACKAGE_VERSION ==%b"
echo
echo 'Configuration:'
echo '    Prefix:                          '${prefix}
echo '    ${ERLANG_INSTALL_LIB_DIR}:       '${ERLANG_INSTALL_LIB_DIR}
echo '    Application dir.:                '${ERLANG_INSTALL_LIB_DIR_translib}
echo
echo '    Erlang emulator:                 '${ERL}
echo '    Erlang compiler:                 '${ERLC}
echo '    Erlang interpreter:              '${ESCRIPT}
echo
echo '    Testsuite:                       '${enable_testsuite}
echo '    Documentation:                   '${enable_documentation}
echo
echo
