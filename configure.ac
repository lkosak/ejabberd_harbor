AC_INIT([ejabberd_harbor], [master], [lkosak@gmail.com])

AC_ERLANG_NEED_ERLC
AC_ERLANG_NEED_ERL

ERLCFLAGS="$ERLCFLAGS -I $prefix/lib/ejabberd/include\
 -pz $prefix/lib/ejabberd/ebin"
ERLFLAGS="$ERLFLAGS -pz $prefix/lib/ejabberd/ebin"

ERL_CHECK_HEADER(ejabberd.hrl, [],
			     [AC_MSG_ERROR([ejabberd.hrl is required.  Make sure PREFIX is the same as the PREFIX at which ejabberd is installed.])])

ERL_CHECK_HEADER(jlib.hrl, [],
			     [AC_MSG_ERROR([jlib.hrl is required.  Make sure PREFIX is the same as the PREFIX at which ejabberd is installed.])])

ERL_CHECK_MODULE(gen_mod, [],
		 [AC_MSG_ERROR([gen_mod module is required.  Make sure PREFIX is the same as the PREFIX at which ejabberd is installed.])])

AC_SUBST(ERLCFLAGS)
AC_CONFIG_FILES([Makefile src/mod_muc_harbor/Makefile])
AC_OUTPUT
