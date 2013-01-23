AUTOMAKE_OPTIONS = subdir-objects

EJABBERD_PREFIX=$(libdir)/ejabberd

ESRCDIR=$(top_srcdir)/src
EBINDIR=$(top_builddir)/ebin

ERL=erl $(ERLCFLAGS) -pa ebin $(ERLFLAGS) -noshell -eval

mod_ejabberd_harbor_FILES = ejabberd_auth_harbor.erl harbor_odbc_queries.erl auth_harbor_test.erl
mod_ejabberd_harbor_BEAMS = ejabberd_auth_harbor.beam harbor_odbc_queries.beam auth_harbor_test.beam

EXTRA_DIST = ${addprefix $(ESRCDIR)/, $(mod_ejabberd_harbor_FILES)}

mod_ejabberd_harbordir = $(EJABBERD_PREFIX)/ebin
mod_ejabberd_harbor_DATA = ${addprefix $(EBINDIR)/, $(mod_ejabberd_harbor_BEAMS)}
CLEANFILES = $(mod_ejabberd_harbor_DATA)

test: all
	$(ERL) 'auth_harbor_test:test(), halt().'

$(EBINDIR)/%.beam: $(ESRCDIR)/%.erl
	$(ERLC) $(ERLCFLAGS) -b beam -o $(EBINDIR) $<
