
include ../vsn.mk

exec_MODULES = exec_app exec
TARGET_FILES = exec_app.beam exec.beam exec.app
src_FILES = exec_app.erl exec.erl

ERLCFLAGS = -I../include -DARCH='"@ARCH@"'

erl_modules = `echo $(exec_MODULES) | \
    sed -e 's/^\s*//' -e 's/\s*$$//' -e 's/\s/, /g'`

erl_src_list = `echo $(exec_MODULES) | \
    sed -e 's/\(\S\+\)/"\1.erl"/g' -e 's/^\s*/[/' -e 's/\s*$$/]/' -e 's/\(\s\+\)/, /g'`

SUFFIXES = .erl .beam .app.src .app

.erl.beam:
	$(ERLC) $(ERLCFLAGS) -o $@ $<

.app.src.app:
	modules=$(erl_modules); \
	cat $< |sed -e "s/%VSN%/$(VSN)/g" -e "s/%MODULES%/$$modules/g" >$@

install_sh = @install_sh@

# standard targets

all-local: $(TARGET_FILES)

clean-local:
	rm -fr $(TARGET_FILES)

install-exec-local:
	$(install_sh) -d $(ERLANG_INSTALL_LIB_DIR_exec)/ebin
	$(install_sh) -m 0644 -t $(ERLANG_INSTALL_LIB_DIR_exec)/ebin $(TARGET_FILES)
	$(install_sh) -d $(ERLANG_INSTALL_LIB_DIR_exec)/include
	$(install_sh) -m 0644 -t $(ERLANG_INSTALL_LIB_DIR_exec)/include ../include/exec.hrl
	$(install_sh) -d $(ERLANG_INSTALL_LIB_DIR_exec)/src
	$(install_sh) -m 0644 -t $(ERLANG_INSTALL_LIB_DIR_exec)/src $(src_FILES)
	$(install_sh) -d $(ERLANG_INSTALL_LIB_DIR_exec)/doc/html
	$(install_sh) -m 0644 -t $(ERLANG_INSTALL_LIB_DIR_exec)/doc/html ../doc/html/*

# edoc stuff...

HTML_DIR = ../doc/html
HTML_OPTS = [ \
    {def, {vsn, "$(VSN)"} }, {preprocess, true}, {includes, ["../include"]}, \
    {overview, "overview.edoc"}, {title, "The exec application"}, {application, 'exec'}, \
    {dir, "$(HTML_DIR)"} \
]

edoc:
	ERL_DOCS=$(erl_src_list); \
	$(ERL) -noshell -run edoc_run files "$$ERL_DOCS" '$(HTML_OPTS)'
	@if [ -f $(HTML_DIR)/overview.html ] && \
	    grep -q 'overview-summary.html' $(HTML_DIR)/index.html 2>&-; \
	then \
	    mv $(HTML_DIR)/overview.html $(HTML_DIR)/overview-summary.html ; \
	fi
