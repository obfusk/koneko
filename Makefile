SHELL     := bash
TESTFILES := lib/*.knk README.md doc/*.md

RUN_HS_   := cabal v2-run koneko
RUN_HS    := $(RUN_HS_) --
RUN_JS    := node js/koneko

PRELUDE   := lib/prelude.knk
LIBS      := $(wildcard lib/*.knk)
MOD_DEFS  := __module-defs__ [ show say! ] each

export LC_ALL=C.UTF-8

.PHONY: test test_haskell doctest_hs doctest_knk_hs
.PHONY: test_node doctest_knk_js test_prim_bltn test_prld
.PHONY: cabal_build clean cleanup
.PHONY: repl_haskell repl_node repl_browser
.PHONY: link_vim_syntax copy_vim_syntax
.PHONY: html html_docs_vim html_index html_links

test: test_haskell test_node test_prim_bltn test_prld

test_haskell: doctest_hs doctest_knk_hs

doctest_hs:
	cabal v2-run doctests   # nicer output than v2-test

doctest_knk_hs:
	$(RUN_HS) --doctest $(TESTFILES)

test_node: doctest_knk_js

doctest_knk_js:
	@echo
	$(RUN_JS) --doctest $(TESTFILES)

test_prim_bltn:
	set -e; \
	for x in __prim__ __bltn__; do \
	  echo "$$x"; expr=":$$x"' $(MOD_DEFS)'; \
	  hs_list () { $(RUN_HS_) -v0 -- -e "$$expr"; }; \
	  hs_list | fmt -$${COLUMNS:-80}; hs_list | wc -l; \
	  diff -Naur <( hs_list ) <( $(RUN_JS) <<< "$$expr" ); \
	done

test_prld:
	set -e; \
	src_list () { \
	  grep -Eo '^\s{,4}:\S+' $(PRELUDE) | \
	    sed 's!\s*!!g' | grep -v ^:_ | sort -u; \
	}; \
	hs_list () { \
	  $(RUN_HS_) -v0 -- -e ':__prld__ $(MOD_DEFS)' | grep -v ^:_ | \
	    grep -Ev ':([A-Z].*\?$$|[~^][A-Z])'; \
	}; \
	hs_list | fmt -$${COLUMNS:-80}; hs_list | wc -l; \
	diff -Naur <( hs_list ) <( src_list )

cabal_build:
	if cabal v2-build --help | grep -q write-ghc-environment-files; then \
	  cabal v2-build --write-ghc-environment-files=always --enable-tests ; \
	else \
	  cabal v2-build --enable-tests ; \
	fi

clean:
	cabal v2-clean
	rm -f .ghc.environment.*
	[ ! -e dist ] || rmdir dist
	[ ! -e lib-doc ] || rm -r lib-doc

cleanup:
	find -name '*~' -delete -print

repl_haskell:
	rlwrap $(RUN_HS)

repl_node:
	$(RUN_JS)

repl_browser:
	cd js && python3 -m http.server

link_vim_syntax:
	mkdir -p ~/.vim/{ftdetect,ftplugin,syntax}
	for dir in {ftdetect,ftplugin,syntax}; do \
	  ln -vsr -t ~/.vim/$$dir vim/$$dir/koneko.vim ; \
	done

copy_vim_syntax:
	mkdir -p ~/.vim/{ftdetect,ftplugin,syntax}
	for dir in {ftdetect,ftplugin,syntax}; do \
	  cp -vi -t ~/.vim/$$dir vim/$$dir/koneko.vim ; \
	done

html: html_docs_vim html_index html_links

html_docs_vim: $(patsubst lib/%.knk,lib-doc/%.knk.html,$(LIBS))

lib-doc/%.knk.html: lib/%.knk
	mkdir -p lib-doc
	scripts/knk2html "$<" "$@"

html_index: $(patsubst lib/%.knk,lib-doc/%.knk.index.html,$(LIBS))

lib-doc/%.knk.index.html: lib-doc/%.knk.index.md
	pandoc -M title="Index of $$(basename "$<" .index.md)" \
	  -s -f gfm -t html "$<" > "$@"

lib-doc/%.knk.index.md: lib/%.knk
	mkdir -p lib-doc
	scripts/knk2index "$<" > "$@"

# TODO
html_links: $(LIBS)
	@echo TODO
