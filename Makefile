SHELL     := bash
TESTFILES := lib/*.knk README.md doc/*.md
LIBS      := $(wildcard lib/*.knk)

.PHONY: test test_haskell doctest_hs doctest_knk_hs
.PHONY: test_node doctest_knk_js test_diff
.PHONY: cabal_build clean cleanup
.PHONY: repl_haskell repl_node repl_browser
.PHONY: link_vim_syntax copy_vim_syntax
.PHONY: html html_docs_vim html_index html_links

test: test_haskell test_node test_diff

test_haskell: doctest_hs doctest_knk_hs

doctest_hs:
	cabal v2-run doctests   # nicer output than v2-test

doctest_knk_hs:
	cabal v2-run koneko -- --doctest $(TESTFILES)

test_node: doctest_knk_js

doctest_knk_js:
	@echo
	node js/koneko --doctest $(TESTFILES)

test_diff:
	scripts/test-diff

cabal_build:
	build="cabal v2-build" env=write-ghc-environment-files; \
	if $$build --help | grep -q $$env; then \
	  $$build --$$env=always --enable-tests; \
	else \
	  $$build --enable-tests; \
	fi

clean:
	cabal v2-clean
	rm -f .ghc.environment.*
	[ ! -e dist ] || rmdir dist
	[ ! -e lib-doc ] || rm -r lib-doc

cleanup:
	find -name '*~' -delete -print

repl_haskell:
	scripts/repl_hs

repl_node:
	scripts/repl_js

repl_browser:
	cd js && python3 -m http.server

link_vim_syntax:
	scripts/copy-or-link-vim-syntax link

copy_vim_syntax:
	scripts/copy-or-link-vim-syntax copy

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
