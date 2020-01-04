SHELL     := bash
TESTFILES := lib/*.knk README.md doc/*.md

.PHONY: test test_haskell doctest_hs doctest_knk_hs test_node doctest_knk_js
.PHONY: cabal_build clean cleanup repl_haskell repl_node repl_browser
.PHONY: link_vim_syntax copy_vim_syntax
.PHONY: html_docs_vim

test: test_haskell test_node

test_haskell: doctest_hs doctest_knk_hs

doctest_hs:
	cabal v2-run doctests   # nicer output than v2-test

doctest_knk_hs:
	cabal v2-run koneko -- --doctest $(TESTFILES)

test_node: doctest_knk_js

doctest_knk_js:
	@echo
	node js/koneko --doctest $(TESTFILES)

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
	rlwrap cabal v2-run koneko --

repl_node:
	node js/koneko

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

html_docs_vim: $(patsubst lib/%.knk,lib-doc/%.knk.html,$(wildcard lib/*.knk))

lib-doc/%.knk.html: lib/%.knk
	mkdir -p lib-doc
	vim -RE +'let g:html_dynamic_folds=1' +TOhtml +'w! $@' +'qa!' "$<"
	sed -i -r \
	  -e $$'/id=\'fold2\'/! s!class=\'closed-fold\'!class=\'open-fold\'!' \
	  -e $$'s!.*DOCTYPE.*!<\!DOCTYPE html>!' \
	  -e $$'/^body/ s!\}!text-align: center; }!' \
	  -e $$'/^pre/ s!\}!text-align: left; display: inline-block; }!' \
	  -e $$'s!.*<title>.*!<title>$(notdir $<)</title>!' \
	  -e $$'s!^\\*.*!html { font-size: 1.2em; }!' \
	  "$@"
