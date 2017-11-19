SHELL   := bash
PY      ?= python3
ME      := koneko

.PHONY: test test_verbose coverage clean package publish

test:
	$(PY) -m$(ME) --test

test_verbose:
	$(PY) -m$(ME) --test -v

coverage:
	$(PY) -mcoverage run -m $(ME) --test
	$(PY) -mcoverage html

clean:
	rm -fr .coverage build/ dist/ htmlcov/ $(ME).egg-info/
	find -name '*.pyc' -delete
	find -name __pycache__ -delete

package:
	$(PY) setup.py sdist bdist_wheel

publish: clean package
	read -r -p "Are you sure? "; \
	[[ "$$REPLY" =~ [Yy]* ]] && twine upload dist/*

# TODO: gpg --detach-sign -a dist/package-1.0.1.tar.gz
