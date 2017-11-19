SHELL   := bash
PY      ?= python3
ME      := koneko

.PHONY: all test test_verbose coverage clean publish

all:
	$(PY) setup.py sdist bdist_wheel

test:
	$(PY) -m$(ME) --test

test_verbose:
	$(PY) -m$(ME) --test -v

coverage:
	$(PY) -mcoverage run -m $(ME)
	$(PY) -mcoverage html

clean:
	rm -fr .coverage build/ dist/ htmlcov/ $(ME).egg-info/
	find -name '*.pyc' -delete
	find -name __pycache__ -delete

publish: clean all
	read -r -p "Are you sure? "; \
	[[ "$$REPLY" =~ [Yy]* ]] && twine upload dist/*

# TODO: gpg --detach-sign -a dist/package-1.0.1.tar.gz
