.PHONY: default test test-7.10 test-8.0

default: test

test: test-7.10

test-7.10:
	stack --stack-yaml=stack-7.10.yaml test

# This fails, but we keep it around if we ever want to fix it.
test-8.0:
	stack --stack-yaml=stack-8.0.yaml test --executable-profiling --test-arguments="+RTS -xc -RTS"
