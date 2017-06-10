.PHONY: default test

default: test

test:
	# STACK_YAML=stack-7.8.yaml stack test
	STACK_YAML=stack-7.10.yaml stack test
	STACK_YAML=stack-8.0.yaml stack test



# HC=../ghc/ghc/stage2-inplace/ghc.exe
# PKG=../ghc/utils/ghc-pkg/install-inplace/bin/ghc-pkg.exe 
# SETUP=./Setup.exe
#
# main: build
#
# Setup.exe: Setup.hs
# 	$(HC) --make $<
#
# configure:
# 	$(SETUP) configure --with-compiler=$(HC) --with-hc-pkg=$(PKG) --user
#
# build:
# 	$(SETUP) build
#
# install:
# 	$(SETUP) install
#
# test:
# 	./dist/build/test/test.exe
#
# clean:
# 	$(SETUP)
