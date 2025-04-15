PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

all: install

test:
	${RSCRIPT} -e 'library(methods); devtools::test()'

test_all:
	STORR_RUN_LONG_TESTS=true make test

roxygen:
	@mkdir -p man
	${RSCRIPT} -e "library(methods); devtools::document()"

install:
	R CMD INSTALL .

build:
	R CMD build .

check:
	_R_CHECK_CRAN_INCOMING_=FALSE make check_all

check_all: build
	${RSCRIPT} -e "rcmdcheck::rcmdcheck(args = c('--as-cran', '--no-manual'))"

autodoc:
	${RSCRIPT} autodoc.R process

vignettes: vignettes/storr.Rmd vignettes/external.Rmd
	${RSCRIPT} -e 'library(methods); devtools::build_vignettes()'

pkgdown:
	${RSCRIPT} -e "library(methods); pkgdown::build_site()"

website: pkgdown
	./update_web.sh

tests/testthat/base64_reference.csv: scripts/build_base64_reference.R
	$<

# No real targets!
.PHONY: all test document install vignettes
