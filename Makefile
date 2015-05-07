PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

all: install

test:
	STORR_SKIP_DOWNLOADS=true make test_all

test_all:
	${RSCRIPT} -e 'library(methods); devtools::test()'

roxygen:
	@mkdir -p man
	${RSCRIPT} -e "library(methods); devtools::document()"

install:
	R CMD INSTALL .

build:
	R CMD build .

check:
	STORR_SKIP_DOWNLOADS=true make check_all

check_all: build
	_R_CHECK_CRAN_INCOMING_=FALSE R CMD check --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

vignettes/storr.Rmd: vignettes/storr_src.R
	${RSCRIPT} -e 'sowsear::sowsear("vignettes/storr_src.R", "Rmd")'
	mv vignettes/storr_src.Rmd vignettes/storr.Rmd

vignettes: vignettes/storr.Rmd
	${RSCRIPT} -e 'library(methods); devtools::build_vignettes()'
	mv inst/doc/storr_src.R vignettes/storr_src.R

# No real targets!
.PHONY: all test document install vignettes
