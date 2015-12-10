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
	_R_CHECK_CRAN_INCOMING_=FALSE make check_all

check_all: build
	R CMD check --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

autodoc:
	${RSCRIPT} autodoc.R process

vignettes/storr.Rmd: vignettes/src/storr.R
	${RSCRIPT} -e 'library(sowsear); sowsear("$<", output="$@")'

vignettes: vignettes/storr.Rmd
	${RSCRIPT} -e 'library(methods); devtools::build_vignettes()'

staticdocs:
	@mkdir -p inst/staticdocs
	Rscript -e "library(methods); staticdocs::build_site()"
	rm -f vignettes/*.html
	@rmdir inst/staticdocs
website: staticdocs
	./update_web.sh

# No real targets!
.PHONY: all test document install vignettes
