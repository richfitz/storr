#!/usr/bin/env Rscript

## Dirty hack to compile docs in the absence of proper Roxygen R6 support.
devtools::load_all(".")

add_usage <- function(dat, object) {
  capture_usage <- function(name) {
    tmp <- capture.output(args(object[[name]]))
    tmp <- strip_trailing_whitespace(paste(tmp[-length(tmp)], collapse="\n"))
    sub("^function\\s*", name, tmp)
  }

  valid <- names(object)
  extra <- setdiff(names(dat), valid)
  if (length(extra) > 0L) {
    warning(sprintf("In '%s', extra methods: %s",
                    class(object)[[1]],
                    paste(extra, collapse=", ")),
            immediate.=TRUE, call.=FALSE)
  }

  for (name in names(dat)) {
    dat[[name]]$method_name <- name
    dat[[name]]$usage <- capture_usage(name)
    dat[[name]]$order <- names(formals(object[[name]]))
  }
  dat
}

indent <- function(str, n, pad=NULL) {
  if (is.null(pad)) {
    pad <- paste(rep(" ", n), collapse="")
  }
  p <- function(s) {
    paste(paste0(pad, s), collapse="\n")
  }
  vapply(strsplit(str, "\n"), p, character(1))
}

format_params <- function(xp) {
  fmt1 <- "\\itemize{\n%s\n}"
  fmt2 <- "\\item{\\code{%s}: %s\n}\n"
  pars <- sprintf(fmt2, names(xp), indent(unlist(xp), 2))
  sprintf(fmt1, indent(paste(pars, collapse="\n"), 2))
}

format_method <- function(x) {
  title <- sprintf("\\item{\\code{%s}}{", x$method_name)
  end <- "}"

  p_msg   <- setdiff(x$order, names(x$params))
  p_extra <- setdiff(names(x$params), x$order)
  if (length(p_msg) > 0) {
    warning(sprintf("In '%s', missing parameters: %s",
                    x$method_name, paste(p_msg, collapse=", ")),
            immediate.=TRUE, call.=FALSE)
  }
  if (length(p_extra) > 0) {
    warning(sprintf("In '%s', extra parameters: %s",
                    x$method_name, paste(p_extra, collapse=", ")),
            immediate.=TRUE, call.=FALSE)
  }
  ## preseve order, though I'm pretty sure that the yaml package is
  ## actually preserving it.
  if (length(p_msg) == 0 && length(p_extra) == 0) {
    x$params <- x$params[x$order]
  }

  body <- sprintf("%s\n\n\\emph{Usage:}\n\\code{%s}",
                  x$short, x$usage)
  if (!is.null(x$params)) {
    body <- paste0(body, "\n\n\\emph{Arguments:}\n", format_params(x$params))
  }
  if (!is.null(x$details)) {
    body <- paste0(body, "\n\n\\emph{Details:}\n", x$details)
  }
  if (!is.null(x$value)) {
    body <- paste0(body, "\n\n\\emph{Value}:\n", x$value)
  }
  paste(title, indent(body, 2), end, sep="\n")
}

strip_trailing_whitespace <- function(x) {
  gsub("[ \t]+(\n|$)", "\\1", x)
}

format_class <- function(x) {
  ret <- vapply(x, format_method, character(1))
  ret <- sprintf("@section Methods:\n\n\\describe{\n%s\n}",
                 paste(ret, collapse="\n"))
  ret <- indent(ret, pad="##' ")
  strip_trailing_whitespace(ret)
}

## From remake, rrqueue, etc, etc.
yaml_load <- function(string) {
  handlers <- list(`bool#yes` = function(x) {
    if (identical(toupper(x), "TRUE")) TRUE else x
  }, `bool#no` = function(x) {
    if (identical(toupper(x), "FALSE")) FALSE else x
  })
  yaml::yaml.load(string, handlers = handlers)
}
yaml_read <- function(filename) {
  yaml_load(paste(readLines(filename), collapse="\n"))
}

process_storr <- function() {
  dat <- add_usage(yaml_read("man-roxygen/storr.yml"), storr_environment())
  str <- format_class(dat)
  writeLines(str, "man-roxygen/storr_methods.R")
}

if (!interactive() && identical(commandArgs(TRUE), "process")) {
  process_storr()
}
