## TODO: it should be really straightforward to allow a vector of
## filenames here, I think.

##' Support for caching results of github releases.
##'
##' This might broaden out to allow uses of other sorts of storr
##' objects at some point, but for now it's pretty heavily tailored
##' towards the external storr, stored using an rds file in a
##' directory determined by \code{rappdirs}.
##'
##' These functions are designed for use only by authors of packages
##' that want to use the github release pattern to distribute
##' versioned data.  A blog post or publication about this is
##' forthcoming.
##'
##' @title Fetch and cache github release files
##' @param info Result of running \code{github_release_storr}.
##' @export
github_release_storr <- function(info) {
  name <- info$name
  path <- info$path
  if (is.null(github_release[[name]]) || !file.exists(path)) {
    furl <- github_release_file(info$repo, info$file)
    hook <- fetch_hook_download(furl, info$read)
    dr <- driver_external(driver_rds(path), hook)
    st <- storr(dr, name)
    st$set("github_release_versions", memoise(github_release_versions),
           namespace="internals")
    github_release[[name]] <- st
  }
  github_release[[name]]
}

## This is going to the environment we use to save the data within an
## R session.  It would probably be better to use a storr directly,
## but we can do that easily enough later.
github_release <- new.env(parent=emptyenv())

##' @export
##' @rdname github_release_storr
##' @param repo Name of the github repository, in format
##'   username/repository (e.g., \code{richfitz/storr}).
##' @param filename Name of the filename on a release.  Future
##'   versions of this may support multiple filenames - please let me
##'   know if that would be useful.
##' @param read Function to use to read the file.  Must take the
##'   filename of a downloaded file as an argument.  For example,
##'   \code{read.csv} would be appropriate to read in csv file
##'   (provided you like the default treatment of
##'   \code{stringsAsFactors}).
##' @param name Name to call this storr.  defaults to the repository
##'   name, but can be configured.  This is used to determine the name
##'   to save the cached data into.
github_release_storr_info <- function(repo, filename, read,
                                      name=basename(repo)) {
  path <- github_release_storr_path(name)
  structure(list(name=name, path=path, repo=repo,
                 filename=filename, read=read),
            class="github_release_storr_info")
}

##' @export
##' @rdname github_release_storr
##' @param version For \code{github_release_version_get}; version of
##'   the data to download.  For \code{github_release_del}, the
##'   version to delete.  \code{NULL} is treated differently for the
##'   two functions: for \code{get}, \code{NULL} means get the most
##'   recent version (according to
##'   \code{github_release_storr_version_current}), but for \code{del}
##'   it means delete \emph{all} versions, and the storr directory
##'   itself).
github_release_storr_get <- function(info, version=NULL) {
  if (is.null(version)) {
    version <- github_release_storr_version_current(info)
  }
  github_release_storr(info)$get(version)
}

##' @export
##' @rdname github_release_storr
##' @param type Type of version numbers to download: \code{local}
##'   returns locally downloaded versions, while \code{github} returns
##'   versions on github (will be looked up once per session only).
##'   The exception is for \code{github_release_storr_version_current}
##'   which, given \code{type="local"} will fall back on trying github
##'   if the local storr is empty.
github_release_storr_versions <- function(info, type="local") {
  st <- github_release_storr(info)
  if (type == "local") {
    st$list()
  } else if (type == "github") {
    st$get("github_release_versions", "internals")(info$repo)
  } else {
    stop("Unknown type ", type)
  }
}

##' @export
##' @rdname github_release_storr
github_release_storr_version_current <- function(info, type="local") {
  ## TODO: This *should* ping /latest I think.
  ## Manually add data if data and package versions are out of line
  v <- github_release_storr_versions(info, type)
  if (length(v) == 0L && type == "local") {
    v <- github_release_storr_versions(info, "github")
  }
  v[[length(v)]]
}

##' @export
##' @rdname github_release_storr
github_release_storr_del <- function(info, version) {
  if (is.null(version)) {
    unlink(github_release_storr_path(name), recursive=TRUE)
  } else {
    github_release_storr(info)$del(version)
  }
}


##' Utility function for extracting versions of github releases.
##' @title List github releases
##' @param repo Name of a repo in format username/repository
##' @param strip_v Strip the leading "v" from version names? (e.g.,
##' "v1.0.0" becomes "1.0.0"
##' @export
github_release_versions <- function(repo, strip_v=TRUE) {
  oo <- options(warnPartialMatchArgs=FALSE, warnPartialMatchDollar=FALSE)
  if (isTRUE(oo$warnPartialMatchArgs)) {
    on.exit(options(oo))
  }

  url <- sprintf("https://api.github.com/repos/%s/releases", repo)
  content <- httr::GET(url)

  tags <- vcapply(httr::content(content), function(x) x$tag_name)
  if (strip_v) {
    tags <- sub("^v", "", tags)
  }

  rev(tags)
}

##' @export
##' @param filename filename within the release
##' @param add_v Add the leading "v" to the version name (e.g, "1.0.0"
##' becomes "v1.0.0").
##' @rdname github_release_versions
github_release_file <- function(repo, filename, add_v=TRUE) {
  fmt <- sprintf("https://github.com/%s/releases/download/%s%%s/%s",
                 repo, if(add_v) "v" else "", filename)
  function(version, namespace) {
    sprintf(fmt, version)
  }
}

## TODO: Could possibly do this with a external?
## TODO: mangle_key not working properly if passed into
## storr_environment(); we're not picking that up correctly and I
## don't know why.
memoise <- function(fun) {
  force(fun)
  st <- storr_environment()
  function(...) {
    key <- digest::digest(list(...))
    if (!st$exists(key)) {
      st$set(key, fun(...))
    }
    st$get(key)
  }
}

##' @importFrom rappdirs user_data_dir
github_release_storr_path <- function(name) {
  rappdirs::user_data_dir(name)
}
