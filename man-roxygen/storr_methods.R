##' @section Methods:
##'
##' \describe{
##' \item{\code{destroy}}{
##'   Totally destroys the storr by telling the driver to destroy all the data and then deleting the driver.  This will remove all data and cannot be undone.
##'
##'   \emph{Usage:}
##'   \code{destroy()}
##' }
##' \item{\code{flush_cache}}{
##'   Flush the temporary cache of objects that accumulates as the storr is used.  Should not need to be called often.
##'
##'   \emph{Usage:}
##'   \code{flush_cache()}
##' }
##' \item{\code{set}}{
##'   Set a key to a value.
##'
##'   \emph{Usage:}
##'   \code{set(key, value, namespace = self$default_namespace, use_cache = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The key name.  Can be any string.
##'     }
##'
##'     \item{\code{value}:   Any R object to store.  The object will generally be serialised (this is not actually true for the environment storr) so only objects that would usually be expected to survive a \code{saveRDS}/\code{readRDS} roundtrip will work.  This excludes Rcpp modules objects, external pointers, etc.  But any "normal" R object will work fine.
##'     }
##'
##'     \item{\code{namespace}:   An optional namespace.  By default the default namespace that the storr was created with will be used (by default that is "objects").  Different namespaces allow different types of objects to be stored without risk of names colliding.  Use of namespaces is optional, but if used they must be a string.
##'     }
##'
##'     \item{\code{use_cache}:   Use the internal cache to avoid reading or writing to the underlying storage if the data has already been seen (i.e., we have seen the hash of the object before).
##'     }
##'   }
##'
##'   \emph{Value}:
##'   Invisibly, the hash of the saved object.
##' }
##' \item{\code{set_by_value}}{
##'   Like \code{set} but saves the object with a key that is the same as the hash of the object.  Equivalent to \code{$set(digest::digest(value), value)}.
##'
##'   \emph{Usage:}
##'   \code{set_by_value(value, namespace = self$default_namespace, use_cache = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{value}:   An R object to save, with the same limitations as \code{set}.
##'     }
##'
##'     \item{\code{namespace}:   Optional namespace to save the key into.
##'     }
##'
##'     \item{\code{use_cache}:   Use the internal cache to avoid reading or writing to the underlying storage if the data has already been seen (i.e., we have seen the hash of the object before).
##'     }
##'   }
##' }
##' \item{\code{get}}{
##'   Retrieve an object from the storr.  If the requested value is not found thena \code{KeyError} will be raised (an R error, but can be caught with \code{tryCatch}.  See the "storr" vignette.
##'
##'   \emph{Usage:}
##'   \code{get(key, namespace = self$default_namespace, use_cache = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The name of the key to get.
##'     }
##'
##'     \item{\code{namespace}:   Optional namespace to look for the key within.
##'     }
##'
##'     \item{\code{use_cache}:   Use the internal cache to avoid reading or writing to the underlying storage if the data has already been seen (i.e., we have seen the hash of the object before).
##'     }
##'   }
##' }
##' \item{\code{get_hash}}{
##'   Retrieve the hash of an object stored in the storr (rather than the object itself).
##'
##'   \emph{Usage:}
##'   \code{get_hash(key, namespace = self$default_namespace)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The name of the key to get.
##'     }
##'
##'     \item{\code{namespace}:   Optional namespace to look for the key within.
##'     }
##'   }
##' }
##' \item{\code{del}}{
##'   Delete an object fom the storr.
##'
##'   \emph{Usage:}
##'   \code{del(key, namespace = self$default_namespace)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The name of the key
##'     }
##'
##'     \item{\code{namespace}:   The namespace of the key.
##'     }
##'   }
##'
##'   \emph{Value}:
##'   \code{TRUE} if an object was deleted, \code{FALSE} otherwise.
##' }
##' \item{\code{clear}}{
##'   Clear a storr.  This function might be slow as it will iterate over each key.  Future versions of storr might allow drivers to implement a clear method that will allow faster clearing.
##'
##'   \emph{Usage:}
##'   \code{clear(namespace = self$default_namespace)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{namespace}:   A namespace, to clear a single namespace, or \code{NULL} to clear all namespaces.
##'     }
##'   }
##' }
##' \item{\code{exists}}{
##'   Test if a key exists within a namespace
##'
##'   \emph{Usage:}
##'   \code{exists(key, namespace = self$default_namespace)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The name of the key
##'     }
##'
##'     \item{\code{namespace}:   The namespace of the key.
##'     }
##'   }
##' }
##' \item{\code{exists_object}}{
##'   Test if an object with a given hash exists within the storr
##'
##'   \emph{Usage:}
##'   \code{exists_object(hash)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{hash}:   Hash to test
##'     }
##'   }
##' }
##' \item{\code{gc}}{
##'   Garbage collect the storr.  Because keys do not directly map to objects, but instead map to hashes which map to objects, it is possible that hash/object pairs can persist with nothing pointing at them.  Running \code{gc} will remove these objects from the storr.
##'
##'   \emph{Usage:}
##'   \code{gc()}
##' }
##' \item{\code{get_value}}{
##'   Get the content of an object given its hash.
##'
##'   \emph{Usage:}
##'   \code{get_value(hash, use_cache = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{hash}:   The hash of the object to retrieve.
##'     }
##'
##'     \item{\code{use_cache}:   Use the internal cache to avoid reading or writing to the underlying storage if the data has already been seen (i.e., we have seen the hash of the object before).
##'     }
##'   }
##'
##'   \emph{Value}:
##'   The object if it is present, otherwise throw a \code{HashError}.
##' }
##' \item{\code{set_value}}{
##'   Set a hash to a value.  You will not need to use this very often, but it is used internally.
##'
##'   \emph{Usage:}
##'   \code{set_value(value, use_cache = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{value}:   An R object to set.
##'     }
##'
##'     \item{\code{use_cache}:   Use the internal cache to avoid reading or writing to the underlying storage if the data has already been seen (i.e., we have seen the hash of the object before).
##'     }
##'   }
##'
##'   \emph{Value}:
##'   Invisibly, the hash of the object.
##' }
##' \item{\code{list}}{
##'   List all keys stored in a namespace.
##'
##'   \emph{Usage:}
##'   \code{list(namespace = self$default_namespace)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{namespace}:   The namespace to list keys within.
##'     }
##'   }
##'
##'   \emph{Value}:
##'   A sorted character vector (possibly zero-length).
##' }
##' \item{\code{list_hashes}}{
##'   List all hashes stored in the storr
##'
##'   \emph{Usage:}
##'   \code{list_hashes()}
##'
##'   \emph{Value}:
##'   A sorted character vector (possibly zero-length).
##' }
##' \item{\code{list_namespaces}}{
##'   List all namespaces known to the database
##'
##'   \emph{Usage:}
##'   \code{list_namespaces()}
##'
##'   \emph{Value}:
##'   A sorted character vector (possibly zero-length).
##' }
##' \item{\code{import}}{
##'   Import R objects from an environment.
##'
##'   \emph{Usage:}
##'   \code{import(src, list = NULL, namespace = self$default_namespace)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{src}:   Object to import objects from; can be a list, environment or another storr.
##'     }
##'
##'     \item{\code{list}:   Names of of objects to import (or \code{NULL} to import all objects in \code{envir}.  If given it must be a character vector.  If named, the names of the character vector will be the names of the objects as created in the storr.
##'     }
##'
##'     \item{\code{namespace}:   Namespace to get objects from, and to put objects into.
##'     }
##'   }
##' }
##' \item{\code{export}}{
##'   Export objects from the storr into something else.
##'
##'   \emph{Usage:}
##'   \code{export(dest, list = NULL, namespace = self$default_namespace)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{dest}:   A target destination to export objects to; can be a list, environment, or another storr.  Use \code{list()} to export to a brand new list, or use \code{as.list(object)} for a shorthand.
##'     }
##'
##'     \item{\code{list}:   Names of objects to export, with the same rules as \code{list} in \code{$import}.
##'     }
##'
##'     \item{\code{namespace}:   Namespace to get objects from, and to put objects into.
##'     }
##'   }
##'
##'   \emph{Value}:
##'   Invisibly, \code{dest}, which allows use of \code{e <- st$export(new.env())}.
##' }
##' \item{\code{archive_export}}{
##'   Export objects from the storr into a special "archive" storr, which is an \code{\link{storr_rds}} with name mangling turned on (which encodes keys with base64 so that they do not voilate filesystem naming conventions).
##'
##'   \emph{Usage:}
##'   \code{archive_export(path, names = NULL, namespace = self$default_namespace)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{path}:   Path to create the storr at; can exist already.
##'     }
##'
##'     \item{\code{names}:   As for \code{$export}
##'     }
##'
##'     \item{\code{namespace}:   Namespace to get objects from.
##'     }
##'   }
##' }
##' \item{\code{archive_import}}{
##'   Inverse of \code{archive_export}; import objects from a storr that was created by \code{archive_export}.
##'
##'   \emph{Usage:}
##'   \code{archive_import(path, names = NULL, namespace = self$default_namespace)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{path}:   Path of the exported storr.
##'     }
##'
##'     \item{\code{names}:   As for \code{$import}
##'     }
##'
##'     \item{\code{namespace}:   Namespace to import objects into.
##'     }
##'   }
##' }
##' }
