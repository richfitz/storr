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
##'     \item{\code{value}:   Any R object to store.  The object will generally be serialized (this is not actually true for the environment storr) so only objects that would usually be expected to survive a `saveRDS`/`readRDS` roundtrip will work.  This excludes Rcpp modules objects, external pointers, etc.  But any "normal" R object will work fine.
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
##'   Like `set` but saves the object with a key that is the same as the hash of the object.  Equivalent to `$set(digest::digest(value), value)`.
##'
##'   \emph{Usage:}
##'   \code{set_by_value(value, namespace = self$default_namespace, use_cache = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{value}:   An R object to save, with the same limitations as `set`.
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
##'   Retrieve an object from the storr.  If the requested value is not found then a `KeyError` will be raised (an R error, but can be caught with `tryCatch`; see the "storr" vignette).
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
##'   Delete an object from the storr.
##'
##'   \emph{Usage:}
##'   \code{del(key, namespace = self$default_namespace)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   A vector of names of keys
##'     }
##'
##'     \item{\code{namespace}:   The namespace of the key.
##'     }
##'   }
##'
##'   \emph{Value}:
##'   A logical vector the same length as the recycled length of key/namespace, with each element being `TRUE` if an object was deleted, `FALSE` otherwise.
##' }
##' \item{\code{duplicate}}{
##'   Duplicate the value of a set of keys into a second set of keys. Because the value stored against a key is just the hash of its content, this operation is very efficient - it does not make a copy of the data, just the pointer to the data (for more details see the storr vignette which explains the storage model in more detail).  Multiple keys (and/or namespaces) can be provided, with keys and namespaces recycled as needed.  However, the number of source and destination keys must be the same.  The order of operation is not defined, so if the sets of keys are overlapping it is undefined behaviour.
##'
##'   \emph{Usage:}
##'   \code{duplicate(key_src, key_dest, namespace = self$default_namespace,
##'       namespace_src = namespace, namespace_dest = namespace)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key_src}:   The source key (or vector of keys)
##'     }
##'
##'     \item{\code{key_dest}:   The destination key
##'     }
##'
##'     \item{\code{namespace}:   The namespace to copy keys within (used only of `namespace_src` and `namespace_dest` are not provided
##'     }
##'
##'     \item{\code{namespace_src}:   The source namespace - use this where keys are duplicated across namespaces.
##'     }
##'
##'     \item{\code{namespace_dest}:   The destination namespace - use this where keys are duplicated across namespaces.
##'     }
##'   }
##' }
##' \item{\code{fill}}{
##'   Set one or more keys (potentially across namespaces) to the same value, without duplication effort serialisation, or duplicating data.
##'
##'   \emph{Usage:}
##'   \code{fill(key, value, namespace = self$default_namespace, use_cache = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   A vector of keys to get; zero to many valid keys
##'     }
##'
##'     \item{\code{value}:   A single value to set all keys to
##'     }
##'
##'     \item{\code{namespace}:   A vector of namespaces (either a single namespace or a vector)
##'     }
##'
##'     \item{\code{use_cache}:   Use the internal cache to avoid reading or writing to the underlying storage if the data has already been seen (i.e., we have seen the hash of the object before).
##'     }
##'   }
##' }
##' \item{\code{clear}}{
##'   Clear a storr.  This function might be slow as it will iterate over each key.  Future versions of storr might allow drivers to implement a bulk clear method that will allow faster clearing.
##'
##'   \emph{Usage:}
##'   \code{clear(namespace = self$default_namespace)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{namespace}:   A namespace, to clear a single namespace, or `NULL` to clear all namespaces.
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
##'     \item{\code{key}:   A vector of names of keys
##'     }
##'
##'     \item{\code{namespace}:   The namespace of the key.
##'     }
##'   }
##'
##'   \emph{Value}:
##'   A logical vector the same length as the recycled length of key/namespace, with each element being `TRUE` if the object exists and `FALSE` otherwise.
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
##' \item{\code{mset}}{
##'   Set multiple elements at once
##'
##'   \emph{Usage:}
##'   \code{mset(key, value, namespace = self$default_namespace, use_cache = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   A vector of keys to set; zero to many valid keys
##'     }
##'
##'     \item{\code{value}:   A vector of values
##'     }
##'
##'     \item{\code{namespace}:   A vector of namespaces (either a single namespace or a vector)
##'     }
##'
##'     \item{\code{use_cache}:   Use the internal cache to avoid reading or writing to the underlying storage if the data has already been seen (i.e., we have seen the hash of the object before).
##'     }
##'   }
##'
##'   \emph{Details:}
##'   The arguments `key` and `namespace` are recycled such that either can be given as a scalar if the other is a vector. Other recycling is not allowed.
##' }
##' \item{\code{mget}}{
##'   Get multiple elements at once
##'
##'   \emph{Usage:}
##'   \code{mget(key, namespace = self$default_namespace, use_cache = TRUE,
##'       missing = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   A vector of keys to get; zero to many valid keys
##'     }
##'
##'     \item{\code{namespace}:   A vector of namespaces (either a single namespace or a vector)
##'     }
##'
##'     \item{\code{use_cache}:   Use the internal cache to avoid reading or writing to the underlying storage if the data has already been seen (i.e., we have seen the hash of the object before).
##'     }
##'
##'     \item{\code{missing}:   Value to use for missing elements; by default `NULL` will be used.  IF `NULL` is a value that you might have stored in the storr you might want to use a different value here to distinguish "missing" from "set to NULL".  In addition, the `missing` attribute will indicate which values were missing.
##'     }
##'   }
##'
##'   \emph{Details:}
##'   The arguments `key` and `namespace` are recycled such that either can be given as a scalar if the other is a vector. Other recycling is not allowed.
##'
##'   \emph{Value}:
##'   A list with a length of the recycled length of `key` and `namespace`.  If any elements are missing, then an attribute `missing` will indicate the elements that are missing (this will be an integer vector with the indices of values were not found in the storr).
##' }
##' \item{\code{mset_by_value}}{
##'   Set multiple elements at once, by value.  A cross between `mset` and `set_by_value`.
##'
##'   \emph{Usage:}
##'   \code{mset_by_value(value, namespace = self$default_namespace, use_cache = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{value}:   A list or vector of values to set into the storr.
##'     }
##'
##'     \item{\code{namespace}:   A vector of namespaces
##'     }
##'
##'     \item{\code{use_cache}:   Use the internal cache to avoid reading or writing to the underlying storage if the data has already been seen (i.e., we have seen the hash of the object before).
##'     }
##'   }
##' }
##' \item{\code{gc}}{
##'   Garbage collect the storr.  Because keys do not directly map to objects, but instead map to hashes which map to objects, it is possible that hash/object pairs can persist with nothing pointing at them.  Running `gc` will remove these objects from the storr.
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
##'   The object if it is present, otherwise throw a `HashError`.
##' }
##' \item{\code{set_value}}{
##'   Add an object value, but don't add a key.  You will not need to use this very often, but it is used internally.
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
##' \item{\code{mset_value}}{
##'   Add a vector of object values, but don't add keys.  You will not need to use this very often, but it is used internally.
##'
##'   \emph{Usage:}
##'   \code{mset_value(values, use_cache = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{values}:   A list of R objects to set
##'     }
##'
##'     \item{\code{use_cache}:   Use the internal cache to avoid reading or writing to the underlying storage if the data has already been seen (i.e., we have seen the hash of the object before).
##'     }
##'   }
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
##'   \code{import(src, list = NULL, namespace = self$default_namespace,
##'       skip_missing = FALSE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{src}:   Object to import objects from; can be a list, environment or another storr.
##'     }
##'
##'     \item{\code{list}:   Names of of objects to import (or `NULL` to import all objects in `envir`.  If given it must be a character vector.  If named, the names of the character vector will be the names of the objects as created in the storr.
##'     }
##'
##'     \item{\code{namespace}:   Namespace to get objects from, and to put objects into.  If `NULL`, all namespaces from `src` will be imported. If named, then the same rule is followed as `list`; `namespace = c(a = b)` will import the contents of namespace `b` as namespace `a`.
##'     }
##'
##'     \item{\code{skip_missing}:   Logical, indicating if missing keys (specified in `list`) should be skipped over, rather than being treated as an error (the default).
##'     }
##'   }
##' }
##' \item{\code{export}}{
##'   Export objects from the storr into something else.
##'
##'   \emph{Usage:}
##'   \code{export(dest, list = NULL, namespace = self$default_namespace,
##'       skip_missing = FALSE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{dest}:   A target destination to export objects to; can be a list, environment, or another storr.  Use `list()` to export to a brand new list, or use `as.list(object)` for a shorthand.
##'     }
##'
##'     \item{\code{list}:   Names of objects to export, with the same rules as `list` in `$import`.
##'     }
##'
##'     \item{\code{namespace}:   Namespace to get objects from, and to put objects into.  If `NULL`, then this will export namespaces from this (source) storr into the destination; if there is more than one namespace,this is only possible if `dest` is a storr (otherwise there will be an error).
##'     }
##'
##'     \item{\code{skip_missing}:   Logical, indicating if missing keys (specified in `list`) should be skipped over, rather than being treated as an error (the default).
##'     }
##'   }
##'
##'   \emph{Value}:
##'   Invisibly, `dest`, which allows use of `e <- st$export(new.env())` and `x <- st$export(list())`.
##' }
##' \item{\code{archive_export}}{
##'   Export objects from the storr into a special "archive" storr, which is an [storr_rds] with name mangling turned on (which encodes keys with base64 so that they do not violate filesystem naming conventions).
##'
##'   \emph{Usage:}
##'   \code{archive_export(path, names = NULL, namespace = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{path}:   Path to create the storr at; can exist already.
##'     }
##'
##'     \item{\code{names}:   As for `$export`
##'     }
##'
##'     \item{\code{namespace}:   Namespace to get objects from.  If `NULL`, then exports all namespaces found in this (source) storr.
##'     }
##'   }
##' }
##' \item{\code{archive_import}}{
##'   Inverse of `archive_export`; import objects from a storr that was created by `archive_export`.
##'
##'   \emph{Usage:}
##'   \code{archive_import(path, names = NULL, namespace = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{path}:   Path of the exported storr.
##'     }
##'
##'     \item{\code{names}:   As for `$import`
##'     }
##'
##'     \item{\code{namespace}:   Namespace to import objects into.  If `NULL`, then imports all namespaces from the source storr.
##'     }
##'   }
##' }
##' \item{\code{index_export}}{
##'   Generate a data.frame with an index of objects present in a storr. This can be saved (for an rds storr) in lieu of the keys/ directory and re-imported with `index_import`.  It will provide a more version control friendly export of the data in a storr.
##'
##'   \emph{Usage:}
##'   \code{index_export(namespace = NULL)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{namespace}:   Optional character vector of namespaces to export.  The default is to export all namespaces.
##'     }
##'   }
##' }
##' \item{\code{index_import}}{
##'   Import an index.
##'
##'   \emph{Usage:}
##'   \code{index_import(index)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{index}:   Must be a data.frame with columns 'namespace', 'key' and 'hash' (in any order).  It is an error if not all hashes are present in the storr.
##'     }
##'   }
##' }
##' }
