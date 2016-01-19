## The overhead of using tryCatch is ~2-4 us but the overhead of a
## Redis PING is about 32 us so the savings are worth it for many
## things.  For rds it's about even.
storr_traits_default <- function() {
  list(accept_raw=FALSE,
       throw_missing=FALSE)
}

storr_traits <- function(given) {
  default <- storr_traits_default()
  extra <- setdiff(names(given), names(default))
  if (length(extra) > 0L) {
    stop("Unknown traits ", paste(extra, collapse=", "))
  }
  default[names(given)] <- given
  default
}
