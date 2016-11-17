storr_traits_default <- function() {
  list(accept_raw = FALSE,
       throw_missing = FALSE,
       drop_r_version = FALSE,
       hash_algorithm = TRUE)
}

storr_traits <- function(given) {
  default <- storr_traits_default()
  extra <- setdiff(names(given), names(default))
  if (length(extra) > 0L) {
    stop("Unknown traits ", paste(extra, collapse = ", "))
  }
  default[names(given)] <- given

  if (default$drop_r_version && !default$accept_raw) {
    stop("if drop_r_version is TRUE, then accept_raw must also be TRUE")
  }

  default
}
