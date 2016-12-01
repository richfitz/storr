storr_traits_default <- function() {
  list(accept = "object",
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

  match_value(default$accept, c("object", "raw", "string"), "accept")

  if (default$drop_r_version && default$accept != "raw") {
    stop("if 'drop_r_version' is TRUE, then 'accept' must be \"raw\"")
  }

  default
}
