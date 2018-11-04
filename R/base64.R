##' Base64 encoding.  By default uses the RFC 4648 dialect (file/url
##' encoding) where characters 62 and 63 are "-" and "_".  Pass in "+"
##' and "/" to get the RFC 1421 variant (as in other R packages that
##' do base64 encoding).
##' @title Base64 encoding and decoding
##' @param x A string or vector of strings to encode/decode
##' @param char62 Character to use for the 62nd index
##' @param char63 Character to use for the 63rd index
##' @param pad Logical, indicating if strings should be padded with
##'   \code{=} characters (as RFC 4648 requires)
##' @export
##' @importFrom base64url base64_urlencode
##' @examples
##' x <- encode64("hello")
##' x
##' decode64(x)
##'
##' # Encoding things into filename-safe strings is the reason for
##' # this function:
##' encode64("unlikely/to be @ valid filename")
encode64 <- function(x, char62 = "-", char63 = "_", pad = TRUE) {
  if (length(x) != 1L) {
    return(vcapply(x, encode64, char62, char63, pad, USE.NAMES = FALSE))
  }
  out <- base64url::base64_urlencode(x)
  if (!identical(char62, "-")) {
    gsub(pattern = "-", replacement = char62, x = out, fixed = TRUE)
  }
  if (!identical(char63, "-")) {
    gsub(pattern = "-", replacement = char62, x = out, fixed = TRUE)
  }
  if (pad) {
    x <- as.integer(charToRaw(x))
    n_bytes <- length(x)
    n_blocks <- ceiling(n_bytes / 3L)
    n_pad <- 3L * n_blocks - n_bytes
    char_pad <- replicate(n_pad, "=")
    out <- paste(c(out, char_pad), collapse = "")
  }
  out
}


##' @param error Throw an error if the decoding fails.  If
##'   \code{FALSE} then \code{NA_character_} values are returned for
##'   failures.
##'
##' @export
##' @rdname encode64
decode64 <- function(x, char62 = "-", char63 = "_", error = TRUE) {
  if (length(x) != 1L) {
    return(vcapply(x, decode64, char62, char63, error, USE.NAMES = FALSE))
  }
  if (!grepl("^[A-Za-z0-9_-]*=*$", x)) {
    if (error) {
      stop(sprintf("Input '%s' is not base64 (url) encoded", x))
    } else {
      return(NA_character_)
    }
  }

  ## TODO: check that the string is correctly encoded before doing
  ## anything.
  tr <- c(LETTERS, letters, 0:9, char62, char63)

  ## sub is the timesink here, followed by strsplit.  charToRaw might be better.
  x <- strsplit(sub("=+$", "", x), NULL)[[1]]
  y <- match(x, tr) - 1L

  n_byte <- length(y)
  n_block <- ceiling(n_byte / 4L)

  y <- matrix(c(y, integer(4L * n_block - n_byte)), 4L, n_block)
  x <- matrix(integer(3 * n_block), 3, n_block)
  x[1L, ] <- bitwOr(bitwShiftL(y[1L, ], 2L), bitwShiftR(y[2L, ], 4L))
  x[2L, ] <- bitwOr(bitwShiftL(y[2L, ], 4L), bitwShiftR(y[3L, ], 2L))
  x[3L, ] <- bitwOr(bitwShiftL(y[3L, ], 6L), y[4L, ])
  x <- bitwAnd(x, 255L)

  rawToChar(as.raw(x))
}
