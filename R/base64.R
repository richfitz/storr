encode64 <- function(x, char62="-", char63="_") {
  if (length(x) != 1L) {
    return(vcapply(x, encode64,char62, char63, USE.NAMES=FALSE))
  }
  tr <- c(LETTERS, letters, 0:9, char62, char63)
  x <- as.integer(charToRaw(x))
  n_bytes <- length(x)
  n_blocks <- ceiling(n_bytes / 3L)
  n_pad <- 3L * n_blocks - n_bytes

  ## The integer() call here pads the *input* to have the correct number
  ## of blocks of bytes.
  x <- matrix(c(x, integer(3L * n_blocks - n_bytes)), 3L, n_blocks)

  y <- matrix(integer(4 * n_blocks), 4L, n_blocks)
  y[1L, ] <- bitwShiftR(x[1L, ], 2L)
  y[2L, ] <- bitwOr(bitwShiftL(x[1L, ], 4L), bitwShiftR(x[2L, ], 4L))
  y[3L, ] <- bitwOr(bitwShiftL(x[2L, ], 2L), bitwShiftR(x[3L, ], 6L))
  y[4L, ] <- x[3L, ]

  z <- tr[bitwAnd(y, 63L) + 1L]
  if (n_pad > 0) {
    len <- length(z)
    z[(len - n_pad + 1):len] <- "="
  }
  paste0(z, collapse="")
}

decode64 <- function(x, char62="-", char63="_") {
  if (length(x) != 1L) {
    return(vcapply(x, decode64,char62, char63, USE.NAMES=FALSE))
  }
  ## TODO: check that the string is correctly encoded before doing
  ## anything.
  tr <- c(LETTERS, letters, 0:9, char62, char63)

  ## sub is the timesink here, followed by strsplit.  charToRaw might be better.
  x <- strsplit(sub("=+$", "", x), NULL)[[1]]
  y <- match(x, tr) - 1L

  n_byte <- length(y)
  n_block <- ceiling(n_byte / 4L)
  nd_byte <- 3L * n_block

  ## Hmm.
  y <- matrix(c(y, integer(4L * n_block - n_byte)), 4L, n_block)
  x <- matrix(integer(3 * n_block), 3, n_block)
  x[1L, ] <- bitwOr(bitwShiftL(y[1L, ], 2L), bitwShiftR(y[2L, ], 4L))
  x[2L, ] <- bitwOr(bitwShiftL(y[2L, ], 4L), bitwShiftR(y[3L, ], 2L))
  x[3L, ] <- bitwOr(bitwShiftL(y[3L, ], 6L), y[4L, ])
  x <- bitwAnd(x, 255L)

  rawToChar(as.raw(x))
}
