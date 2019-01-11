fake_file_ops <- function(root) {
  R6_fake_file_ops$new(root)
}


R6_fake_file_ops <- R6::R6Class(
  "fake_file_ops",

  public = list(
    root = NULL,

    initialize = function(root) {
      assert_scalar_character(root)
      self$root <- root
    },

    type = function() {
      "fake"
    },

    destroy = function() {
      self$delete_dir(NULL)
    },

    ## Create a directory (or something that will act as one)
    create_dir = function(path) {
      dir.create(file.path(self$root, path), FALSE, TRUE)
    },

    list_dir = function(path) {
      list.files(file.path(self$root, path))
    },

    exists = function(path) {
      file.exists(file.path(self$root, path))
    },

    exists_dir = function(path) {
      path_remote <- file.path(self$root, path)
      file.exists(path_remote) && file.info(path_remote)$isdir
    },

    delete_file = function(path) {
      file_remove(file.path(self$root, path))
    },

    delete_dir = function(path) {
      path_remote <-
        if (is.null(path)) self$root else file.path(self$root, path)
      unlink(path_remote, recursive = TRUE)
    },

    upload_file = function(file, dest_dir) {
      if (dest_dir == ".") {
        path_remote <- self$root
      } else {
        path_remote <- file.path(self$root, dest_dir)
        self$create_dir(dest_dir)
      }
      file.copy(file, path_remote, overwrite = TRUE)
      invisible(file.path(dest_dir, basename(file)))
    },

    download_file = function(file, dest_dir) {
      file_remote <- file.path(self$root, file)
      if (!file.exists(file_remote)) {
        stop("Remote resource not found")
      }
      if (is.null(dest_dir)) {
        readBin(file_remote, raw(), file.size(file_remote))
      } else {
        dir.create(dest_dir, FALSE, TRUE)
        file.copy(file_remote, dest_dir, overwrite = TRUE)
        file.path(dest_dir, basename(file))
      }
    },

    ## NOTE: these two _bytes functions are not *needed* but are
    ## present because I built this off the ssh driver
    write_bytes = function(bytes, dest_file) {
      tmp <- tempfile()
      dir.create(tmp)
      on.exit(unlink(tmp, recursive = TRUE))
      path_local <- file.path(tmp, basename(dest_file))
      writeBin(bytes, path_local)
      self$upload_file(path_local, dirname(dest_file))
    },

    read_bytes = function(file) {
      self$download_file(file, NULL)
    },

    write_string = function(string, dest_file) {
      self$write_bytes(charToRaw(paste0(string, "\n")), dest_file)
    },

    read_string = function(file) {
      sub("\n$", "", rawToChar(self$read_bytes(file)))
    }
  ))
