.get_debias_mod <- local({
  mod <- NULL
  function(reload = FALSE) {
    if (reload || is.null(mod) || reticulate::py_is_null_xptr(mod)) {
      
      pkg <- "PEcAnAssimSequential"
      
      # Installed package roots
      roots <- Filter(nzchar, c(
        system.file("python",        package = pkg),
        system.file("python_models", package = pkg)
      ))
      
      # Dev fallbacks (if running from source)
      if (!length(roots)) {
        ns_path <- tryCatch(getNamespaceInfo(pkg, "path"), error = function(e) NA_character_)
        roots <- unique(na.omit(c(
          file.path(ns_path, "python"),
          file.path(ns_path, "python_models"),
          normalizePath(file.path("inst", "python"), mustWork = FALSE),
          normalizePath(file.path("inst", "python_models"), mustWork = FALSE)
        )))
        roots <- roots[dir.exists(roots)]
      }
      
      if (!length(roots)) {
        stop("Could not find a python dir (inst/python or inst/python_models) in ", pkg, ".")
      }
      
      root <- roots[1]
      # Prefer package dir; else single file
      if (dir.exists(file.path(root, "pecan_debias")) &&
          file.exists(file.path(root, "pecan_debias", "__init__.py"))) {
        mod_name <- "pecan_debias"
      } else if (file.exists(file.path(root, "debias.py"))) {
        mod_name <- "debias"
      } else {
        stop("Expected either 'pecan_debias/__init__.py' or 'debias.py' under: ", root)
      }
      
      mod <<- reticulate::import_from_path(mod_name, path = root, convert = TRUE)
      if (reticulate::py_is_null_xptr(mod)) {
        stop("Import returned a null Python object (py_is_null_xptr == TRUE).")
      }
    }
    mod
  }
})

