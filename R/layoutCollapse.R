layoutHeightsCollapse <- function(...) {
  x.settings <- lattice::trellis.par.get()$layout.heights
  x.settings[] <- 0
  x.settings$panel=1
  inputs <- list(...)
  if (length(inputs))
    x.settings[names(inputs)] <- inputs
  x.settings
}

layoutWidthsCollapse <- function(...) {
  y.settings <- lattice::trellis.par.get()$layout.widths
  y.settings[] <- 0
  y.settings$panel=1
  inputs <- list(...)
  if (length(inputs))
    y.settings[names(inputs)] <- inputs
  y.settings
}
