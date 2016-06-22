as.htmlimg <- function(object, height = "80", width = NULL, ## height and width in pixels
                       wd = getwd(), align = "middle") {
  paste0('<img src="', file.path(wd, object), '" ',
        if (!is.null(height)) paste0('height="', height, '" '),
        if (!is.null(width))  paste0('width="',  width,  '" '),
        if (!is.null(align))  paste0('align="',  align,  '" '),
         " />")
}
