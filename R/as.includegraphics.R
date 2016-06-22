as.includegraphics <-
function(object,
         height="1em",
         width=NULL,
         wd=getwd(),
         raise=NULL,
         viewport=NULL, ## if specified, then left bottom right top.
                        ## used for pdf png jpeg
                        ## See MediaBox in pdf file.
                        ## Ask operating system for png file.
         bb=NULL, ## if specified, then left bottom right top.
                  ## used for bmp tiff
         trim="0 0 0 0", ## left bottom right top
         clip="true"
         ) {
  result <- paste0("\\includegraphics[",
                   if (!is.null(height)) paste0("height=", height),
                   if (!is.null(width)) paste0(", width=", width),
                   if (!is.null(viewport)) paste0(", viewport=", viewport),
                   if (trim != "0 0 0 0") paste0(", trim=", trim, ", clip=", clip),
                   "]{",
                   file.path(wd, object),
                   "}")
  if (!is.null(raise))
    result <- paste0("\\raisebox{", raise, "}{", result, "}")
  result
}
