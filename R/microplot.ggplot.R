microplot.ggplot <-
  function(object, ## object has class "ggplot"
           collapse=NULL, ## this doesn't work yet
           figPrefix=Hmisc::first.word(deparse(substitute(object))),
           height=stop("Specify height of each panel in inches.", call.=FALSE),
           width=stop("Specify width of each panel in inches.", call.=FALSE),
           x.axis=TRUE, y.axis=FALSE,
           key=FALSE,  ## FALSE, or a list which is a valid key
           height.key=height,
           width.key=width,
           device=c("pdf","postscript","png",
                    if (version$os == "mingw32") "win.metafile"), ## other devices require a user-written function
           ...) {  ## arguments to panel function, i.e., ggplot analogs to lattice cex and such

    stop("microplot.ggplot has not yet been written.", call.=FALSE)
  }

