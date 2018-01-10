microplot_yfactor <- function (object, ...)
  UseMethod("microplot_yfactor")

microplot_yfactor.ggplot <- ## This is a special-case function for the case that the y-variable is discrete
  function(object, ## object has class "ggplot"
           obji=object, ## "part of special-case, eventually construct a vector of single panels with background, labels, and margins removed.", ## not yet done.
           figPrefix=Hmisc::first.word(deparse(substitute(object))),
           height=stop("Specify height of each panel in inches.", call.=FALSE),
           width=stop("Specify width of each panel in inches.", call.=FALSE),
           x.axis=TRUE, y.axis=FALSE, ## currently y.axis is ignored, hence FALSE
           key=FALSE,  ## FALSE, or a list which is a valid key
           height.key=height,
           width.key=width,
           device=c("pdf","postscript","png", if (version$os == "mingw32") "win.metafile"), ## other devices require a user-written function
           n, ## required argument is this special-case function
           ...) {  ## arguments to panel function, i.e., cex and such

    if (missing(object) && (missing(obji) || missing(figPrefix)))
      stop("When object is missing, both obji and figPrefix must be specified.", call.=FALSE)

    ## n <- dim(obji)  ## for the moment, n is a required argument
    is.key <- if (is.logical(key)) {
                if (key) warning("specified key is not a valid key.", call.=FALSE)
                FALSE
              }
              else {
                warning("microplot_yfactor.ggplot can't plot a key.", call.=FALSE)
                FALSE
              }
    ## These functions write each panel, and perhaps the x-axis and key, to its own file.
    ## The return value is the vector of file names.
    filenames <- ## filenames is a vector of length n+axis["x"]+is.key
      get(paste0(device,"Microplot","_yfactorGgplot" ))(
        obji, n, is.key, figPrefix, height, width, x.axis, y.axis, key, height.key, width.key, ...)

    ## the return value is an array with up to two attributes
    invisible(
    structure(filenames[1:n], dim=n,  ## more special case
              axis.names=c(x=if (x.axis) filenames[n+1]),
              key.name = if (is.key) filenames[n+x.axis+is.key])
    )
  }



## The *Microplot_yfactorGgplot functions are special-case functions
## for the case that the y-variable is discrete.  Each steps through
## the panels, and then optionally writes an x.axis panel and a key
## (legend) panel.

pdfMicroplot_yfactorGgplot <-
  function(object,  ## for special case we are using object, not the vector version obji
           n,
           is.key,
           figPrefix,
           height,
           width,
           x.axis, y.axis,
           key,
           height.key,
           width.key,
           ...) {  ## arguments to panel function, i.e., cex and such
    filenames <- paste0(figPrefix, sprintf("%3.3i",1:(n+x.axis+is.key)), ".pdf")
    pdf(paste0(figPrefix, "%03d.pdf"),
        height=height, width=width, onefile=FALSE) ## inch
    object.xlim <- object$coordinates$limits$x  ## save x limits
    for (i in 1:n) { ## n individual boxplots without axes
      ## remove background, labels, margins
      object$coordinates$limits$x <- c(i, i) ## limit to one panel
      print(object + theme_collapse())
    }
    if (x.axis) {
      object$coordinates$limits$x <- c(-1, -1) ## no panels
      print(
      object + xlab(NULL) + ylab(NULL) +
        theme_collapse(axis.ticks=element_line(),
                       axis.ticks.y = element_blank(),
                       axis.text=element_text(),
                       axis.text.y = element_blank(),
                       axis.title=element_blank(),
                       axis.line.x=element_line(color="black")) ## display x axis
      )
    }
    object$coordinates$limits$x <- object.xlim  ## restore x limits
    dev.off()
    if (is.key) {
      warning("key not drawn", call.=FALSE)
      ## pdf(filenames[n+x.axis+is.key],
      ##     height=height.key, width=width.key) ## inch
      ## dev.off()
    }
    filenames ## return value is vector of file names
  }

postscriptMicroplot_yfactorGgplot <-
  function(object,  ## for special case we are using object, not the vector version obji
           n,
           is.key,
           figPrefix,
           height,
           width,
           x.axis, y.axis,
           key,
           height.key,
           width.key,
           ...) {  ## arguments to panel function, i.e., cex and such
    filenames <- paste0(figPrefix, sprintf("%3.3i",1:(n+x.axis+is.key)), ".ps")
    postscript(paste0(figPrefix, "%03d.ps"),
        height=height, width=width, onefile=FALSE, horizontal=FALSE) ## inch see ?postscript
    object.xlim <- object$coordinates$limits$x  ## save x limits
    for (i in 1:n) { ## n individual boxplots without axes
      ## remove background, labels, margins
      object$coordinates$limits$x <- c(i, i) ## limit to one panel
      print(object + theme_collapse())
    }
    if (x.axis) { ## n+x.axis
      object$coordinates$limits$x <- c(-1, -1) ## no panels
      print(
      object + xlab(NULL) + ylab(NULL) +
        theme_collapse(axis.ticks=element_line(),
                       axis.ticks.y = element_blank(),
                       axis.text=element_text(),
                       axis.text.y = element_blank(),
                       axis.title=element_blank(),
                       axis.line.x=element_line(color="black")) ## display x axis
      )
    }
    object$coordinates$limits$x <- object.xlim  ## restore x limits
    dev.off()
    if (is.key) {
      warning("key not drawn", call.=FALSE)
      ## postscript(filenames[n+x.axis+is.key],
      ##            height=height.key, width=width.key, horizontal=FALSE) ## inch see ?postscript
      ## dev.off()
    }
    filenames ## return value is vector of file names
  }


win.metafileMicroplot_yfactorGgplot <-
  function(object,  ## for special case we are using object, not the vector version obji
           n,
           is.key,
           figPrefix,
           height,
           width,
           x.axis, y.axis,
           key,
           height.key,
           width.key,
           ...) {  ## arguments to panel function, i.e., cex and such
    filenames <- paste0(figPrefix, sprintf("%3.3i",1:(n+x.axis+is.key)), ".wmf")
    win.metafile(paste0(figPrefix, "%03d.wmf"),
        height=height, width=width) ## inch
    object.xlim <- object$coordinates$limits$x  ## save x limits
    for (i in 1:n) { ## n individual boxplots without axes
      ## remove background, labels, margins
      object$coordinates$limits$x <- c(i, i) ## limit to one panel
      print(object + theme_collapse())
    }
    if (x.axis) { ## n+x.axis
      object$coordinates$limits$x <- c(-1, -1) ## no panels
      print(
      object + xlab(NULL) + ylab(NULL) +
        theme_collapse(axis.ticks=element_line(),
                       axis.ticks.y = element_blank(),
                       axis.text=element_text(),
                       axis.text.y = element_blank(),
                       axis.title=element_blank(),
                       axis.line.x=element_line(color="black")) ## display x axis
      )
    }
    object$coordinates$limits$x <- object.xlim  ## restore x limits
    dev.off()
      if (is.key) {
        warning("key not drawn", call.=FALSE)
        ## win.metafile(filenames[n+x.axis+is.key],
        ##     height=height.key, width=width.key) ## inch
        ##   draw.key(key, draw=TRUE) ## n+x.axis+is.key
        dev.off()
      }
    filenames ## return value is vector of file names
  }


pngMicroplot_yfactorGgplot <-
  function(object,  ## for special case we are using object, not the vector version obji
           n,
           is.key,
           figPrefix,
           height,
           width,
           x.axis, y.axis,
           key,
           height.key,
           width.key,
           ...) {  ## arguments to panel function, i.e., cex and such
    filenames <- paste0(figPrefix, sprintf("%3.3i",1:(n+x.axis+is.key)), ".png")
    object.xlim <- object$coordinates$limits$x  ## save x limits
    for (i in 1:n) {
      png(filenames[i],
          height=height, width=width, units="in", res=300) ## inch
      object$coordinates$limits$x <- c(i, i) ## limit to one panel
      print(object + theme_collapse())
      dev.off()
    }
    if (x.axis) {
      png(filenames[n+1],
          height=height, width=width, units="in", res=300) ## inch
      object$coordinates$limits$x <- c(-1, -1) ## no panels
      print(
      object + xlab(NULL) + ylab(NULL) +
        theme_collapse(axis.ticks=element_line(),
                       axis.ticks.y = element_blank(),
                       axis.text=element_text(),
                       axis.text.y = element_blank(),
                       axis.title=element_blank(),
                       axis.line.x=element_line(color="black")) ## display x axis
      )
      dev.off()
      object$coordinates$limits$x <- object.xlim  ## restore x limits
    }
    if (is.key) {
      warning("key not drawn", call.=FALSE)
      ## png(filenames[n+x.axis+is.key],
      ##     height=height.key, width=width.key, units="in", res=300) ## inch
      ## dev.off()
    }
    filenames ## return value is vector of file names
  }
