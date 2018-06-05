microplot.ggplot <-
  function(object, ## object has class "ggplot"
           collapse=theme_collapse(), ## theme_collapse(...) ?
           figPrefix=first.word(deparse(substitute(object))),
           vectorgraph.colname=figPrefix,
           height.panel=1, ## inch
           width.panel=1,  ## inch
           height.x.axis=height.panel,
           width.y.axis=width.panel,
           height.xlab=height.panel,
           width.ylab=width.panel,
           height.key=height.panel,
           width.key=width.panel,
           ## xlim=object$coordinates$limits$x,
           ## ylim=object$coordinates$limits$y,
           tick.text.size=7,
           key=FALSE, ## plot_grid(get_legend(object)), ## FALSE, or a ggplot object which is a valid key
           device=c("pdf","postscript","ps","png"),
           res=600, type=getOption("bitmapType"), ## used by png
           ...) {  ## arguments to panel function, i.e., ggplot analogs to lattice cex and such

    ## change "." to "-" in figPrefix
    ##  The 'latex' macro \code{\\includegraphics} requires that there be no
    ##  \code{"."} in the filename basename.
    figPrefix <- RemoveDot(figPrefix)

    ## if (is.null(xlim) || is.null(ylim))
    ##   warning("x and/or y limits are not specified. ",
    ##           "Panels might not be equivalently rendered.", call.=FALSE)

    device <- match.arg(device)
    if (device=="ps") device <- "postscript"

    gtobjc <- plot_to_gtable(object + collapse)  ## panels
    gtobj <- plot_to_gtable(object)              ## not panels

    ## panels
    panel.grobnumbers <- grep("panel", gtobj$layout$name)
    n.panels <- length(panel.grobnumbers)
    last.panel <- gtobj$layout$name[panel.grobnumbers[n.panels]]
    c.r.last.panels <- as.numeric(strsplit(last.panel, "-")[[1]][-1])
    if (length(c.r.last.panels) > 2)
      stop("microplot.ggplot requires 1D or 2D panel structure.", call.=FALSE)
    if (length(c.r.last.panels) == 0) {
      c.r.panels <- c(1, 1)
      nCC <- c.r.panels[1]
      nRR <- c.r.panels[2]
      n <- nRR * nCC
      RR <- NULL
      CC <- NULL
      }
    if (length(c.r.last.panels) == 1) {
      c.r.panels <- c(1, c.r.last.panels)
      nCC <- c.r.panels[1]
      nRR <- c.r.panels[2]
      n <- nRR * nCC
      RR <- {
        if ("facets" %in% names(object$facet$params))
          levels(object$data[[names(object$facet$params$facets)]])
        else
          levels(object$data[[names(object$facet$params$rows)]])
      }
      if (!object$facet$params$as.table) RR <- rev(RR)
      CC <- NULL
    }
   if (length(c.r.last.panels) == 2) {
      c.r.panels <- c.r.last.panels
      nCC <- c.r.panels[1]
      nRR <- c.r.panels[2]
      n <- nRR * nCC
      RR <- {
        if ("facets" %in% names(object$facet$params))
          levels(object$data[[names(object$facet$params$facets)]])
        else
          levels(object$data[[names(object$facet$params$rows)]])
      }
      if (!object$facet$params$as.table) RR <- rev(RR)
      names.cols <- names(object$facet$params$cols)
      CC <- if (length(names.cols)==0 || is.null(names.cols))
              vectorgraph.colname
            else
              levels(object$data[[names.cols]])
    }

    x.axis.grobnumber <- grep("axis-b", gtobj$layout$name)[1]
    y.axis.grobnumber <- grep("axis-l", gtobj$layout$name)[1]

    xlab.grobnumber <- grep("xlab-b", gtobj$layout$name)[1]
    ylab.grobnumber <- grep("ylab-l", gtobj$layout$name)[1]

    is.key <- is.list(key)


    ## Write each panel, the x.axis and y.axis, key, xlab, ylab to its own file.
    ## the return value is an array with up to three attributes.
    extension <- c(pdf=".pdf",
                   postscript=".ps",
                   png=".png")
    filenames.all <- paste0(figPrefix,
                            sprintf("%3.3i",1:(n+2+length(xlab.grobnumber)+length(ylab.grobnumber)+is.key)),
                            extension[device])

    filenames <- matrix(filenames.all[1:n],
                        nRR, nCC, dimnames=list(RR, CC), byrow=TRUE)
    attr(filenames, "axis.names") <- c(x=filenames.all[n+1],
                                       y=filenames.all[n+2])
    attr(filenames, "lab.names")  <- c(x=filenames.all[n+3],
                                       y=filenames.all[n+4])
    if (is.key) attr(filenames, "key.name") <- filenames.all[n+5]

    ## panels
    i <- 0
    for (gn in panel.grobnumbers) {
      i <- i+1
      writePanel(plot_grid(gtobjc$grobs[[gn]]),
                device, filenames[i], height=height.panel, width=width.panel, res=res)
    }

    ## x axis ## n+1
    writePanel(plot_grid(gtobj$grobs[[x.axis.grobnumber]]),
               device, filenames.all[n+1], height=height.x.axis, width=width.panel, res=res)

    ## y axis ## n+2
    writePanel(plot_grid(gtobj$grobs[[y.axis.grobnumber]]),
               device, filenames.all[n+2], height=height.panel, width=width.y.axis, res=res)

    ## xlab ## n+3
    writePanel(plot_grid(gtobj$grobs[[xlab.grobnumber]]),
               device, filenames.all[n+3], height=height.xlab, width=width.panel, res=res)

    ## ylab ## n+4
    writePanel(plot_grid(gtobj$grobs[[ylab.grobnumber]]),
               device, filenames.all[n+4], height=height.panel, width=width.ylab, res=res)

    ## legend ## n+5
    if (is.key) {
      writePanel(key,
                 device, filenames.all[n+5], height=height.key, width=width.key, res=res)
    }

    class(filenames) <- c("microplotMatrix", class(filenames))

    filenames
  }
