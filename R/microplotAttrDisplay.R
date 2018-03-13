microplotAttrDisplay <- function(ii,
                                 y.axis=unname(attr(ii, "axis.names")["y"]), ## the simpler [["y"]] doesn't work when NA
                                 x.axis=unname(attr(ii, "axis.names")["x"]),
                                 ylab=unname(attr(ii, "lab.names")["y"]),
                                 xlab=unname(attr(ii, "lab.names")["x"]),
                                 key=attr(ii, "key.name"),
                                 columnKey=NULL,
                                 label.x.axis="", ## empty, nchar=0
                                 label.y.axis=" " ## one space, nchar=1
                                 ) {

  jj <- ii

  if (length(y.axis)==0 || is.na(y.axis) || (is.logical(y.axis) && !y.axis)) y.axis <- NULL

  if (length(x.axis)==0 || is.na(x.axis) || (is.logical(x.axis) && !x.axis)) x.axis <- NULL

  if (!is.null(y.axis))
    if (length(ylab)==0 || is.na(ylab)   || (is.logical(ylab)   && !ylab  )) ylab <- NULL

  if (!is.null(xlab))
    if (length(xlab)==0 || is.na(xlab)   || (is.logical(xlab)   && !xlab  )) xlab <- NULL

  if (!is.null(y.axis)) {
    jj <- cbind(" "=y.axis, jj)
    colnames(jj)[1] <- label.y.axis
    if (!is.null(ylab))
      jj <- cbind(" "=ylab, jj)
  }

  if (!is.null(x.axis)) {
    x.axis <- rep(x.axis, ncol(ii))
    if (!is.null(y.axis)) {
      x.axis <- c(label.x.axis, x.axis)
      if (!is.null(ylab))
        x.axis <- c("", x.axis)
    }
    jj <- rbind(jj,
                " "=x.axis)
   }

  if (!is.null(xlab)) {
    xlab <- rep(xlab, ncol(ii))
    if (!is.null(y.axis)) {
      xlab <- c("", xlab)
      if (!is.null(ylab))
        xlab <- c("", xlab)
    }
    jj <- rbind(jj,
                " "=xlab)
  }

  if (!is.null(key)) {
    if (!is.null(columnKey)) {
      keyRep <- rep("", ncol(ii))
      keyRep[columnKey]<- key
      key <- keyRep
      if (!is.null(y.axis)) {
        key <- c("", key)
        if (!is.null(ylab))
          key <- c("", key)
      }
      jj <- rbind(jj,
                  " "=key)
    } else {
      attr(jj, "key.name") <- attr(ii, "key.name")
    }
  }

  class(jj) <- class(ii)

  ## if ("includegraphicsMatrix" %in% class(jj))
  ##   attr(jj, "microplotMatrix") <- attr(ii, "microplotMatrix")

  if (is.null(rownames(ii))) rownames(jj) <- NULL

  jj
}
