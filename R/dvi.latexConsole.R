dvi.latexConsole <- function(object, prlog=FALSE,
                      nomargins=TRUE, width=5.5, height=7, ..., ConsoleToFile=TRUE)
{
  fi <- object$file;
  sty <- object$style

  if(length(sty))
    sty <- paste('\\usepackage{',sty,'}',sep='')

  if(nomargins)
    sty <-  c(sty,
              paste('\\usepackage[paperwidth=',width,
                    'in,paperheight=', height,
                    'in,noheadfoot,margin=0in]{geometry}',sep=''))

  ## pre <- tempfile(); post <- tempfile()  # 1dec03
  tmp <- gsub("//", "/", tempfile()) ## necessary on Macintosh.  irrelevant elsewhere.
  tmptex <- paste(tmp, 'tex', sep='.')
  infi <- readLines(fi, n=-1)       # Splus 7 doesn't default to read to EOF 3may05
  cat('\\documentclass{report}', sty,
      '\\begin{document}\\pagestyle{empty}', infi,
      '\\end{document}\n', file=tmptex, sep='\n')

  tmpConsole <- if (ConsoleToFile)
                  paste0(tmp, 'Console.log')
                else
                  ""
  ToTmpConsole <- if (ConsoleToFile)
                    paste0(">", tmpConsole)
                  else
                    ""
  ## tmpConsole <- gsub("//", "/", tmpConsole)

  if (.Platform$OS.type == "unix")
    sys(paste("cd", shQuote(tempdir()), "&&", optionsCmds("latex"),
              "-interaction=scrollmode", shQuote(tmp), ToTmpConsole), output = FALSE)
  else ## MS DOS
    shell(paste("cd", shQuote(tempdir()), "&", optionsCmds("latex"),
                "-interaction=scrollmode", shQuote(tmp), ToTmpConsole), shell="CMD",
          intern = FALSE)


  if(prlog)
    cat(scan(paste(tmp,'log',sep='.'),list(''),sep='\n')[[1]],
        sep='\n')

  if (ConsoleToFile) cat("latex console file is", tmpConsole, "\n")

  extension <- getOption("dviExtension", "dvi")
  fi <- paste(tmp, extension, sep='.')
  structure(list(file=fi), class=c('dvilC', extension))
}



show.latexConsole <- function(object) ## function, not a method of generic show
{
  if(object$file == '') {
    if(length(object$style)) {
      environment(show.latex)$latexStyles <-
        if(exists("latexStyles", envir=environment(show.latex)))
          unique(c(environment(show.latex)$latexStyles, object$style))
        else object$style

    }

    return(invisible())
  }

  show.dvilC(dvi.latexConsole(object))
}
environment(show.latexConsole) <- new.env()

print.latexConsole <- function(x, ...) print(show.latexConsole(x))

print.dvilC <- function (x, ...) {
  result <- show.dvilC(x)
  if ("OSfilename" %in% class(result))
    print(result)
  else
    result
}

show.dvilC <-
function (object, width = 5.5, height = 7, ConsoleToFile=TRUE)
{
  TmpConsole <- ""
  psname <- ""
  viewer <- options()$xdvicmd
  if (is.null(viewer)) viewer <- "dvips"
  arguments <- switch(viewer,
                      dvips={
                        filename <- strsplit(basename(object$file), "\\.")[[1]][1]
                        psname <- paste0(filename, ".ps")
                        TmpConsole <- paste0(filename, "Console.log")
                        cat("Generated ps file is:\n  ", psname, "\n")
                        cat("Generated dvips log file is:\n  ", TmpConsole, "\n")
                        stdout <- if (version$os == "mingw32") "" else psname
                        object$file
                      },
                      xdvi=paste(" -paper ", width, "x", height, "in -s 0 ",
                                 object$file, sep = ""),
                      yap=,
                      kdvi=,
                      object$file)
  system2(viewer, args=arguments, stdout=stdout, stderr=TmpConsole, wait = TRUE)
  if (viewer == "dvips")
    structure(psname, class=c("psFilename", "OSfilename"))
  else
    invisible(object$file)
}


print.OSfilename <- function(x, wait=FALSE, ...) {
  show.OSfilename(x, wait=wait, ...)
  invisible(x)
}

show.OSfilename <- function(x, wait=FALSE, ...) {
  system(paste("open", x), wait=wait)
}
