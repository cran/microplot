as.orgfile <- function(object, wd = getwd(), ...) {
    result <- paste0("[[file:", file.path(wd, object), "]]")
    ## For the nonce, all other arguments than object and wd are ignored.
    result
  }

as.orgtable <- function(x, rownames=FALSE) {
  tableheader <- paste(c("", dimnames(x)[[2]], ""), collapse=" | ")
  tmp <- dimnames(x)[[2]]
  tmp[] <- "-"
  tablerule <-  paste0("|", paste(tmp, collapse="+"), "|")
  tablebody <- apply(cbind("", x, ""), 1, paste, collapse=" | ")
  result <- as.matrix(c(tableheader, tablerule, tablebody))
  if (rownames) {
    result <- paste0("|", c("","-",row.names(x)), result)
    substring(result[2], 3, 3) <- "+"
  }
  result
}
