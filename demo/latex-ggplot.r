## tests R/microplot.ggplot

## These are the settings for my machines
## Set options for Hmisc::latex
latexSetOptions()

library(ggplot2)

dd <- data.frame(rr=rep(letters[1:4], each=3*10),
                 cc=rep(LETTERS[5:7], each=10, times=4),
                 x=rnorm(120, s=20+rep(1:12, each=10)),
                 y=rnorm(120, m=100, s=20+rep(1:12, each=10)),
                 g=rep(c("1","2","3","4","5","6","7","8","9","A","B","C"),
                       each=10),
                 rra=rep(letters[8:9], each=5))

dd$ggg <- c(49:57, 65:67)[as.numeric(dd$g)]
dd$col <- factor(HH::likertColor(12)[as.numeric(dd$g)],
                 levels=HH::likertColor(12))
mycolors <- levels(dd$col)
names(mycolors) <- levels(dd$g)



gg <- ggplot(dd, aes(x, y)) +
  scale_x_continuous(limits = c(-120, 120)) +
  scale_y_continuous(limits = c( -20, 220)) +
  theme(axis.text=element_text(size=6)) +
  geom_point(aes(shape=dd$ggg), col=dd$col, size=5) +
  scale_shape_identity() +
  facet_grid(rr ~ cc)
gg

latex(gg)

gg <- gg +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        legend.key=element_blank()) +
  geom_line(aes(x=-200, y=-200, color=levels(dd$g)[ 1])) +
  geom_line(aes(x=-200, y=-200, color=levels(dd$g)[ 2])) +
  geom_line(aes(x=-200, y=-200, color=levels(dd$g)[ 3])) +
  geom_line(aes(x=-200, y=-200, color=levels(dd$g)[ 4])) +
  geom_line(aes(x=-200, y=-200, color=levels(dd$g)[ 5])) +
  geom_line(aes(x=-200, y=-200, color=levels(dd$g)[ 6])) +
  geom_line(aes(x=-200, y=-200, color=levels(dd$g)[ 7])) +
  geom_line(aes(x=-200, y=-200, color=levels(dd$g)[ 8])) +
  geom_line(aes(x=-200, y=-200, color=levels(dd$g)[ 9])) +
  geom_line(aes(x=-200, y=-200, color=levels(dd$g)[10])) +
  geom_line(aes(x=-200, y=-200, color=levels(dd$g)[11])) +
  geom_line(aes(x=-200, y=-200, color=levels(dd$g)[12])) +
  scale_colour_manual(name='', values=mycolors)
gg
ggKey <- plot_grid(get_legend(gg)) ## both functions imported from cowplot package
ggKey


## 1. starting at upper left, by rows
gg + labs(title="gg")

## 1.a no spacing control
latex(gg, caption="gg")

## 1.b height and width of axis and lab,
##     these numbers come from trial and error for this graph
latex(gg, caption="gg, height and width of axes",
      rowlabel="row",
      height.x.axis=.2, ## inch
      width.y.axis=.3)  ## inch

## 1.c trim some space from the left side of the y.axis,
##     and adjust width to compensate;
##     key
latex(gg, caption="gg, trim y.axis, key",
      rowlabel="row",
      height.x.axis=.2, ## inch
      width.y.axis=.3, ## inch
      y.axis.includegraphics=
        list(viewport="0 0 21 72", trim="6 0 0 0", width=paste0(.3*15/21,"in")),
      key=ggKey, width.key=6)


## vector
dd$rr.cc <- with(dd, interaction(rr,cc)) ## rr.cc must be a column of dd
ggv <- ggplot(dd, aes(x, y)) +
  scale_x_continuous(limits = c(-120, 120)) +
  scale_y_continuous(limits = c( -20, 220)) +
  theme(axis.text=element_text(size=6)) +
  geom_point(aes(shape=dd$ggg), col=dd$col, size=5) +
  scale_shape_identity() +
  facet_grid(rr.cc ~ .)
ggv

latex(ggv, height.panel=.41,
      height.x.axis=.2, ## inch
      width.y.axis=.3,  ## inch
      rowlabel="group",
      vectorgraph.colname="Graph Panels")




detach("package:ggplot2") ## can't unload, imported by other packages
