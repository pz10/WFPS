folder_out <- paste0(output_path, "/monography_plots/WFPS")
dir.create(folder_out, recursive=T)

# flux file
myfolder <- paste0(output_path, "/WFPS")
myfile <- paste0(myfolder, "/3h_interpolation.dat")

# read data file
toplot <- fread(myfile)

toplot[,precipitation:= factor(precipitation, levels = c("c", "i", "d"), labels = c("cons", "incr", "decr"))]
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"))


arrows <- data.table(x = rep(c(0, 15, 30), 2),
                     y= c(10, 30, 50, 50, 30, 10),
                     xend =  rep(c(0, 15, 30), 2),
                     yend = 0,
                     precipitation = rep(c("incr", "decr"), each=3))

arrows.cons <- data.table(x = seq(0, 45, 1.5),
                          y= 10,
                          xend =  seq(0, 45, 1.5),
                          yend = 0,
                          precipitation = "cons")

labels <- data.table(x = c(20, 0, 15, 30, 0, 15, 30),
                     y= c(12.5, 0,20,40, 40,20,10),
                     #                      y= c(12.5, rep(0,6)),
                     precipitation = c("cons","incr","incr","incr", "decr", "decr", "decr" ),
                     label = c("2 mm / day", "10 mm", "30 mm", "50 mm", "50 mm", "30 mm", "10 mm" ))
rain.pattern <- data.table(x = rep(47.5, 3),
                           y= rep(50,3),
                           precipitation = c("cons","incr","decr"),
                           label = c("homogeneous", "increasing", "decreasing"))
legend <- data.table(x = c(-3, -3),
                     y= c(65, 85),
                     xend =  c(0,0),
                     yend = c(65, 85),
                     precipitation = "cons",
                     col=c("black", "red"),
                     label=c("no tillage", "traditional tillage")
)

g.1 <- ggplot(toplot, aes(days.adjusted, WFPS_0))
g.1 <- (g.1
        + facet_wrap( ~ precipitation, scales = "fixed", ncol=1)
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin =       unit(-0.4, "lines"),
                strip.background =   element_blank(),
                strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("WFPS  [%]")
        + xlab("days")
        + labs(title = "(a)")
        + coord_cartesian(ylim=c(-5, 105), xlim= c(-5, 50))
        + scale_x_continuous(breaks = c(0, 15, 30, 45))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + geom_point(aes(colour = tillage), size=0.25)
        + scale_colour_manual(values = c("black","red"))
        + geom_segment(data=arrows, aes(x=x,y=y,yend=yend,xend=xend), col = "grey30", size=0.3,
                       arrow = arrow(angle = 10, type="closed", length = unit(1, "mm")))
        + geom_segment(data=arrows.cons, aes(x=x,y=y,yend=yend,xend=xend), col = "grey30", size=0.3,
                       arrow = arrow(angle = 10, type="closed", length = unit(1, "mm")))
        + geom_text(data=labels, aes(x=x, y=y, label=label), hjust= -0.1, vjust=0, size=2, col= "grey50")
        + geom_text(data=rain.pattern, aes(x=x, y=y, label=label),
                    hjust= 0.5, vjust=0.5, size=2.5, angle=90, col= "grey50")
        + geom_segment(data=legend, mapping = aes(x=x,y=y,yend=yend,xend=xend), size=0.5, colour=legend$col)
        + geom_text(data=legend, mapping = aes(x=x+5,y=y, label=label),
                    hjust= 0, vjust=0.5, size=2)
)
# g.1

# myplot <- paste0(folder_out,"/compilation_WFPS_0.png")
# png(filename=myplot,  width = 80, height = 80, units = "mm", res=1200)
# print(g.1)
# dev.off()
################
####
# WFPS_0 kernel density estimate by rain event
toplot[, event:= as.character(NA)]
toplot[days.adjusted>0 & days.adjusted<14, event:= " days:   0 - 15"]
toplot[days.adjusted>15 & days.adjusted<29, event:= "15 - 30"]
toplot[days.adjusted>30 & days.adjusted<44, event:= "30 - 45"]

toplot[,myWFPS_0:= WFPS_0]
toplot[is.na(days.adjusted),myWFPS_0:=NA]
toplot[days.adjusted < 0, myWFPS_0:=NA]

toplot <- toplot[!is.na(event)]
to.plot.tt <- toplot[tillage=="T"]
to.plot.nt <- toplot[tillage=="NT"]

# d <- density(toplot[event=="0 - 15" & tillage=="T", myWFPS_0], adjust = 1/2)
# d <- density(toplot[event=="0 - 15", myWFPS_0], adjust = 1/2)
d <- density(toplot$myWFPS_0, adjust = 1/2)# bw.nrd0(toplot$myWFPS_0) to know the default bw for our data
myd <- data.table(myWFPS_0=d$x, y=d$y)
# myd[,y:=6*y]

# plot(toplot$myWFPS_0)
# 

legend <- data.table(x = c(0.5, 0.5),
                     y= c(0,7),
                     xend =  c(1.1, 1.1),
                     yend = c(0,7),
                     event = "30 - 45",
                     col=c("black", "red"),
                     label=c("no tillage", "traditional tillage")
)

g.2 <- ggplot(toplot, aes(x= myWFPS_0))
g.2 <- (g.2
        + facet_wrap( ~ event, scales = "fixed")
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.25, 0, 0.25), "lines"),
                legend.position =   "none",
                axis.ticks = element_line(size = 0.1),
                strip.background = element_blank(),
                strip.text = element_text("gg"),
                plot.title = element_text(hjust = 0)
        )
        + xlab("WFPS  [%]")
        + ylab("occurrence [%]")
        + labs(title="(b)")
        #                 + coord_cartesian(xlim = c(0, 100))
        + scale_x_continuous(breaks = c(0, 25, 50, 75, 100), limits=c(0,105))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.2 <- (g.2
        + geom_ribbon(data=myd,aes(ymin=0, ymax=100*y), fill="grey90")
        + geom_density(adjust =1/2, aes(colour = tillage,y = 100*..density..))
        + scale_colour_manual(values = c("black","red"))
        + coord_flip()
        + geom_hline(yintercept=0, size = 0.5)
        
        + geom_segment(data=legend, mapping = aes(x=y,y=x,yend=xend,xend=yend), size=0.5, colour=legend$col)
        + geom_text(data=legend, mapping = aes(x=y,y=x+1, label=label),
                    hjust= 0, vjust=0.5, size=2)
        #         + geom_histogram(aes(y = ..density..), binwidth = 0.1)
        #         
        #         + geom_histogram(data=to.plot.tt, aes(y = ..density..), binwidth = 1, fill="red", alpha=0.5)
        #         + geom_histogram(data=to.plot.nt, aes(y = -..density..), binwidth = 1, fill="grey50", alpha=0.5)
)
# g.2


myplot <- paste0(folder_out,"/compilation_WFPS_0_balanced_3h_interp.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1, vp = vplayout(1, 1))
print(g.2, vp = vplayout(1, 2))

dev.off()
