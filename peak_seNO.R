t <- 1 # times se
limits <- aes(ymax = count.cumNO.mgNm2 + t*se.count.cumNO.mgNm2, ymin= count.cumNO.mgNm2 - t*se.count.cumNO.mgNm2)
################################################################################
# by rain pattern
g.1 <- ggplot(data, aes(x = precipitation, y=count.cumNO.mgNm2, fill=precipitation))
g.1 <- (g.1
        + facet_wrap( ~ labelP, scales = "fixed", ncol = 6)
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin = unit(0.1, "lines"),
                strip.background = element_blank(),
                #                         strip.text.x = element_blank(),
                plot.title = element_text(hjust = 0)
        )
        
        + ylab("NO peak occurrence   [days]")
        + labs(title = "(a)")
)

g.1 <- (g.1
        + geom_crossbar(limits, width=0.5, size=0.25)
        + scale_fill_manual(values = col.rain)
        + geom_point(data = NO, aes(x = precipitation, y=df.count.cumNO.mgNm2, colour=factor(incubation)),
                     position=position_dodge(width=0.75), size = 1.5 )
        + scale_colour_manual(values=col.inc)
)
g.1

g.2 <- ggplot(data.P, aes(x = precipitation, y=count.cumNO.mgNm2, fill=precipitation))
g.2 <- (g.2
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(1.1, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin = unit(0.1, "lines"),
                #                 strip.background = element_blank(),
                #                         strip.text.x = element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("")
        + labs(title = "(b)")
)

g.2 <- (g.2
        + geom_crossbar(limits, width=0.5, size=0.25)
        + scale_fill_manual(values = col.rain, name="Rain pattern")
        + geom_point(data = NO, aes(x = precipitation, y=df.count.cumNO.mgNm2, colour=factor(incubation)),
                     position=position_dodge(width=0.75), size = 1.5 )
        + scale_colour_manual(values=col.inc)
)
g.2
#
myplot <- paste0(myplots,"/_se_15N_rain.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 10)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1, vp = vplayout(1, 1:8))
print(g.2, vp = vplayout(1, 9:10))

dev.off()

################################################################################
# by fert
g.1 <- ggplot(data, aes(x = fertilization, y=count.cumNO.mgNm2, fill=fertilization))
g.1 <- (g.1
        + facet_wrap( ~ labelF, scales = "fixed", ncol = 6)
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin = unit(0.1, "lines"),
                strip.background = element_blank(),
                #                         strip.text.x = element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("NO peak occurrence   [days]")
        + labs(title = "(a)")
)

g.1 <- (g.1
        + geom_crossbar(limits, width=0.5, size=0.25)
        + scale_fill_manual(values = col.fert)
        + geom_point(data = NO, aes(x = fertilization, y=df.count.cumNO.mgNm2, colour=factor(incubation)),
                     position=position_dodge(width=0.75), size = 1.5 )
        + scale_colour_manual(values=col.inc)
)

g.2 <- ggplot(data.F, aes(x = fertilization, y=count.cumNO.mgNm2, fill=fertilization))
g.2 <- (g.2
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(1.1, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin = unit(0.1, "lines"),
                #                 strip.background = element_blank(),
                #                         strip.text.x = element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("")
        + labs(title = "(b)")
)

g.2 <- (g.2
        + geom_crossbar(limits, width=0.5, size=0.25)
        + scale_fill_manual(values = col.fert)
        + geom_point(data = NO, aes(x = fertilization, y=df.count.cumNO.mgNm2, colour=factor(incubation)),
                     position=position_dodge(width=0.75), size = 1.5 )
        + scale_colour_manual(values=col.inc)
)
g.2
#
myplot <- paste0(myplots,"/_se_15N_fert.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 10)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1, vp = vplayout(1, 1:8))
print(g.2, vp = vplayout(1, 9:10))

dev.off()

################################################################################
# by tillage
g.1 <- ggplot(data, aes(x = tillage, y = count.cumNO.mgNm2, fill = tillage))
g.1 <- (g.1
        + facet_wrap( ~ labelT, scales = "fixed", ncol = 9)
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin = unit(0.1, "lines"),
                strip.background = element_blank(),
                #                         strip.text.x = element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("NO peak occurrence   [days]")
        + labs(title = "(a)")
)

g.1 <- (g.1
        + geom_crossbar(limits, width=0.5, size=0.25)
        + scale_fill_manual(values = col.till)
        + geom_point(data = NO, aes(x = tillage, y=df.count.cumNO.mgNm2, colour=factor(incubation)),
                     position=position_dodge(width=0.75), size = 1.5 )
        + scale_colour_manual(values=col.inc)
)

g.2 <- ggplot(data.T, aes(x = tillage, y=count.cumNO.mgNm2, fill=tillage))
g.2 <- (g.2
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(1.1, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                panel.margin = unit(0.1, "lines"),
                #                 strip.background = element_blank(),
                #                         strip.text.x = element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab("")
        + labs(title = "(b)")
)

g.2 <- (g.2
        + geom_crossbar(limits, width=0.5, size=0.25)
        + scale_fill_manual(values = col.till)
        + geom_point(data = NO, aes(x = tillage, y=df.count.cumNO.mgNm2, colour=factor(incubation)),
                     position=position_dodge(width=0.75), size = 1.5 )
        + scale_colour_manual(values=col.inc)
)
g.2
#
myplot <- paste0(myplots,"/_se_15N_tillage.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 10)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1, vp = vplayout(1, 1:8))
print(g.2, vp = vplayout(1, 9:10))

dev.off()