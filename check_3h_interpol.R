# NO
p <- qplot(days.adjusted, NO, data=data, color=tillage, facets=precipitation ~ fertilization, size=I(2),
           xlab="", ylab="")
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12)) +
        scale_x_continuous(limits= c(-5,45), breaks = c(0,15,30,45))


# N2O
p <- qplot(days.adjusted, N2O, data=data, color=tillage, facets=precipitation ~ fertilization, size=I(2),
           xlab="", ylab="")
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12)) +
        scale_x_continuous(limits= c(-5,45), breaks = c(0,15,30,45))

# CO2
p <- qplot(days.adjusted, CO2, data=data, color=tillage, facets=precipitation ~ fertilization, size=I(2),
           xlab="", ylab="")
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12)) +
        scale_x_continuous(limits= c(-5,45), breaks = c(0,15,30,45))

# CH4
p <- qplot(days.adjusted, CH4, data=data, color=tillage, facets=precipitation ~ fertilization, size=I(2),
           xlab="", ylab="")
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12)) +
        scale_x_continuous(limits= c(-5,45), breaks = c(0,15,30,45))


# WFPS
p <- qplot(days.adjusted, WFPS, data=data, color=tillage, facets=precipitation ~ fertilization, size=I(2),
           xlab="", ylab="")
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12)) +
        scale_x_continuous(limits= c(-5,45), breaks = c(0,15,30,45))

# WFPS_0
p <- qplot(days.adjusted, WFPS_0, data=data, color=tillage, facets=precipitation ~ fertilization, size=I(2),
           xlab="", ylab="")
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12)) +
        scale_x_continuous(limits= c(-5,45), breaks = c(0,15,30,45))

# WFPS_f
p <- qplot(days.adjusted, WFPS_f, data=data, color=tillage, facets=precipitation ~ fertilization, size=I(2),
           xlab="", ylab="")
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12)) +
        scale_x_continuous(limits= c(-5,45), breaks = c(0,15,30,45))