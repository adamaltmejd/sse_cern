require(readxl)
require(data.table)
require(ggplot2)
require(dplyr)

# Store relevant data (update manually weekly)
destfile <- file.path("data", "AF", "AF_unemp.xlsx")
AF2 <-  data.table((read_excel(destfile, sheet = 1, range="A1:D53", col_types = c("numeric"))))
setnames(AF2, c("wk", "y18", "y19", "y20"))

# Plot up until current week
current_wk <- strtoi(format(Sys.Date(), "%V"))
plotdata <- subset(AF2, wk <= current_wk + 10)

temp <- complete.cases(AF2)*AF2
lastweek <- max(temp$wk)

p <- ggplot(plotdata, aes(x=wk)) +

    geom_line(aes(y = y20, colour = "y2020"), size = 1) + geom_point(aes(y=y20), color = "red") +
    geom_line(aes(y = y19, colour = "y2019"), size = 1)  +
    geom_line(aes(y = y18, colour = "y2018"), size = 1)  +
    scale_colour_manual("", breaks = c("y2020", "y2019", "y2018"), values = c("red", "black", "grey"), labels = c("2020", "2019", "2018")) +

  scale_x_continuous(breaks = seq(1,current_wk+10,by = 1)) +  xlab(" ") +
  labs(title = "Number unemployed registered at Swedish Public Employment Service",
     caption = paste0("Source: Swedish Public Employment Service. Updated: Week ", lastweek, " 2020."),
       x = "Week",
     y = "Number of unemployed") + theme_linedraw() + theme(panel.border = element_blank(),
  panel.grid.major = element_line(linetype = "dotted", color = "grey60", size = 0.2),
  panel.grid.minor = element_line(linetype = "dotted", color = "grey80", size = 0.2),
  plot.title = element_text(size = 20))

ggsave(filename = file.path("docs", "unemp.png"), plot = p,
       height = 6, width = 10, units="in", dpi = 300,
       bg = "transparent")
