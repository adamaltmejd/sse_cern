setwd("C:/Users/nero/Documents/covid")
workdir <- getwd()

require(readxl)
require(data.table)
require(ggplot2)
require(dplyr)

# Download latest Socialstyrelsen data and save Excel file
download.file("https://www.socialstyrelsen.se/globalassets/sharepoint-dokument/dokument-webb/statistik/statistik-socialstyrelsen",
              destfile = file.path(workdir, "data", "SS", "SS_latest.xlsx"), method = "curl", extra = c("-L"), quiet = FALSE)

# Store relevant data
destfile <- file.path(workdir, "data", "SS", "SS_latest.xlsx")
SS <-  data.table((read_excel(destfile, sheet = 2, skip=4, n_max = 52, col_types = c("numeric"))))
setnames(SS, c("wk", "y15", "y16", "y17", "y18", "y19", "y20"))

# Plot up until current week
current_wk <- strtoi(format(Sys.Date(), "%V"))
plotdata <- subset(SS, wk <= current_wk + 10)

temp <- complete.cases(SS)*SS
lastweek <- max(temp$wk)

# Plot death toll
p <- ggplot(plotdata, aes(x=wk)) + 
  geom_line(aes(y = y20), color = "red") + geom_point(aes(y=y20), color = "red") +
  geom_line(aes(y = y19), color = "grey") +
  geom_line(aes(y = y18), color = "grey") +
  geom_line(aes(y = y17), color = "grey") +
  geom_line(aes(y = y16), color = "grey") +
  geom_line(aes(y = y15), color = "grey") +
  labs(title = "Weekly total number of deaths in Sweden 2015-2019 (grey) and 2020 (red)",
     caption = paste0("Source: Socialstyrelsen. Updated: Week ", lastweek, " 2020."),
       x = "Week",
     y = "Weekly number of deaths") + theme_linedraw() + theme(panel.border = element_blank(),
  panel.grid.major = element_line(linetype = "dotted", color = "grey60", size = 0.2),
  panel.grid.minor = element_line(linetype = "dotted", color = "grey80", size = 0.2))

ggsave(filename = "deaths.png", plot = p,
       height = 6, width = 10, units="in", dpi = 300,
       bg = "transparent")
