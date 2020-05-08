require(readxl)
require(data.table)
require(ggplot2)
require(dplyr)

# Download latest Socialstyrelsen data and save Excel file
download.file("https://www.socialstyrelsen.se/globalassets/sharepoint-dokument/dokument-webb/statistik/antal-doda-per-100000-per-lan-och-vecka-2015-v1-2020-v17.xlsx",
              destfile = file.path("data", "SS", "SS_latest.xlsx"), method = "curl", extra = c("-L"), quiet = FALSE)

# Store relevant data
destfile <- file.path("data", "SS", "SS_latest.xlsx")
SS <-  data.table((read_excel(destfile, sheet = 2, range="A5:G57", col_types = c("numeric"))))
setnames(SS, c("wk", "y15", "y16", "y17", "y18", "y19", "y20"))
#skip=4, n_max = 52

# Plot up until current week
current_wk <- strtoi(format(Sys.Date(), "%V"))
plotdata <- subset(SS, wk <= current_wk + 10)

temp <- complete.cases(SS)*SS
lastweek <- max(temp$wk)

# Plot death toll
p <- ggplot(plotdata, aes(x=wk)) +
  geom_line(aes(y = y20), color = "red", size = 1) + geom_point(aes(y=y20), color = "red") +
  geom_line(aes(y = y19), color = "grey") +
  geom_line(aes(y = y18), color = "grey") +
  geom_line(aes(y = y17), color = "grey") +
  geom_line(aes(y = y16), color = "grey") +
  geom_line(aes(y = y15), color = "grey") +
  scale_x_continuous(breaks = seq(1,current_wk+10,by = 1)) +
  labs(title = "Weekly number of deaths in Sweden 2015-2019 (grey) and 2020 (red)",
     caption = paste0("Source: National Board of Health and Welfare. Updated: Week ", lastweek, " 2020."),
       x = "Week",
     y = "Weekly number of deaths per 100,000 inhabitants") + theme_linedraw() + theme(panel.border = element_blank(),
  panel.grid.major = element_line(linetype = "dotted", color = "grey60", size = 0.2),
  panel.grid.minor = element_line(linetype = "dotted", color = "grey80", size = 0.2),
  plot.title = element_text(size = 20))

ggsave(filename = file.path("docs", "deaths.png"), plot = p,
       height = 6, width = 10, units="in", dpi = 300,
       bg = "transparent")
