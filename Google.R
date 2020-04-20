setwd("C:/Users/nero/Documents/covid")
workdir <- getwd()

require(readr)
require(data.table)
require(ggplot2)
require(dplyr)

# Download latest Google Community Mobility data
download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
              destfile = file.path(workdir, "data", "Google", "Google.csv"), method = "curl", extra = c("-L"), quiet = FALSE)

# Store relevant data
destfile <- file.path(workdir, "data", "Google", "Google.csv")
Google <-  data.table(read_csv(destfile))
Google_SE <- subset(Google, country_region_code == "SE" & is.na(sub_region_1)==1)
Google_SE[, date := as.Date(date, "%y-%m-%d")]

# Create plot
p <- ggplot(Google_SE, aes(x=date)) + 
  geom_line(aes(y = parks_percent_change_from_baseline, colour = "Parks")) +
  geom_line(aes(y = residential_percent_change_from_baseline, colour = "Residential")) +
  geom_line(aes(y = grocery_and_pharmacy_percent_change_from_baseline, colour = "Grocery & pharmacy")) +
  geom_line(aes(y = retail_and_recreation_percent_change_from_baseline, colour = "Retail & recreation")) +
  geom_line(aes(y = workplaces_percent_change_from_baseline, colour = "Workplace")) +
  geom_line(aes(y = transit_stations_percent_change_from_baseline, colour = "Transit stations")) +
  scale_colour_manual("", 
                      breaks = c("Parks", "Residential", "Grocery & pharmacy", "Retail & recreation", "Workplace", "Transit stations"),
                      values = c("red", "blue", "green", "orange", "black", "grey")) +
  xlab(" ") +
  labs(title = "Google Community Mobility Index for Sweden",
       caption = paste0("Source: Google. Updated: ", Sys.Date(),"."),
       x = " ",
       y = "Change relative to baseline (%)") + theme_linedraw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(linetype = "dotted", color = "grey60", size = 0.2),
        panel.grid.minor = element_line(linetype = "dotted", color = "grey80", size = 0.2))

ggsave(filename = "google.png", plot = p,
       height = 6, width = 10, units="in", dpi = 300,
       bg = "transparent")
