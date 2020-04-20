require(readxl)
require(data.table)
require(ggplot2)
require(dplyr)

# Download latest FHM data and save Excel file
download.file("https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data",
              destfile = file.path("data", "FHM", "FHM_latest.xlsx"), method = "curl", extra = c("-L"), quiet = FALSE)

# Store deaths, cases and ICUs
destfile <- file.path("data", "FHM", "FHM_latest.xlsx")
Dead <- data.table((read_excel(destfile, sheet = 2, col_types = c("text", "numeric"))))
Case <- data.table((read_excel(destfile, sheet = 1, range = cell_cols("A:B"), col_types = c("text", "numeric"))))
ICU <- data.table((read_excel(destfile, sheet = 3, col_types = c("text", "numeric"))))

# Fix dates
setnames(Dead, c("date", "N_dead"))
Dead[date == "Uppgift saknas" | date == "uppgift saknas", date := NA]
Dead[, date := as.Date(as.numeric(date), origin = "1899-12-30")]

setnames(Case, c("date", "N_case"))
Case[, date := as.Date(as.numeric(date), origin = "1899-12-30")]

setnames(ICU, c("date", "N_ICU"))
ICU[, date := as.Date(as.numeric(date), origin = "1899-12-30")]

temp <- full_join(Dead, Case, by = "date")

data <- full_join(temp, ICU, by = "date")

data <- arrange(data, date)
setorder(data, date, na.last=TRUE)

data$N_case[is.na(data$N_case)] <- 0
data$N_dead[is.na(data$N_dead)] <- 0
data$N_ICU[is.na(data$N_ICU)] <- 0

data$N_dead_cum <- cumsum(data$N_dead)
data$N_case_cum <- cumsum(data$N_case)
data$N_ICU_cum <- cumsum(data$N_ICU)

data$N_dead_cum_lag <- shift(data$N_dead_cum, 1L, type="lag")
data$N_case_cum_lag <- shift(data$N_case_cum, 1L, type="lag")
data$N_ICU_cum_lag <- shift(data$N_ICU_cum, 1L, type="lag")

data$N_dead_growth <- 100 * (data$N_dead_cum / data$N_dead_cum_lag - 1)
data$N_case_growth <- 100 * (data$N_case_cum / data$N_case_cum_lag - 1)
data$N_ICU_growth <- 100 * (data$N_ICU_cum / data$N_ICU_cum_lag - 1)

plotdata <- subset(data, date >= "2020-03-17")

# Plot death toll
p <- ggplot(plotdata, aes(x=date)) +
  geom_line(aes(y = N_dead_growth, colour = "Deaths")) +
  geom_line(aes(y = N_case_growth, colour = "Cases")) +
  geom_line(aes(y = N_ICU_growth, colour = "ICU")) +
  scale_colour_manual("", breaks = c("Deaths", "Cases", "ICU"), values = c("red", "black", "orange")) +
  xlab(" ") +
 labs(title = "Daily growth rate total Swedish Covid-19 cases, ICU and deaths",
       caption = paste0("Source: Folkh�lsomyndigheten. Updated: ", Sys.Date(),"."),
       x = " ",
       y = "Daily growth rate (%)") + theme_linedraw() + theme(panel.border = element_blank(),
 panel.grid.major = element_line(linetype = "dotted", color = "grey60", size = 0.2),
 panel.grid.minor = element_line(linetype = "dotted", color = "grey80", size = 0.2))

ggsave(filename = file.path("docs", "growth.png"), plot = p,
       height = 6, width = 10, units="in", dpi = 300,
       bg = "transparent")


