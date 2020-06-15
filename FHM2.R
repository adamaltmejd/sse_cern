require(readxl)
require(data.table)
require(ggplot2)
require(dplyr)

can_be_numeric <- function(x) {
    # Check if vector can be converted to numeric
    stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
    numNAs <- sum(is.na(x))
    numNAs_new <- suppressWarnings(sum(is.na(as.numeric(x))))
    return(numNAs_new == numNAs)
}

# Download today's FHM data and save Excel file
download.file("https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data",
              destfile = file.path("data", "FHM", paste0("Folkhalsomyndigheten_Covid19_",Sys.Date(),".xlsx")), method = "curl", extra = c("-L"), quiet = FALSE)

Dead <- data.table((read_excel(file.path("data", "FHM", paste0("Folkhalsomyndigheten_Covid19_",Sys.Date(),".xlsx")), sheet = 2, col_types = c("text", "numeric"))))
Dead$ReportDate <- Sys.Date()
setnames(Dead, c("Date", "N", "ReportDate"))
Dead[Date == "Uppgift saknas" | Date == "uppgift saknas", Date := NA]
Dead[, Date := as.Date(as.numeric(Date), origin = "1899-12-30")]

# Load old FHM data
dates <- seq(as.Date("2020-04-02"), as.Date(Sys.Date()-1), by=1)

for (d in dates) {
    varname <- paste0("rep_", as.Date(d,origin="1970-01-01"))
    destfile <- file.path("data", "FHM", paste0("Folkhalsomyndigheten_Covid19_",as.Date(d,origin="1970-01-01"),".xlsx"))
    temp <- data.table((read_excel(destfile, sheet = 2, col_types = c("text", "numeric"))))
    setnames(temp, c("Date", "N"))
    temp[Date == "Uppgift saknas" | Date == "uppgift saknas" | Date == "Uppgift saknaa", Date := NA]
    if (can_be_numeric(temp[, Date])) {
        temp[, Date := as.Date(as.numeric(Date), origin = "1899-12-30")]
    } else {
        temp[, Date := as.Date(Date)]
    }
    temp$ReportDate <- as.Date(d,origin="1970-01-01")
    Dead <- rbind(Dead,temp)
}

setnames(Dead, c("Date", "N", "ReportDate"))
Dead[, Date := as.Date(as.numeric(Date), origin = "1970-01-01")]
Dead[, ReportDate := as.Date(as.numeric(ReportDate), origin = "1970-01-01")]

Dead[is.na(N), N := 0]

plotdata <- subset(Dead, Date >= "2020-03-17")
plotdata$Ntoday <- plotdata$N
plotdata[ReportDate < as.Date(Sys.Date()), Ntoday := NA]

p <- ggplot(plotdata, aes(x=Date, group = ReportDate)) +
    geom_line(aes(y=N, color = ReportDate), size = 1) + scale_colour_date(name = "Reporting date", low = "yellow", high = "red") +
    geom_line(aes(y=Ntoday, lty=format(Sys.time(), "%b %d")), color="black", size = 1.5) +
    scale_linetype(name = "Latest report") +
    scale_x_date(date_breaks = "5 day", date_labels = "%d/%m") +
    xlab(" ") +
    labs(title = "Swedish Covid-19 mortality: Death dates by reporting date",
         caption = paste0("Source: Public Health Agency of Sweden. Updated: ", Sys.Date(),"."),
         x = "Date of death",
         y = "Daily number of Covid-19 deaths") + theme_linedraw() + theme(panel.border = element_blank(),
          panel.grid.major = element_line(linetype = "dotted", color = "grey60", size = 0.2),
             panel.grid.minor = element_line(linetype = "dotted", color = "grey80", size = 0.2),
             plot.title = element_text(size = 20))

ggsave(filename = file.path("docs", "reportdeath.png"), plot = p,
       height = 6, width = 10, units="in", dpi = 300,
       bg = "transparent")
