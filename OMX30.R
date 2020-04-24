require(readr)
require(data.table)
require(ggplot2)
require(dplyr)

# Download latest Google Community Mobility data
download.file(paste0("https://query1.finance.yahoo.com/v7/finance/download/%5EOMX?period1=1555616072&period2=",  as.numeric(as.POSIXct(Sys.Date(), format="%Y-%m-%d")), "&interval=1d&events=history"),
              destfile = file.path("data", "OMX30", "OMX30.csv"), method = "curl", extra = c("-L"), quiet = FALSE)

# Store relevant data
destfile <- file.path("data", "OMX30", "OMX30.csv")
OMX30 <-  data.table(read_csv(destfile))
OMX30[, Date := as.Date(Date, "%y-%m-%d")]

OMX30$lastclose[length(OMX30$Close)] <- last(OMX30$Close)

lastdate <- max(OMX30$Date)

# Create plot
p <- ggplot(OMX30, aes(x=Date)) +
  geom_line(aes(y = Close), color = "red", size = 1) + geom_point(aes(y = lastclose), color = "red") +
  geom_hline(yintercept=last(OMX30$Close), linetype="dashed", color = "red") +
   scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +

  labs(title = "NASDAQ Stockholm OMX-30 Index",
       caption = paste0("Source: Yahoo! Finance. Updated: ", lastdate,"."),
       x = " ",
       y = "OMX-30 Index") + theme_linedraw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(linetype = "dotted", color = "grey60", size = 0.2),
        panel.grid.minor = element_line(linetype = "dotted", color = "grey80", size = 0.2),
        plot.title = element_text(size = 20))

ggsave(filename = file.path("docs", "OMX30.png"), plot = p,
       height = 6, width = 10, units="in", dpi = 300,
       bg = "transparent")
