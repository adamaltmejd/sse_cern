library(readxl)
library(data.table)
library(ggplot2)
library(dplyr)

# Download latest data from Arbetsf�rmedlingen Jan-March
download.file("https://arbetsformedlingen.se/download/18.47a458fb16df81b9133d5f5/1581081168259/varsel-riket-2020-01.xls",
              destfile = file.path("data", "AF", "AF_Jan.xls"), method = "curl", extra = c("-L"), quiet = FALSE)

download.file("https://arbetsformedlingen.se/download/18.2bef8e33170a57d9565432/1583502368823/varsel-riket-2020-02.xls",
              destfile = file.path("data", "AF", "AF_Feb.xls"), method = "curl", extra = c("-L"), quiet = FALSE)

download.file("https://arbetsformedlingen.se/download/18.2bef8e33170a57d956585b8/1586106682398/varsel-riket_2020-03.xls",
              destfile = file.path("data", "AF", "AF_Mar.xls"), method = "curl", extra = c("-L"), quiet = FALSE)

download.file("https://arbetsformedlingen.se/download/18.2bef8e33170a57d9565a51d/1587358931112/Varsel-riket_2020-04-17.xls",
              destfile = file.path("data", "AF", "AF_Apr.xls"), method = "curl", extra = c("-L"), quiet = FALSE)

# Store relevant data
destfile <- file.path("data", "AF", "AF_Jan.xls")
AF_Jan <-  data.table((read_excel(destfile, sheet = 1, range="A7:D26", col_types = c("text","text","numeric","numeric"))))
setnames(AF_Jan, c("SNI", "Name", "N_not", "N"))
AF_Jan$Month <- "January"

destfile <- file.path("data", "AF", "AF_Feb.xls")
AF_Feb <-  data.table((read_excel(destfile, sheet = 1, range="A7:D26", col_types = c("text","text","numeric","numeric"))))
setnames(AF_Feb, c("SNI", "Name", "N_not", "N"))
AF_Feb$Month <- "February"

destfile <- file.path("data", "AF", "AF_Mar.xls")
AF_Mar <-  data.table((read_excel(destfile, sheet = 1, range="A7:D26", col_types = c("text","text","numeric","numeric"))))
setnames(AF_Mar, c("SNI", "Name", "N_not", "N"))
AF_Mar$Month <- "March"

destfile <- file.path("data", "AF", "AF_Apr.xls")
AF_Apr <-  data.table((read_excel(destfile, sheet = 1, range="A12:D31", col_types = c("text","text","numeric","numeric"))))
setnames(AF_Apr, c("SNI", "Name", "N_not", "N"))
AF_Apr$Month <- "April"

AF <- rbind(AF_Jan,AF_Feb,AF_Mar,AF_Apr)
AF$N[is.na(AF$N)] <- 0

AF$Month <- factor(AF$Month, levels = c("April", "March", "February", "January"))

AF$SNI2[AF$SNI == "A"] <- "Agriculture"
AF$SNI2[AF$SNI == "B"] <- "Mining"
AF$SNI2[AF$SNI == "C"] <- "Manufacturing"
AF$SNI2[AF$SNI == "D"] <- "Electricity"
AF$SNI2[AF$SNI == "E"] <- "Water supply"
AF$SNI2[AF$SNI == "F"] <- "Construction"
AF$SNI2[AF$SNI == "G"] <- "Wholesale and retail"
AF$SNI2[AF$SNI == "H"] <- "Transportation"
AF$SNI2[AF$SNI == "I"] <- "Hotel and restaurant"
AF$SNI2[AF$SNI == "J"] <- "Information"
AF$SNI2[AF$SNI == "K"] <- "Finance"
AF$SNI2[AF$SNI == "L"] <- "Real estate"
AF$SNI2[AF$SNI == "M"] <- "Professional services"
AF$SNI2[AF$SNI == "N"] <- "Administration"
AF$SNI2[AF$SNI == "O"] <- "Public administration"
AF$SNI2[AF$SNI == "P"] <- "Education"
AF$SNI2[AF$SNI == "Q"] <- "Health"
AF$SNI2[AF$SNI == "R"] <- "Recreation"
AF$SNI2[AF$SNI == "S"] <- "Other services"

p <- ggplot(AF, aes(SNI2, N, group = Month)) +
  geom_col(aes(fill = Month)) +
labs(title = "Advance layoff notifications in 2020",
     caption = paste0("Source: Arbetsförmedlingen. Updated: 2020-04-17."),
     x = " ",
     y = "Number of employees notified") + theme_linedraw() + theme(panel.border = element_blank(),
          panel.grid.major = element_line(linetype = "dotted", color = "grey60", size = 0.2),
          panel.grid.minor = element_line(linetype = "dotted", color = "grey80", size = 0.2),
          axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = file.path("docs", "layoff.png"), plot = p,
       height = 6, width = 10, units="in", dpi = 300,
       bg = "transparent")
