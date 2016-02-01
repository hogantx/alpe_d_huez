setwd("C:/Users/bhogan/Documents/R/Data")
library(ggplot2)
library(scales)
library(ggthemes)
library(dplyr)
library(XML)
library(scrapeR)
library(stringr)

### websites with ascent times
http://www.climbing-records.com/2014/07/new-hautacam-top-100-nibali-close-to.html
http://www.climbing-records.com/2013/07/five-fresh-names-in-all-time-top-100.html

### read latest list of adh ascent times
mps <- ("http://www.climbing-records.com/2013/07/five-fresh-names-in-all-time-top-100.html")
tmp <- readLines(mps)
tmp[1012:1111]

### create data frame
adh <- data.frame(text = character(100),
  year = integer(100),
  minutes = integer(100),
  seconds = integer(100),
  time = numeric(100),
  name = character(100),
  status = character(100),
  stringsAsFactors=FALSE)

### populate adh data frame from text
adh$text <- tmp[1012:1111]
adh$minutes <- as.integer(substr((str_extract(adh$text, "\\d{2}:\\d{2}")), 1, 2))
adh$seconds <- as.integer(substr((str_extract(adh$text, "\\d{2}:\\d{2}")), 4, 5))
adh$time <- adh$minutes +  (adh$seconds/60)

### 2013-2015 entries have particular encoding
adh$text <- gsub("span style", "", adh$text)
adh$name <- (str_extract(adh$text, "\\w{3,}\\s\\w{3,}"))

adh$text <- gsub("#38761", "", adh$text)
adh$year <- as.integer(str_extract(adh$text, "\\d{4}"))

### Wiki of doping
doping <- "https://en.wikipedia.org/wiki/List_of_doping_cases_in_cycling"
tmp2 <- readLines(doping)
str(tmp2)

### Paste wiki to one string
tmp3 <- ""
for(i in 1:2661)
{
tmp3 <- paste0(tmp3, tmp2[i])
}

### Search riders in wiki string
for(i in 1:100)
{
adh$status[i] <- grepl(adh$name[i], tmp3)
}

### Correct NA for Zulle and Schleck
adh$status[49] <- 1
adh$status[80] <- 1

adh$status <- ifelse((adh$status == 1 | adh$status == TRUE),
  "Suspected", 
  "Not Suspected")


### Set image dimension
dev.new(width=10, height=5)

### Plot
ggplot(adh, aes(year, time, colour=status, shape=status)) +
  geom_point(position = "jitter") + geom_rug(position = "jitter") +
  scale_shape_manual(values=c(3, 20)) + 
  scale_colour_manual(values=c("blue", "black")) 

savePlot("Alpe d'Huez ascent times", "wmf", device = dev.cur())

