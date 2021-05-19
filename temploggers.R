#To open excel in R
library(openxlsx)
# To plot using ggplot2
library(ggplot2)
#To plot side by side or on top of each other
library(gridExtra)
#To use date_break functinoallity
library(scales)
library(lubridate)
library(rkt)
library(zoo)
library(plotly)
library(EnvStats)
library(tidyverse)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(gridExtra)
#path of spreadsheet
dbPath1 <- "All_Loggers.xlsx"

# commit
#load the worksheets
NC <-  read.xlsx(dbPath1, "NC", detectDates = T)
HC <-  read.xlsx(dbPath1, "HC", detectDates = T)
PA <-  read.xlsx(dbPath1, "PA", detectDates = T)
WC <-  read.xlsx(dbPath1, "WC", detectDates = T)


#combine worksheets

AllCounts<- rbind(NC, HC, PA, WC)

#add logical for above 16

AllCounts$Temperature.Above.16 <- AllCounts$Seven_DADMax>16
AllCounts$Temp.Type <- ifelse(AllCounts$Seven_DADMax < 16, "below", "above")

#Max and Min Temperatures
AllCounts$Date <- mdy(AllCountsHC$Date)
AllCounts$Temp <- as.numeric(AllCountsHC$Temp)
AllCounts$Month <- month(AllCounts$Date, label = TRUE)
MaxDailyTemp <- aggregate(Temp ~ Date, data = AllCountsHC, FUN = max)

#Add Months to HC_MaxDailyTemp for Seasonall Kendall
MaxDailyTemp$Month <- month(MaxDailyTemp$Date, label = TRUE)
MaxDailyTemp$Year <- year(MaxDailyTemp$Date)

# FinallY the plot worked  
HC_Temp <- ggplot(HC_MaxDailyTemp, aes(x=Date, y=Temp, group = 1)) + geom_point(size=1, shape=21, fill="white") + geom_hline(yintercept = 16, color = "red") +
  labs(title="Horse Creek Temperature  Over Time", subtitle="From 2010-2017") + geom_smooth(method = "loess") +
  scale_x_date(breaks = date_breaks("3 months"), labels = date_format("%b-%y")) + theme(axis.text.x = element_text(angle = 90)) + 
  theme(plot.background = element_rect(fill = "light yellow"))
plot(HC_Temp)

MaxDailyTemp$Seven_DADMax <- rollapply(MaxDailyTemp$Temp, width = 7, mean, partial = TRUE, align = "right")


#ggplot 7 Day Average Daily Max (7-DADMax)
All_DADMax <- ggplot(AllCounts, aes(x=Date, y=Seven_DADMax)) + geom_point(size=.8, shape=21, fill="white") + geom_hline(yintercept = 16, color = "red") + facet_wrap(~Site, nrow = 4) +
  labs(title=NULL, subtitle=NULL, y="7 Day Avg Daily Max Temperature (Celcius)", caption="Source: Continuous Temperature Loggers, 2011-2017") + 
  geom_smooth(method = "glm") + scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b-%y")) +theme_bw() + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
        axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15))
plot(All_DADMax) 

#Seasonall Mann Kendall For Temp.
rkt(HC_MaxDailyTemp$Year, HC_MaxDailyTemp$Temp, block = HC_MaxDailyTemp$Month, correct = TRUE, rep = "a")


# bar chart showing percentage of days above 7-DADMAx!
g <- ggplot(AllCounts, aes(Site)) + geom_bar(aes(fill=Temperature.Above.16), width = 0.5) +
  labs(title = NULL, subtitle=NULL, y="Days") + 
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15))
g
g + scale_fill_discrete(name="Temperature",
                        breaks=c("FALSE","TRUE"),
                        labels=c("< 16 Degrees C","> 16 Degrees C"))

#cdfplot
ggplot(AllCounts, aes(Seven_DADMax, group=Site, colour=Site)) + stat_ecdf() + geom_vline(xintercept = 16, color = "red") +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) + 
  labs(title="Days Above Temperature Water Quality Standards", subtitle="2011-2017, Continuous Temperature Loggers", x="Temperature (Degrees Celcius)", y= "Cumulative Distributiion Frequency") +
  geom_point(x=16, y=.71, colour="darkseagreen4") + geom_point(x=16, y=.69, colour="royalblue") +
  geom_point(x=16, y=.805, colour="red") + geom_point(x=16, y=.835, colour="purple")



#summaryStats
summaryStats(Temperature.Above.16 ~ Site, data = AllCounts)

#_____________________________FINAL GRAPHS___________________________________
tiff("ROlling_7_DADMAX_TMDL.tiff", units="in", width=12, height=8, res=300)
ggplot(AllCounts, aes(x=Date, y=Seven_DADMax, group = Site, ,color = Temp.Type)) + geom_point(size=.8, shape=21, fill="white") + geom_hline(yintercept = 16, color = "red") + facet_wrap(~Site, nrow = 4) +
  labs(title=NULL, subtitle=NULL, y="7 Day Avg Daily Max Temperature (Celcius)", caption="Source: Continuous Temperature Loggers, 2011-2017") + 
  geom_smooth(method = "glm") + scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b-%y")) +theme_bw() + 
  theme(axis.title.y = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 16, angle = 65, vjust = 0.6),
        axis.text.y = element_text(size = 16), legend.title = element_text(size=25), legend.text = element_text(size=22), axis.title.x = element_text(size = 22), legend.position = "none",  strip.text.x = element_text(size=12, colour = "steelblue3"))
dev.off()

tiff("Days_Over_16_bar.tiff", units="in", width=16, height=12, res=300)
ggplot(AllCounts, aes(Site)) + geom_bar(aes(fill=Temperature.Above.16), width = 0.5) +
  labs(title = NULL, subtitle=NULL, y="Days") + 
  theme_bw() + theme(axis.title.y = element_text(size=27), title = element_text(size = 18), axis.text.x = element_text(size = 22, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 22), axis.title.x = element_text(size = 27), legend.title = element_text(size=22), legend.text = element_text(size=22)) + scale_fill_discrete(name="Temperature",
                                                                                                                                                                                                     breaks=c("FALSE","TRUE"),
                                                                                                                                                                                                     labels=c("< 16 Degrees C","> 16 Degrees C"))
dev.off()

tiff("Days_Over_16_cfd.tiff", units="in", width=16, height=10, res=300)
ggplot(AllCounts, aes(Seven_DADMax, group=Site, colour=Site)) + stat_ecdf() + geom_vline(xintercept = 16, color = "red") +
  theme_bw() + theme(axis.title.y = element_text(size=27), title = element_text(size = 27), axis.text.x = element_text(size = 22, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 22), legend.title = element_text(size=22), legend.text = element_text(size=22)) + 
  labs(title="Days Above Temperature Water Quality Standards", subtitle="2011-2017, Continuous Temperature Loggers", x="Temperature (Degrees Celcius)", y= "Cumulative Distributiion Frequency") +
  geom_point(x=16, y=.71, colour="darkseagreen4") + geom_point(x=16, y=.69, colour="royalblue") +
  geom_point(x=16, y=.805, colour="red") + geom_point(x=16, y=.835, colour="purple")

dev.off()

a = ggplot(AllCounts, aes(x=Date, y=Seven_DADMax, group = Site, ,color = Temp.Type)) + geom_point(size=.8, shape=21, fill="white") + geom_hline(yintercept = 16, color = "red") + facet_wrap(~Site, nrow = 4) +
  labs(title=NULL, subtitle=NULL, y="7 Day Avg Daily Max Temperature (Celcius)", caption="Source: Continuous Temperature Loggers, 2011-2017") + 
  geom_smooth(method = "glm") + scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b-%y")) +theme_bw() + 
  theme(axis.title.y = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 16, angle = 65, vjust = 0.6),
        axis.text.y = element_text(size = 16), legend.title = element_text(size=25), legend.text = element_text(size=22), axis.title.x = element_text(size = 22), legend.position = "none",  strip.text.x = element_text(size=12, colour = "steelblue3"))

ggplotly(a)

#ridge density plot for temp!

HC_ridge <- ggplot(subset(AllCounts, Site== "HC"), aes(x = Seven_DADMax, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [C]", option = "C") +
  labs(title = 'Stream Temperatures in Bothell in 2018') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

NC_ridge <- ggplot(subset(AllCounts, Site== "NC"), aes(x = Seven_DADMax, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [C]", option = "C") +
  labs(title = 'Stream Temperatures in Bothell in 2018') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

PA_ridge <- ggplot(subset(AllCounts, Site== "PA"), aes(x = Seven_DADMax, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [C]", option = "C") +
  labs(title = 'Stream Temperatures in Bothell in 2018') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

WC_ridge <- ggplot(subset(AllCounts, Site== "WC"), aes(x = Seven_DADMax, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [C]", option = "C") +
  labs(title = 'Stream Temperatures in Bothell in 2018') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

grid.arrange(HC_ridge, PA_ridge, NC_ridge, WC_ridge)

#with facetwrap
ggplot(AllCounts, aes(x = Seven_DADMax, y = Month, fill = ..x..)) + geom_vline(xintercept = 16, color = "red", size = 0.8) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) + facet_wrap(~Site, ncol = 2) +
  scale_fill_viridis(name = "Temp. [C]", option = "C") +
  labs(title = 'Stream Temperatures in Bothell in 2018') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
