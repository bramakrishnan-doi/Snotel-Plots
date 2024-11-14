# libraries
library(ggplot2)
library(tidyverse)
library(data.table)
library(ggforce)

snow_sv <- data.table::fread(
 "https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC4/1506_Salt.csv"
    )
# Get location of column names that end with a number (years)
cnames <- grep("\\D+$",colnames(snow_sv), invert = TRUE)

# Add the prefix "WY" to the column names selected above
colnames(snow_sv)[cnames] <- paste0("WY", colnames(snow_sv)[cnames])

previousMonth <- floor_date(Sys.Date(), "month") - months(1)



snow_filt_sv <- snow_sv[,.(date,WY2024,WY2025,`Median ('91-'20)`)] %>% 
  mutate(mon = as.numeric(str_extract(date,"^[0-9]{2}")),
         date = as.Date(paste0(date,"-",ifelse(mon>9,1979,1980)), "%m-%d-%Y")) %>% 
  pivot_longer(!c(date,mon), names_to = "WY", values_to = "swe") %>% 
  mutate(WY = case_when(
    WY=="WY2025" ~ "Water Year: 2025",
    WY=="WY2024" ~ "Water Year: 2024",
    .default = "Median 1991-2020"
    ))



curWY <- snow_filt_sv %>%
  filter(WY == "Water Year: 2025" & !is.na(swe)) %>%
  filter(date == max(date))

curWY_maxSWE <- snow_filt_sv %>%
  filter(WY == "Water Year: 2025") %>%
  filter(swe==max(swe, na.rm = T))



med_SWE <- snow_filt_sv %>% filter(str_detect(WY,"Median")) %>%
  filter(date==curWY_maxSWE$date)

# Caption to add after Peak SWE has happened
# caption = paste(strwrap(paste0(
#                         "SWE peaked at ",
#                         round(curWY_maxSWE$swe/med_SWE$swe*100,0),
#                         "% of the peak seasonal median on ",
#                         format(curWY_maxSWE$Date, "%B %d, %Y"),"."),
#                 25), 
#                 collapse = "\n")

# cmswe <- curWY_maxSWE
# cmswe$Label = caption

maxswe <- max(snow_filt_sv$swe, na.rm = TRUE)
ymax <- ifelse((maxswe %% 2)>1, 
                maxswe-(maxswe %% 2) + 4, 
                maxswe -(maxswe %% 2) +2) 

SV_Snowplot <- ggplot(data=snow_filt_sv) +
  # geom_line(aes(x=Date, y=swe, color=factor(WY), size=factor(WY))) +
  geom_rect(data=data.frame(
    from=as.Date(paste0("1979-",format(Sys.Date(),"%m-%d"))), 
    to=as.Date("1980-09-30")),
            aes(xmin = from, xmax=to, ymin=-Inf, ymax=Inf),
             fill = "gray97", alpha = 0.7)+
  geom_line(aes(x=date, y=swe, color=factor(WY), size=factor(WY))) +
  scale_color_manual(values=c("black","#9A3324","#25819C")) +
  scale_size_manual(values = c(0.8,0.8,1.2)) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b",
               limits = c(as.Date("1979-10-01"), as.Date("1980-09-30")),
               expand = c(0,0)) +
  scale_y_continuous(limits = c(0,ymax), 
                     breaks =seq(0,ymax,2), 
                     expand = c(0,0)) +
  labs(title = "Salt-Verde River Basin", 
       subtitle = paste0("(as of ", format(curWY$date, "%B %d, "),year(Sys.Date()),")"),
       color = "",
       x=NULL, size="",y = "Snow water Equivalent (in)") +
  theme_bw() +
  theme( 
    plot.title = element_text(hjust = 0.5,  
                              color = "black", 
                              size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5,  color = "black", size = 12),
    panel.grid.major = element_line(colour = "grey85"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(vjust = 0.5, 
                               hjust=0.5, color = "black",
                               size = 10),
    axis.text.y = element_text(color = "black", size = 10),
    axis.title = element_text(size = 12),
    axis.title.y = element_text(margin = margin(r = 0.15, l=0.1, unit = "in")),
    legend.position="bottom",
    legend.text=element_text(size=12)
  )

if(curWY$date > min(snow_filt_sv$date) + 20){
  SV_Snowplot <- SV_Snowplot +
    annotate(
      geom = "segment",
      x = curWY$date-1, yend = 7.4,
      xend = curWY$date-18, y = 7.4,
      # curvature = 0.1, 
      linewidth = 0.8,
      arrow = arrow(length = unit(2, "mm"))
    ) +
    annotate(geom = "text", x = curWY$date-3, y = 7.7 , 
             label = "PAST", size=2.4,
             hjust="right")
}

if(curWY$date < max(snow_filt_sv$date, na.rm = TRUE) - 20){
    SV_Snowplot <- SV_Snowplot + annotate(
        geom = "segment",
        x = curWY$date+1, yend = 7.4,
        xend = curWY$date+18, y = 7.4,
        # curvature = 0.1, 
        linewidth = 0.8,
        arrow = arrow(length = unit(2, "mm"))
      ) +
      annotate(geom = "text", x = curWY$date+3, y = 7.7 , 
               label = "FUTURE", size=2.4,
               hjust="left") 
} else {
  
    SV_Snowplot <- SV_Snowplot + annotate(
      geom = "segment",
      x = curWY$date+1, yend = 7.4,
      xend = curWY$date - 2 +
        as.double(difftime(max(snow_filt_sv$date, na.rm = TRUE),
                           curWY$date, 
                           units = c("days"))), y = 7.4,
      # curvature = 0.1, 
      linewidth = 0.6,
      arrow = arrow(length = unit(2, "mm")))
    }

# Add this after Peak SWE
# SV_Snowplot <- SV_Snowplot +
# annotate(
#     geom = "curve", xend = curWY_maxSWE$Date, 
#     yend = curWY_maxSWE$swe + 0.2,x = as.Date("2023-12-31"), y=12, 
#     curvature = -.3, arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate("label", 
#            x = as.Date("2023-11-30"), 
#            y=12, 
#            label = caption, 
#            size = 3,
#            label.padding=unit(0.5, "lines"),
#            fill = "grey95")

ggsave("Salt-VerdeSWE.png", 
       width = 6.5, height = 4.75)

############# UC Plot #######################

snow <- data.table::fread(
  "https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC2/14_Upper_Colorado_Region.csv")

# Get location of column names that end with a number (years)
cnames <- grep("\\D+$",colnames(snow), invert = TRUE)

# Add the prefix "WY" to the column names selected above
colnames(snow)[cnames] <- paste0("WY", colnames(snow)[cnames])

previousMonth <- floor_date(Sys.Date(), "month") - months(1)



snow_filt <- snow[,.(date,WY2024,WY2025,`Median ('91-'20)`)] %>% 
  mutate(mon = as.numeric(str_extract(date,"^[0-9]{2}")),
         date = as.Date(paste0(date,"-",ifelse(mon>9,1979,1980)), "%m-%d-%Y")) %>% 
  pivot_longer(!c(date,mon), names_to = "WY", values_to = "swe") %>% 
  mutate(WY = case_when(
    WY=="WY2025" ~ "Water Year: 2025",
    WY=="WY2024" ~ "Water Year: 2024",
    .default = "Median 1991-2020"
  ))



curWY <- snow_filt %>%
  filter(WY == "Water Year: 2025" & !is.na(swe)) %>%
  filter(date == max(date))

curWY_maxSWE <- snow_filt %>%
  filter(WY == "Water Year: 2025") %>%
  filter(swe==max(swe, na.rm = T))



med_SWE <- snow_filt %>% filter(str_detect(WY,"Median")) %>%
  filter(date==curWY_maxSWE$date)

# Caption to add after Peak SWE has happened
# caption = paste(strwrap(paste0(
#                         "SWE peaked at ",
#                         round(curWY_maxSWE$swe/med_SWE$swe*100,0),
#                         "% of the peak seasonal median on ",
#                         format(curWY_maxSWE$Date, "%B %d, %Y"),"."),
#                 25), 
#                 collapse = "\n")

# cmswe <- curWY_maxSWE
# cmswe$Label = caption

maxswe <- max(snow_filt$swe, na.rm = TRUE)
ymax <- ifelse((maxswe %% 5)>2.5, 
                maxswe-(maxswe %% 5) + 10, 
                maxswe -(maxswe %% 5) +5) 

UC_Snowplot <- ggplot(data=snow_filt) +
  geom_rect(data=data.frame(
    from=as.Date(paste0("1979-",format(Sys.Date(),"%m-%d"))), 
    to=as.Date("1980-09-30")),
            aes(xmin = from, xmax=to, ymin=-Inf, ymax=Inf),
             fill = "gray97", alpha = 0.7)+
  geom_line(aes(x=date, y=swe, color=factor(WY), size=factor(WY))) +
  scale_color_manual(values=c("black","#9A3324","#25819C")) +
  scale_size_manual(values = c(0.8,0.8,1.2)) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b",
               limits = c(as.Date("1979-10-01"), as.Date("1980-09-30")),
               expand = c(0,0)) +
  scale_y_continuous(limits = c(0,ymax), 
                     breaks =seq(0,ymax,5), 
                     expand = c(0,0)) +
  labs(title = "Colorado River Basin Above Lake Powell", 
       subtitle = paste0("(as of ", format(curWY$date, "%B %d, "),year(Sys.Date()),")"),
       color = "",
       x=NULL, size="",y = "Snow water Equivalent (in)") +
  theme_bw() +
  theme( 
    plot.title = element_text(hjust = 0.5,  
                              color = "black", 
                              size = 16, 
                              face = "bold"),
    plot.subtitle = element_text(hjust = 0.5,  color = "black", size = 12),
    panel.grid.major = element_line(colour = "grey85"),
    panel.grid.minor.x = element_blank(),
    
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(vjust = 0.5, 
                               hjust=0.5, color = "black",
                               size = 10),
    axis.text.y = element_text(color = "black", size = 10),
    axis.title = element_text(size = 12),
    axis.title.y = element_text(margin = margin(r = 0.15, l=0.1, unit = "in")),
    legend.position="bottom",
    legend.text=element_text(size=12)
  )


if(curWY$date > min(snow_filt$date) + 20){
  UC_Snowplot <- UC_Snowplot +
    annotate(
      geom = "segment",
      x = curWY$date-1, yend = 18.5,
      xend = curWY$date-18, y = 18.5,
      # curvature = 0.1, 
      linewidth = 0.6,
      arrow = arrow(length = unit(2, "mm"))
    ) +
    annotate(geom = "text", x = curWY$date-2, y = 19.2 , 
             label = "PAST", size=2.0,
             hjust="right")
}
  
if(curWY$date < max(snow_filt$date) - 20){
    UC_Snowplot <- UC_Snowplot + annotate(
        geom = "segment",
        x = curWY$date+1, yend = 18.5,
        xend = curWY$date+18, y = 18.5,
        # curvature = 0.1, 
        linewidth = 0.6,
        arrow = arrow(length = unit(2, "mm"))
      ) +
      annotate(geom = "text", x = curWY$date+2, y = 19.2 , 
               label = "FUTURE", size=2.0,
               hjust="left") 
} else {
  UC_Snowplot <- UC_Snowplot + annotate(
    geom = "segment",
    x = curWY$date+1, yend = 18.5,
    xend = curWY$date - 2 +
      as.double(difftime(max(snow_filt$date),
                         curWY$date, 
                         units = c("days"))), y = 18.5,
    linewidth = 0.6,
    arrow = arrow(length = unit(2, "mm")))
  }
    

# UC_Snowplot <- UC_Snowplot +
#   annotate(
#     geom = "curve", xend = curWY_maxSWE$date, 
#     yend = curWY_maxSWE$swe + 0.2,x = as.Date("1980-01-31"), y=23, 
#     curvature = -.3, arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate("label", 
#            x = as.Date("1979-12-31"), 
#            y=23, 
#            label = caption,
#            size = 3,
#            label.padding=unit(0.5, "lines"),
#            fill = "grey95")

ggsave("AbvPowellSWE.png", 
       width = 6.5, height = 4.75)
