# libraries
library(ggplot2)
library(tidyverse)
library(data.table)
library(ggforce)

snow_sv <- data.table::fread(
 "https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC4/1506_Salt.csv"
    )

previousMonth <- floor_date(Sys.Date(), "month") - months(1)


snow_filt_sv <- snow_sv[,.(date,`2023`,`2024`,`Median ('91-'20)`)]
snow_final_sv <- snow_filt_sv %>% 
  mutate(WY = as.numeric(str_extract(date,"^[0-9]{2}")),
         WY = ifelse(WY>9,2023,2024),
         date = as.Date(paste0(WY,"-",date))) %>% 
  select(Date=date, WY2023=`2023`, 
  WY2024 = `2024`, Median = `Median ('91-'20)`) %>% 
  pivot_longer(!Date, names_to = "WY", values_to = "swe") %>% 
  mutate(WY = case_when(
    WY=="Median" ~ "Median 1991-2020",
    WY=="WY2023" ~ "Water Year: 2023",
    .default = "Water Year: 2024"
  ))

curWY <- snow_final_sv %>% 
  filter(WY == "Water Year: 2024" & !is.na(swe)) %>% 
  filter(Date == max(Date))

curWY_maxSWE <- snow_final_sv %>% 
  filter(str_detect(WY,as.character(year(previousMonth)))) %>% 
  filter(swe==max(swe, na.rm = T))



med_SWE <- snow_final_sv %>% filter(str_detect(WY,"Median")) %>% 
  filter(Date==curWY_maxSWE$Date)

caption = paste(strwrap(paste0(
                        "SWE peaked at ",
                        round(curWY_maxSWE$swe/med_SWE$swe*100,0),
                        "% of the peak seasonal median on ",
                        format(curWY_maxSWE$Date, "%B %d, %Y"),"."),
                25), 
                collapse = "\n")

cmswe <- curWY_maxSWE
cmswe$Label = caption

maxswe <- max(snow_final_sv$swe, na.rm = TRUE)
ymax <- ifelse((maxswe %% 2)>1, 
                maxswe-(maxswe %% 2) + 4, 
                maxswe -(maxswe %% 2) +2) 

SV_Snowplot <- ggplot(data=snow_final_sv) +
  geom_line(aes(x=Date, y=swe, color=factor(WY), size=factor(WY))) +
  geom_rect(data=data.frame(from=as.Date(Sys.Date()), to=as.Date("2024-09-30")),
            aes(xmin = from, xmax=to, ymin=-Inf, ymax=Inf),
             fill = "gray95", alpha = 0.7)+
  scale_color_manual(values=c("black","#9A3324","#25819C")) +
  scale_size_manual(values = c(0.8,0.8,1.2)) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b",
               limits = c(as.Date("2023-10-01"), as.Date("2024-09-30")),
               expand = c(0,0)) +
  scale_y_continuous(limits = c(0,ymax), 
                     breaks =seq(0,ymax,2), 
                     expand = c(0,0)) +
  labs(title = "Salt-Verde River Basin", 
       subtitle = paste0("(as of ", format(curWY$Date, "%B %d, %Y"),")"),
       color = "",
       x=NULL, size="",y = "Snow water Equivalent (in)") +
  theme_bw() +
  theme( 
    plot.title = element_text(hjust = 0.5,  
                              color = "black", 
                              size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5,  color = "black", size = 12),
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

if(curWY$Date > min(snow_final_sv$Date) + 20){
  SV_Snowplot <- SV_Snowplot +
    annotate(
      geom = "segment",
      x = curWY$Date, yend = 15,
      xend = curWY$Date-18, y = 15,
      # curvature = 0.1, 
      linewidth = 0.8,
      arrow = arrow(length = unit(2, "mm"))
    ) +
    annotate(geom = "text", x = curWY$Date-4, y = 15.5 , 
             label = "PAST", size=2.4,
             hjust="right")
}

if(curWY$Date < max(snow_final_sv$Date) - 20){
    SV_Snowplot <- SV_Snowplot + annotate(
        geom = "segment",
        x = curWY$Date+1, yend = 15,
        xend = curWY$Date+18, y = 15,
        # curvature = 0.1, 
        linewidth = 0.8,
        arrow = arrow(length = unit(2, "mm"))
      ) +
      annotate(geom = "text", x = curWY$Date+2, y = 15.5 , 
               label = "FUTURE", size=2.4,
               hjust="left") 
} else {
  
    SV_Snowplot <- SV_Snowplot + annotate(
      geom = "segment",
      x = curWY$Date+1, yend = 15,
      xend = curWY$Date - 2 +
        as.double(difftime(max(snow_final_sv$Date),
                           curWY$Date, 
                           units = c("days"))), y = 15,
      # curvature = 0.1, 
      linewidth = 0.6,
      arrow = arrow(length = unit(2, "mm")))
    }

SV_Snowplot +
annotate(
    geom = "curve", xend = curWY_maxSWE$Date, 
    yend = curWY_maxSWE$swe + 0.2,x = as.Date("2023-12-31"), y=12, 
    curvature = -.3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate("label", 
           x = as.Date("2023-11-30"), 
           y=12, 
           label = caption, 
           size = 3,
           label.padding=unit(0.5, "lines"),
           fill = "grey95")

ggsave("Salt-VerdeSWE.png", 
       width = 6.5, height = 4.75)

############# UC Plot #######################

snow <- data.table::fread(
  "https://nwcc-apps.sc.egov.usda.gov/awdb/basin-plots/POR/WTEQ/assocHUC2/14_Upper_Colorado_Region.csv")

previousMonth <- floor_date(Sys.Date(), "month") - months(1)


snow_filt <- snow[,.(date,`2023`,`2024`,`Median ('91-'20)`)]
snow_final <- snow_filt %>% 
  mutate(WY = as.numeric(str_extract(date,"^[0-9]{2}")),
         WY = ifelse(WY>9,2023,2024),
         date = as.Date(paste0(WY,"-",date))) %>% 
  select(Date=date, WY2023=`2023`, 
          WY2024 = `2024`, Median = `Median ('91-'20)`) %>% 
  pivot_longer(!Date, names_to = "WY", values_to = "swe") %>% 
  mutate(WY = case_when(
    WY=="Median" ~ "Median 1991-2020",
    WY=="WY2023" ~ "Water Year: 2023",
    .default = "Water Year: 2024"
  ))

curWY <- snow_final %>% 
  filter(WY == "Water Year: 2024" & !is.na(swe)) %>% 
  filter(Date == max(Date))

# snow_final %>% 
#   filter(WY == "Water Year: 2024" & !is.na(swe)) %>% 
#   
# 
# curWY$lastDate

curWY_maxSWE <- snow_final %>% 
  filter(str_detect(WY,as.character(year(previousMonth)))) %>% 
  filter(swe==max(swe, na.rm = T))



med_SWE <- snow_final %>% filter(str_detect(WY,"Median")) %>% 
  filter(Date==curWY_maxSWE$Date)

caption = paste(strwrap(
                paste0("SWE peaked at ",
                        round(curWY_maxSWE$swe/med_SWE$swe*100,0),
                        "% of the peak seasonal median on ",
                        format(curWY_maxSWE$Date, "%B %d, %Y"),"."),
                        25), collapse = "\n")

cmswe <- curWY_maxSWE
cmswe$Label = caption

maxswe <- max(snow_final$swe, na.rm = TRUE)
ymax <- ifelse((maxswe %% 5)>2.5, 
                maxswe-(maxswe %% 5) + 10, 
                maxswe -(maxswe %% 5) +5) 

UC_Snowplot <- ggplot(data=snow_final) +
  geom_line(aes(x=Date, y=swe, color=factor(WY), size=factor(WY))) +
  geom_rect(data=data.frame(from=as.Date(Sys.Date()), to=as.Date("2024-09-30")),
            aes(xmin = from, xmax=to, ymin=-Inf, ymax=Inf),
             fill = "gray95", alpha = 0.7)+
  scale_color_manual(values=c("black","#9A3324","#25819C")) +
  scale_size_manual(values = c(0.8,0.8,1.2)) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b",
               limits = c(as.Date("2023-10-01"), as.Date("2024-09-30")),
               expand = c(0,0)) +
  scale_y_continuous(limits = c(0,ymax), 
                     breaks =seq(0,ymax,5), 
                     expand = c(0,0)) +
  labs(title = "Colorado River Basin Above Lake Powell", 
       subtitle = paste0("(as of ", format(curWY$Date, "%B %d, %Y"),")"),
       color = "",
       x=NULL, size="",y = "Snow water Equivalent (in)") +
  theme_bw() +
  theme( 
    plot.title = element_text(hjust = 0.5,  
                              color = "black", 
                              size = 16, 
                              face = "bold"),
    plot.subtitle = element_text(hjust = 0.5,  color = "black", size = 12),
    
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


if(curWY$Date > min(snow_final$Date) + 20){
  UC_Snowplot <- UC_Snowplot +
    annotate(
      geom = "segment",
      x = curWY$Date, yend = 27.5,
      xend = curWY$Date-18, y = 27.5,
      # curvature = 0.1, 
      linewidth = 0.6,
      arrow = arrow(length = unit(2, "mm"))
    ) +
    annotate(geom = "text", x = curWY$Date-2, y = 28.2 , 
             label = "PAST", size=2.0,
             hjust="right")
}
  
if(curWY$Date < max(snow_final$Date) - 20){
    UC_Snowplot <- UC_Snowplot + annotate(
        geom = "segment",
        x = curWY$Date+1, yend = 27.5,
        xend = curWY$Date+18, y = 27.5,
        # curvature = 0.1, 
        linewidth = 0.6,
        arrow = arrow(length = unit(2, "mm"))
      ) +
      annotate(geom = "text", x = curWY$Date+2, y = 28.2 , 
               label = "FUTURE", size=2.0,
               hjust="left") 
} else {
  UC_Snowplot <- UC_Snowplot + annotate(
    geom = "segment",
    x = curWY$Date+1, yend = 27.5,
    xend = curWY$Date - 2 +
      as.double(difftime(max(snow_final$Date),
                         curWY$Date, 
                         units = c("days"))), y = 27.5,
    linewidth = 0.6,
    arrow = arrow(length = unit(2, "mm")))
  }
    

UC_Snowplot <- UC_Snowplot +
  annotate(
    geom = "curve", xend = curWY_maxSWE$Date, 
    yend = curWY_maxSWE$swe + 0.2,x = as.Date("2024-01-31"), y=23, 
    curvature = -.3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate("label", 
           x = as.Date("2023-12-31"), 
           y=23, 
           label = caption,
           size = 3,
           label.padding=unit(0.5, "lines"),
           fill = "grey95")

ggsave(here::here("AbvPowellSWE.png"), 
       width = 6.5, height = 4.75)