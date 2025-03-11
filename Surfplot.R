library(ggseg)
library(ggsegSchaefer)
library(ggseg3d)
library(ggplot2)
library(tidyverse)
library(readxl)
rm(list = ls())

#This R script creates comprehensive visualizations of brain networks, focusing on regions with increased and decreased connectivity in Patients. It combines cortical surface plots using the Schaefer atlas with subcortical visualizations and network matrices to create publication-ready figures.

#### Patients Decreased Connectivity ####
# Load Labels
setwd("G:/PHD_1/NeuroT/")
LB <- read_csv("./Code/CUDCode/brain_networks.csv")
LB_400 <- LB[1:400,]
LB2 <- schaefer17_400$data
LB2$ROI_LB <- gsub("^[lr]h_", "", LB2$label)
### Load degree
DG <-  read.csv("./RES/Mask/avg_masked_Positive_covars_fd_Tg_0.05.csv")
DG <- DG[,c(2:433)]
LB_400$Degree <- rowSums(DG)[1:400]
LB2 <- LB2[,c(3,4,7)]
colnames(LB2)[3] <-"ROI"
LB3 <- merge(LB2,LB_400,by="ROI",all.x=TRUE)
## Plot
someData = tibble(
  region =LB3$region, Degree = LB3$Degree 
)
someData %>% 
  brain_join(schaefer17_400) %>% 
  plot()

someData %>% 
  brain_join(schaefer17_400) %>% 
  ggplot() + 
  geom_sf(aes(fill = Degree),show.legend = TRUE,na.rm = FALSE)+
  scale_fill_gradientn(
    colours = c("white", "#BFBFDF", "#7F7FBF", "#3F3F9F", "#00008B"),na.value = "white"
  ) +
  theme_void()
ggsave("./RES/Plot/brain_plot_Neg_Patientsdec.png", bg = "transparent", width = 10, height = 8, dpi = 300)


#### Patients Increased Connectivity (HC-Patients < 0) ####
rm(list = ls())
# Load Labels
setwd("G:/SF/PHD_1/NeuroT/")
LB <- read_csv("./Code/CUDCode/brain_networks.csv")
LB_400 <- LB[1:400,]
LB2 <- schaefer17_400$data
LB2$ROI_LB <- gsub("^[lr]h_", "", LB2$label)
### Load degree
DG <-  read.csv("./RES/Mask/avg_masked_Negative_covars_fd_Tg_0.05.csv")
DG <- DG[,c(2:433)]
LB_400$Degree <- rowSums(DG)[1:400]
LB2 <- LB2[,c(3,4,7)]
colnames(LB2)[3] <-"ROI"
LB3 <- merge(LB2,LB_400,by="ROI",all.x=TRUE)
## Plot
someData = tibble(
  region =LB3$region, Degree = LB3$Degree 
)
someData %>% 
  brain_join(schaefer17_400) %>% 
  plot()

someData %>% 
  brain_join(schaefer17_400) %>% 
  ggplot() + 
  geom_sf(aes(fill = Degree),show.legend = TRUE,na.rm = FALSE)+
  scale_fill_gradientn(
    colours = c("lightyellow", "yellow", "orange", "red", "darkred"),na.value = "white"
  ) +
  theme_void()
ggsave("./RES/Plot/brain_plot_Neg_Patientsinc.png", bg = "transparent", width = 10, height = 8, dpi = 300)



#### Python SurfPlot2.ipynb (Note: Pos Neg) ####

# "G:\PHD_1\NeuroT\Code\PlotSub