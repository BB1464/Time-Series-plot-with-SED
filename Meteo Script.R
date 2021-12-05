####### Meteo Data Anlysis for Adenike ########
#######                                ########
###############################################
###############################################

setwd(dir = '~/../Desktop')

library(tidyverse)
library(readxl)
library(patchwork)

### Import the data set into R #####

dat <- read_excel('weather Data Femi.xlsx')

dat$Month<-factor(dat$Month,levels = c('2020 Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','2021 Jan','Feb.','Mar.','Apr.'))


########### Visualization ######################
theme_set(theme_classic())
###### Rainfall ##########
p1<-ggplot(dat,aes(Month,`TOTAL RAINFALL`))+
  stat_summary(fun = 'sum',geom = 'bar',fill='blue')+
  theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),axis.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 14))+
  ylab('Rainfall (mm)')+
  coord_cartesian(expand = c(0,0))




####### Maximum and Minimum Temperature ##############
p2<-ggplot(dat,aes(Month,`Min T (°C)`,group=1))+geom_point(colour='red',size=1.7)+geom_line(colour='red',size=1.2)+
  geom_line(aes(Month,MaxT),col='black',group=2,size=1.2)+geom_point(aes(Month,MaxT),colour='black',size=1.7)+ylab('Minimum Temperature (°C)')+
  scale_y_continuous(sec.axis = sec_axis(trans = ~./0.9,name = 'Maximum Temperature (°C)'))+
  theme(axis.title.y.left = element_text(size = 14,face = 'bold',colour = 'red',family = 'serif'),axis.text.y.right = element_text(family = 'serif',face = 'bold',colour = 'black',size = 14,margin = margin(t = 0,r = 5,b = 0,l = 0)),axis.title.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),axis.text.y.left = element_text(family = 'serif',face = 'bold',colour = 'black',size = 14),axis.title.y.right = element_text(family = 'serif',face = 'bold',colour = 'black',size = 14),line = element_line(size = 1,colour = 'black'),axis.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 14))

p1/p2

ggsave('weather.tiff',width = 12,height = 9,dpi = 370)
