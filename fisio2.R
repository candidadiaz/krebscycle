rm(list=ls())


library(readxl)
library(ggplot2)
library("ggthemes") 
library(dplyr)


########################Tendency measures intermediates of Krebs Cycle#####################

#Succinate

succinate<- read_excel("data/intermediates.xlsx", 
                  sheet = "Succinate")


succinate <- succinate %>%
  group_by(Hours, Group) %>%
  summarise(
    Q25 = quantile(Succinate, probs = 0.25),
    Q75 = quantile(Succinate, probs = 0.75),
    Succinate= median(Succinate)
  )


succinatePlot <- ggplot(succinate, aes(Hours, Succinate)) +
  geom_line(aes(group = Group, color = Group), linewidth=0.8)+
  geom_point(aes(shape = Group, colour = Group), size = 5)+
  geom_errorbar(
    aes(ymin = Q25, ymax = Q75, group = Group, color = Group),
    width = 0.3, linewidth=0.8 )

succinatePlot <-  succinatePlot + scale_color_manual(values = c("Black", "Grey51")) + theme_bw(base_size = 16)+ 
  scale_x_continuous(breaks = c(0,3,6)) + ylab("Succinate (mmol/L)")  + xlab("Time (hours)")

ggsave(plot = succinatePlot,        # Plot object to save
       filename = "succinate.png", # Destination file
       width = 7,                 # Width of image (recommended)
       height = 4.5,             # Height of image (recommended)
       dpi = 1000)        



#lactate

lactate<- read_excel("data/intermediates.xlsx", 
                       sheet = "Lactate")


lactate <- lactate %>%
  group_by(Hours, Group) %>%
  summarise(
    Q25 = quantile(Lactate, probs = 0.25),
    Q75 = quantile(Lactate, probs = 0.75),
    Lactate= median(Lactate)
  )

lactatePlot <- ggplot(lactate, aes(Hours, Lactate)) +
  geom_line(aes(group = Group, color = Group), linewidth=0.8)+
  geom_point(aes(shape = Group, colour = Group), size = 5)+
  geom_errorbar(
    aes(ymin = Q25, ymax = Q75, group = Group, color = Group),
    width = 0.3, linewidth=0.8 )

lactatePlot <-  lactatePlot + scale_color_manual(values = c("Black", "Grey51")) + theme_bw(base_size = 16)+ 
  scale_x_continuous(breaks = c(0,3,6)) + xlab("Time (hours)") + ylab("Lactate (mmol/L)")


ggsave(plot = lactatePlot,        # Plot object to save
       filename = "lactate.png", # Destination file
       width = 7,                 # Width of image (recommended)
       height = 4.5,             # Height of image (recommended)
       dpi = 1000)        



#lactate pyruvate

lactateP<- read_excel("data/intermediates.xlsx", 
                     sheet = "LactateP")



lactateP <- lactateP %>%
  group_by(Hours, Group) %>%
  summarise(
    Q25 = quantile(LP, probs = 0.25),
    Q75 = quantile(LP, probs = 0.75),
    LactateLP= median(LP)
  )



lactatepPlot <- ggplot(lactateP, aes(Hours, LactateLP)) +
  geom_line(aes(group = Group, color = Group), linewidth=0.8)+
  geom_point(aes(shape = Group, colour = Group), size = 5)+
  geom_errorbar(
    aes(ymin = Q25, ymax = Q75, group = Group, color = Group),
    width = 0.3, linewidth=0.8
  )

lactatepPlot <- lactatepPlot + scale_color_manual(values = c("Black", "Grey51")) + theme_bw(base_size = 16)+ 
  scale_x_continuous(breaks = c(0,3,6)) + xlab("Time (hours)") + ylab("Lactate/Pyruvate (mmol/L)")


ggsave(plot = lactatepPlot,        # Plot object to save
       filename = "lactatep.png", # Destination file
       width = 7,                 # Width of image (recommended)
       height = 4.5,             # Height of image (recommended)
       dpi = 1000)        



#citrate


citrate<- read_excel("data/intermediates.xlsx", 
                      sheet = "Citrate")


citrate <- citrate %>%
  group_by(Hours, Group) %>%
  summarise(
    Q25 = quantile(Citrate, probs = 0.25),
    Q75 = quantile(Citrate, probs = 0.75),
    Citrate= median(Citrate)
  )



citratePlot <- ggplot(citrate, aes(Hours, Citrate)) +
  geom_line(aes(group = Group, color = Group), linewidth=0.8)+
  geom_point(aes(shape = Group, colour = Group), size = 5)+
  geom_errorbar(
    aes(ymin = Q25, ymax = Q75, group = Group, color = Group),
    width = 0.3, linewidth=0.8
  )

citratePlot <- citratePlot + scale_color_manual(values = c("Black", "Grey51")) + theme_bw(base_size = 16)+ 
  scale_x_continuous(breaks = c(0,3,6)) + xlab("Time (hours)") + ylab("Citrate (mmol/L)") 


ggsave(plot = citratePlot,        # Plot object to save
       filename = "citrate.png", # Destination file
       width = 7,                 # Width of image (recommended)
       height = 4.5,             # Height of image (recommended)
       dpi = 1000)     


#piruvate

pyruvate<- read_excel("data/intermediates.xlsx", 
                     sheet = "Pyruvate")



pyruvate <- pyruvate %>%
  group_by(Hours, Group) %>%
  summarise(
    Q25 = quantile(Pyruvate, probs = 0.25),
    Q75 = quantile(Pyruvate, probs = 0.75),
    Pyruvate= median(Pyruvate)
  )



pyruvatePlot <- ggplot(pyruvate, aes(Hours, Pyruvate)) +
  geom_line(aes(group = Group, color = Group), linewidth=0.8)+
  geom_point(aes(shape = Group, colour = Group), size = 5)+
  geom_errorbar(
    aes(ymin = Q25, ymax = Q75, group = Group, color = Group),
    width = 0.3, linewidth=0.8
  )

pyruvatePlot <- pyruvatePlot + scale_color_manual(values = c("Black", "Grey51")) + theme_bw(base_size = 16)+ 
  scale_x_continuous(breaks = c(0,3,6)) + xlab("Time (hours)") + ylab("Pyruvate (mmol/L)")

ggsave(plot = pyruvatePlot,        # Plot object to save
       filename = "pyruvate.png", # Destination file
       width = 7,                 # Width of image (recommended)
       height = 4.5,             # Height of image (recommended)
       dpi = 1000)     



#malate

malate<- read_excel("data/intermediates.xlsx", 
                      sheet = "Malate")



malate <- malate %>%
  group_by(Hours, Group) %>%
  summarise(
    Q25 = quantile(Malate, probs = 0.25),
    Q75 = quantile(Malate, probs = 0.75),
    Malate= median(Malate)
  )



malatePlot <- ggplot(malate, aes(Hours, Malate)) +
  geom_line(aes(group = Group, color = Group), linewidth=0.8)+
  geom_point(aes(shape = Group, colour = Group), size = 5)+
  geom_errorbar(
    aes(ymin = Q25, ymax = Q75, group = Group, color = Group),
    width = 0.3, linewidth=0.8
  )

malatePlot <-malatePlot + scale_color_manual(values = c("Black", "Grey51")) + theme_bw(base_size = 16)+ 
  scale_x_continuous(breaks = c(0,3,6)) + xlab("Time (hours)") + ylab("Malate (mmol/L)")

ggsave(plot = malatePlot,        # Plot object to save
       filename = "malate.png", # Destination file
       width = 7,                 # Width of image (recommended)
       height = 4.5,             # Height of image (recommended)
       dpi = 1000)    



#fumarate

fumarate<- read_excel("data/intermediates.xlsx", 
                      sheet = "Fumarate")



fumarate <- fumarate %>%
  group_by(Hours, Group) %>%
  summarise(
    Q25 = quantile(Fumarate, probs = 0.25),
    Q75 = quantile(Fumarate, probs = 0.75),
    Fumarate= median(Fumarate)
  )



fumaratePlot <- ggplot(fumarate, aes(Hours, Fumarate)) +
  geom_line(aes(group = Group, color = Group), linewidth=0.8)+
  geom_point(aes(shape = Group, colour = Group), size = 5)+
  geom_errorbar(
    aes(ymin = Q25, ymax = Q75, group = Group, color = Group),
    width = 0.3, linewidth=0.8
  )

fumaratePlot <-fumaratePlot + scale_color_manual(values = c("Black", "Grey51")) + theme_bw(base_size = 16)+ 
  scale_x_continuous(breaks = c(0,3,6)) + xlab("Time (hours)") + ylab("Fumarate (mmol/L)")

ggsave(plot = fumaratePlot,        # Plot object to save
       filename = "fumarate.png", # Destination file
       width = 7,                 # Width of image (recommended)
       height = 4.5,             # Height of image (recommended)
       dpi = 1000)    


#ketoglutarate

ketoglutarate<- read_excel("data/intermediates.xlsx", 
                      sheet = "Ketoglutarate")



ketoglutarate <- ketoglutarate %>%
  group_by(Hours, Group) %>%
  summarise(
    Q25 = quantile(Ketoglutarate, probs = 0.25),
    Q75 = quantile(Ketoglutarate, probs = 0.75),
    Ketoglutarate= median(Ketoglutarate)
  )



ketoglutaratePlot <- ggplot(ketoglutarate, aes(Hours, Ketoglutarate)) +
  geom_line(aes(group = Group, color = Group), linewidth=0.8)+
  geom_point(aes(shape = Group, colour = Group), size = 5)+
  geom_errorbar(
    aes(ymin = Q25, ymax = Q75, group = Group, color = Group),
    width = 0.3, linewidth=0.8
  )

ketoglutaratePlot<- ketoglutaratePlot + scale_color_manual(values = c("Black", "Grey51")) + theme_bw(base_size = 16)+ 
  scale_x_continuous(breaks = c(0,3,6)) + xlab("Time (hours)") + ylab(expression("Ketoglurate"~(mu*mol/L)))

ketoglutaratePlot + scale_color_manual(values = c("Black", "Grey51")) + theme_bw(base_size = 16)+ 
  scale_x_continuous(breaks = c(0,3,6)) + xlab("Time (hours)") + 
ylab(expression(alpha~Ketoglurate~(mu*mol/L)))


ggsave(plot = ketoglutaratePlot,        # Plot object to save
       filename = "ketoglutarate.png", # Destination file
       width = 7,                 # Width of image (recommended)
       height = 4.5,             # Height of image (recommended)
       dpi = 1000)    



figure2<- cowplot::plot_grid(citratePlot, ketoglutaratePlot,  succinatePlot, fumaratePlot, malatePlot, nrow = 3, labels = "AUTO" ) 

ggsave(plot = figure2,        # Plot object to save
       filename = "figure2.png", # Destination file
       width = 10,                 # Width of image (recommended)
       height = 9,             # Height of image (recommended)
       dpi = 1000)    


##############Difference of medians################

pyruvate<- read_excel("data/median.xlsx", 
                      sheet = "Pyruvate")


wilcox.test(pyruvate$endotoxin0,pyruvate$control0, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)


wilcox.test(pyruvate$endotoxin3,pyruvate$control3, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)

wilcox.test(pyruvate$endotoxin6,pyruvate$control6, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)



lactate<- read_excel("data/median.xlsx", 
                      sheet = "Lactate")


wilcox.test(lactate$endotoxin0,lactate$control0, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)


wilcox.test(lactate$endotoxin3,lactate$control3, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)


wilcox.test(lactate$endotoxin6,lactate$control6, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)


succinate<- read_excel("data/median.xlsx", 
                     sheet = "Succinate")

wilcox.test(succinate$endotoxin0,succinate$control0, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)


wilcox.test(succinate$endotoxin3,succinate$control3, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)

wilcox.test(succinate$endotoxin6,succinate$control6, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)


LP<- read_excel("data/median.xlsx", 
                       sheet = "LP")


wilcox.test(LP$endotoxin0,LP$control0, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)


wilcox.test(LP$endotoxin3,LP$control3, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)


wilcox.test(LP$endotoxin6,LP$control6, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)


citrate<- read_excel("data/median.xlsx", 
                       sheet = "Citrate")


wilcox.test(citrate$endotoxin0,citrato$control0, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)


wilcox.test(citrate$endotoxin3,citrate$control3, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)


wilcox.test(citrate$endotoxin6,citrate$control6, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=F)


succinato<- read_excel("data/median.xlsx", 
                sheet = "Succinate")

wilcox.test(succinate$control3, succinate$control6, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=T)


wilcox.test(succinate$control0, succinate$control6, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=T)

wilcox.test(succinate$endotoxin0, succinate$endotoxin6, exact = F,correct = T,conf.int = F, alternative = "two.sided", paired=T)



