#fisio

rm(list=ls())


library(readxl)


###########################Regresion models##############################

#Lactate and Succinate

LASU<- read_excel("data/regresion.xlsx", 
                    sheet = "LASU")

#deltas


model1<-lm(SuccinateD~LactateD,LASU)

summary(model1)


#Lactate Pyruvate and Succinate

LPSU <- read_excel("data/regresion.xlsx", 
                    sheet = "LPSU")

model2<-lm(Succinate~LactatePiruvate,LPSU)

summary(model2)


#Citrate and Lactate pyruvate


CILP <- read_excel("data/regresion.xlsx", 
                    sheet = "CILP")

model3<-lm(Citrate~LactatePiruvate,CILP)

summary(model3)


#Citrate and lactate


CILA <- read_excel("data/regresion.xlsx", 
                   sheet = "CILA")

model4<-lm(Citrate~Lactate, CILA)

summary(model4)


##############################Regresion model plots##################################

library(ggplot2)

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "black")  +  theme_bw()+ theme(text=element_text(size=15))
  
}

plot1 <- ggplotRegression(model1) + xlab(expression(Delta~Succinate (mmol/L))) + ylab(expression(Delta~Lactate (mmol/L)));plot1
                                                     
plot2 <- ggplotRegression(model1) + xlab(expression(Delta~Succinate (mmol/L))) + ylab(expression(Delta~Lactate/Pyruvate (mmol/L)));plot2

plot2 <- ggplotRegression(model1) + xlab(expression(Delta~Succinate (mmol/L))) + ylab(expression(Delta~Lactate/Pyruvate (mmol/L)));plot2
                    
plot3 <- ggplotRegression(model3) + xlab(expression(Delta~Citrate (mmol/L))) + ylab(expression(Delta~Lactate/Pyruvate (mmol/L)));plot3

plot4 <- ggplotRegression(model4) + xlab(expression(Delta~Citrate (mmol/L))) + ylab(expression(Delta~Lactate (mmol/L)));plot4




ggsave(plot = plot1,        # Plot object to save
       filename = "plot1.png", # Destination file
       width = 5,                 # Width of image (recommended)
       height = 3.5,             # Height of image (recommended)
       dpi = 1000)        

ggsave(plot = plot2,        # Plot object to save
       filename = "plot2.png", # Destination file
       width = 5,                 # Width of image (recommended)
       height = 3.5,             # Height of image (recommended)
       dpi = 1000)        

ggsave(plot = plot3,        # Plot object to save
       filename = "plot3.png", # Destination file
       width = 5,                 # Width of image (recommended)
       height = 3.5,             # Height of image (recommended)
       dpi = 1000)    

ggsave(plot = plot4,        # Plot object to save
       filename = "plot4.png", # Destination file
       width = 5,                 # Width of image (recommended)
       height = 3.5,             # Height of image (recommended)
       dpi = 1000)   

figure4 <- cowplot::plot_grid(plot1, plot2,  plot4, plot3, nrow = 2, labels = "AUTO" )  


ggsave(plot = figure4,        # Plot object to save
       filename = "figure4.png", # Destination file
       width = 7,                 # Width of image (recommended)
       height = 8,             # Height of image (recommended)
       dpi = 1000) 


