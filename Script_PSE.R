# Preparation data

install.packages("plyr")
install.packages("dplyr")
install.packages(c("tidyverse", "nls.multstart"))
library(plyr)
library(dplyr)
library(tidyverse)
library(nls.multstart)

rm(list=ls())

# Global

data <- read.csv(file = "Data.csv", header =T, sep=";")


# Condition

data <- data %>%
  filter(Effector == "Outil")
data <- data %>%
  filter(Effector == "Main")
data <- data %>%
  filter(Movement == "Move")
data <- data %>%
  filter(Movement ==  "Nomove")
data <- data %>%
  filter(Reliability ==  "Test")
data <- data %>%
  filter(Reliability ==  "Retest")


# Conversion
data$Estimation <- sub('1','0',data$Estimation)
data$Estimation <- sub('2','1',data$Estimation)
data$Estimation <- as.numeric(data$Estimation)
data$Distance   <- as.integer(data$Distance)
data <- subset(data, select = -RT)


# Analyses pour un participant ou une condition  
data <- data %>% filter(Participant == 1)

y <- sapply(split(data$Estimation, data$Distance), function(x) mean(x, na.rm = TRUE))
x <- c(1,2,3,4,5,6,7,8)

fit_sigmoG <- nls(y ~ 1/(1+exp(-(x-PSE)/JND)), start = c(PSE = 4, JND = 0.5), algorithm = "port" , control = list(maxiter = 10000000))
result <- summary(fit_sigmoG)

output <- print(result[10],4)
write.csv(output, file = "foo.csv", row.names = FALSE)
toto <- read.csv("foo.csv")
PSE <- toto[1, 1]
JND <- toto[2, 1]


# Analyses pour les participants
fit_model <- function(participant_data) {
  y <- sapply(split(participant_data$Estimation, participant_data$Distance), function(x) mean(x, na.rm = TRUE))
  x <- c(1, 2, 3, 4, 5, 6, 7, 8)
  
  fit_sigmoG <- nls(y ~ 1 / (1 + exp(-(x - PSE) / JND)),
                    start = c(PSE = 5, JND = 0.5),
                    algorithm = "port",
                    control = list(maxiter = 100000))
  
  return(coef(fit_sigmoG))
}

result <- data %>%
  group_by(Participant) %>%
  summarise(model_params = list(fit_model(.data))) %>%
  unnest(model_params)

print(result)

write.csv(result, file = "PSE.csv", row.names = F)

# Graphique courbe 

plot(x, y, col="red", pch="", lty=1,xlim=c(1,8), ylim=c(0,1), xlab ="Distance",
     ylab = "\n Proportion of long response", main = "Psychometric curve", 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex=2)

axis(1, at=c(1, 3, 5, 7), labels=c("1", "3", "5", "7"),cex.axis=1.5, font.axis=1)

curve(1/(1+exp(-(x-PSE)/JND)), xlim=c(1,8), ylim=c(0,1), add = TRUE, col="red") 
curve(1/(1+exp(-(x-PSE)/JND)), xlim=c(1,8), ylim=c(0,1), add = TRUE, col="blue")
curve(1/(1+exp(-(x-PSE)/JND)), xlim=c(1,8), ylim=c(0,1), add = TRUE, col="green")
curve(1/(1+exp(-(x-PSE)/JND)), xlim=c(1,8), ylim=c(0,1), add = TRUE, col="grey")
abline(a=NULL, b=NULL, h=0.5, v=PSE, lwd=1)

points(x, y, col="blue", pch=16)
points(x, y, col="red", pch=16)
points(x, y, col="green", pch=16)
points(x, y, col="grey", pch=16)

legend("right", 
       legend=c("Move Tool", "Move Hand", "Nomove Tool", "Nomove Hand"),
       box.lwd=0, box.col="white",
       col=c("red", "blue", "green", "grey"), 
       lty=c(1, 1, 1, 1), pch=c(NA, NA, NA, NA), cex=1.0, 
       inset=c(0.025, 0.025))

legend("right", 
       legend=c("Nomove Tool", "Nomove Hand"),
       box.lwd=0, box.col="white",
       col=c("red", "blue"), 
       lty=c(1, 1), pch=c(NA, NA), cex=1.0, 
       inset=c(0.025, 0.025))

