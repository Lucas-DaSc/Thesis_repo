## Script pour TDS

install.packages("pROC")
install.packages("dplyr")

# Charger les packages
library(pROC)
library(dplyr)

class(data)
class(data$Participant)
class(data$Estimation)
class(data$real_category)

rm(list=ls())
data <- read.csv(file = "Data.csv", header = TRUE, sep = ";")

data$Estimation <- sub('1','0',data$Estimation)
data$Estimation <- sub('2','1',data$Estimation)
data$Estimation <- as.numeric(data$Estimation)
data$Distance   <- as.integer(data$Distance)
data$Participant <- as.numeric(data$Participant)


is.na(data$Estimation)
which(is.na(data$Estimation),arr.ind=T)
DR<-which(is.na(data$Estimation),arr.ind=T)
data<-data[-DR,]

is.na(data$Distance)
which(is.na(data$Distance),arr.ind=T)
DR<-which(is.na(data$Distance),arr.ind=T)
data<-data[-DR,]

data <- data[data$RT != 5000, ]
data <- data[data$RT >= 200, ]

data <- data %>%
  filter(Mesure == "CD")
data <- data %>%
  filter(Condition == "Outil")
data <- data %>%
  filter(Condition == "Main")
data <- data %>%
  filter(Movement == "Move")
data <- data %>%
  filter(Movement == "Nomove")
data <- data %>%
  filter(Effector == "Outil")
data <- data %>%
  filter(Effector == "Main")
data <- data %>%
  filter(Effector != "Nothing")


data <- data %>%
  mutate(real_category = ifelse(Distance <= 6, 0, 1))
data <- data %>%
  mutate(real_category = ifelse(Distance <= 5, 0, 1))
data <- data %>%
  mutate(real_category = ifelse(Distance <= 4, 0, 1))


hits <- sum(data$Estimation == 0 & data$real_category == 0)
misses <- sum(data$Estimation == 1 & data$real_category == 0)
false_alarms <- sum(data$Estimation == 0 & data$real_category == 1)
correct_rejections <- sum(data$Estimation == 1 & data$real_category == 1)

## Court = Signal 

hit_rate <- hits / (hits + misses)
false_alarm_rate <- false_alarms / (false_alarms + correct_rejections)

cat("Taux de hits:", hit_rate, "\n")
cat("Taux de fausses alarmes:", false_alarm_rate, "\n")

roc_obj <- roc(data$real_category, data$Estimation)
plot(roc_obj, main = "Courbe ROC", col = "blue")
auc(roc_obj)

z_hit_rate <- qnorm(hit_rate)
z_false_alarm_rate <- qnorm(false_alarm_rate)

d_prime <- z_hit_rate - z_false_alarm_rate
criterion_c <- -0.5 * (z_hit_rate + z_false_alarm_rate)

cat("Sensibilité (d'):", d_prime, "\n")
cat("Critère de décision (c):", criterion_c, "\n")

hit_rate
false_alarm_rate


data_condition1 <- filter(data, Effector == "Outil")
data_condition2 <- filter(data, Effector == "Main")

data_condition1 <- filter(data, Effector == "Outil")
data_condition2 <- filter(data, Effector == "Main")

# Calculer la courbe ROC pour chaque condition
roc_condition1 <- roc(data_condition1$real_category, data_condition1$Estimation)
roc_condition2 <- roc(data_condition2$real_category, data_condition2$Estimation)

auc(roc_condition1)
auc(roc_condition2)

# Tracer les courbes ROC
plot(roc_condition1, main = "ROC curve for tool vs hand conditions", col = "blue")
plot(roc_condition2, col = "red", add = TRUE)

legend("right", 
       legend=c("Hand Condition", "Tool Condition"),
       box.lwd=0, box.col="white",
       col=c("red", "blue", "green","maroon", "grey"), 
       lty=c(1, 1), pch=c(NA, NA), cex=1.0, 
       inset=c(0.025, 0.025))


# Calculer les taux de hits et de fausses alarmes pour condition1
hits_condition1 <- sum(data_condition1$Estimation == 0 & data_condition1$real_category == 0)
misses_condition1 <- sum(data_condition1$Estimation == 1 & data_condition1$real_category == 0)
false_alarms_condition1 <- sum(data_condition1$Estimation == 0 & data_condition1$real_category == 1)
correct_rejections_condition1 <- sum(data_condition1$Estimation == 1 & data_condition1$real_category == 1)

hit_rate_condition1 <- hits_condition1 / (hits_condition1 + misses_condition1)
false_alarm_rate_condition1 <- false_alarms_condition1 / (false_alarms_condition1 + correct_rejections_condition1)


# Calculer les taux de hits et de fausses alarmes pour condition2
hits_condition2 <- sum(data_condition2$Estimation == 0 & data_condition2$real_category == 0)
misses_condition2 <- sum(data_condition2$Estimation == 1 & data_condition2$real_category == 0)
false_alarms_condition2 <- sum(data_condition2$Estimation == 0 & data_condition2$real_category == 1)
correct_rejections_condition2 <- sum(data_condition2$Estimation == 1 & data_condition2$real_category == 1)

hit_rate_condition2 <- hits_condition2 / (hits_condition2 + misses_condition2)
false_alarm_rate_condition2 <- false_alarms_condition2 / (false_alarms_condition2 + correct_rejections_condition2)

# Calculer la sensibilité (d') et le critère de décision (c) pour condition1
z_hit_rate_condition1 <- qnorm(hit_rate_condition1)
z_false_alarm_rate_condition1 <- qnorm(false_alarm_rate_condition1)

d_prime_condition1 <- z_hit_rate_condition1 - z_false_alarm_rate_condition1
criterion_c_condition1 <- -0.5 * (z_hit_rate_condition1 + z_false_alarm_rate_condition1)

# Calculer la sensibilité (d') et le critère de décision (c) pour condition2
z_hit_rate_condition2 <- qnorm(hit_rate_condition2)
z_false_alarm_rate_condition2 <- qnorm(false_alarm_rate_condition2)

d_prime_condition2 <- z_hit_rate_condition2 - z_false_alarm_rate_condition2
criterion_c_condition2 <- -0.5 * (z_hit_rate_condition2 + z_false_alarm_rate_condition2)


cat("Condition 1 - Sensibilité (d'):", d_prime_condition1, "\n")
cat("Condition 1 - Critère de décision (c):", criterion_c_condition1, "\n")
cat("Condition 1 - Hit Rate   :", hit_rate_condition1, "\n")
cat("Condition 1 - False rate :", false_alarm_rate_condition1, "\n")


cat("Condition 2 - Sensibilité (d'):", d_prime_condition2, "\n")
cat("Condition 2 - Critère de décision (c):", criterion_c_condition2, "\n")
cat("Condition 2 - Hit Rate   :", hit_rate_condition2, "\n")
cat("Condition 2 - False rate :", false_alarm_rate_condition2, "\n")


## Moyenne

# Définir la fonction fit_model
fit_model <- function(participant_data) {
  hits <- sum(participant_data$Estimation == 0 & participant_data$real_category == 0)
  misses <- sum(participant_data$Estimation == 1 & participant_data$real_category == 0)
  false_alarms <- sum(participant_data$Estimation == 0 & participant_data$real_category == 1)
  correct_rejections <- sum(participant_data$Estimation == 1 & participant_data$real_category == 1)
  
  hit_rate <- hits / (hits + misses)
  false_alarm_rate <- false_alarms / (false_alarms + correct_rejections)
  
  hit_rate <- pmin(pmax(hit_rate, 0.001), 0.999)
  false_alarm_rate <- pmin(pmax(false_alarm_rate, 0.001), 0.999)
  
  z_hit_rate <- qnorm(hit_rate)
  z_false_alarm_rate <- qnorm(false_alarm_rate)
  
  d_prime <- z_hit_rate - z_false_alarm_rate
  criterion_c <- -0.5 * (z_hit_rate + z_false_alarm_rate)
  
  return(data.frame(hits = hits, misses = misses, hit_rate = hit_rate, false_alarm_rate = false_alarm_rate, d_prime = d_prime, criterion_c = criterion_c))
}

# Appliquer fit_model à chaque participant
results <- lapply(participants, fit_model)
print(results)

# Vérifier les résultats pour chaque participant
for (i in seq_along(results)) {
  cat("Participant", names(results)[i], ": Hits =", results[[i]]$hits, "\n")
}

results <- lapply(participants, fit_model)
print(results)
results_df <- do.call(rbind, results)
write.csv(results_df, file = "SDT.csv",  row.names = FALSE)

mean(results_df$criterion_c)
sd(results_df$criterion_c) 

mean(results_df$d_prime)
sd(results_df$d_prime) 

