rm(list=ls())
library(rstudioapi)
current_path<-getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)

# Caricamento dati
data <- read_excel("Marketing_Campaign.xlsx", sheet = "Features")
targets <- read_excel("Marketing_Campaign.xlsx", sheet = "Targets")
data <- data.frame(data,targets)
data <- data[,-21]
######## DATA QUALITY ###########
# somma per colonna tutti gli na di data:
colSums(is.na(data))

# Vediamo se ci sono righe duplicate
duplicated(data)
# andiamo a vedere quale riga è duplicata
which(duplicated(data))
# Non ci sono righe duplicate

summary(data)
# Sostituiamo 999 della variabile pdays con -1
data$pdays[which(data$pdays==999)] = -1

## Trasformare in dummy o categoriche le variabili che sono tali
data$House.Ownershi<-ifelse(data$House.Ownershi == "yes", 1, 0)
data$House.Ownershi<- as.factor(data$House.Ownershi)
data$Existing.Loans<-ifelse(data$Existing.Loans == "yes", 1, 0)
data$Existing.Loans<- as.factor(data$Existing.Loans)
data$Marital.Status<-as.factor(data$Marital.Status)
data$Previous.Default<-ifelse(data$Previous.Default == "no", 0, ifelse(data$Previous.Default == "yes", 1, -1 ))
data$Previous.Default<-as.factor(data$Previous.Default)
data$Contact.Channel <- as.factor(data$Contact.Channel)
data$poutcome<-as.factor(data$poutcome)
data$Target<-as.factor(data$Target)
data$Target<-ifelse(data$Target == 1, 0, 1)
data$Month<-as.factor(data$Month)
data$Day.of.Week<-as.factor(data$Day.of.Week)

str(data)

# BOXPLOT --- vediamo se ci sono outliers
dev.new()
par(mfrow=c(1,2))
boxplot(data$Age, col="lightgreen", main="Age")
boxplot(data$Call.Duration, col="lightgreen", main="Call Duration") 
dev.new()
par(mfrow=c(1,2))
boxplot(data$pdays, col="lightgreen", main="pdays")
boxplot(data$previous, col="lightgreen", main="previous")
dev.new()
par(mfrow=c(1,2))
boxplot(data$emp_var_rate, col="lightgreen", main="emp_var_rate")
boxplot(data$cons_price_idx, col="lightgreen", main="cons_price_idx")
par(mfrow=c(1,3))
boxplot(data$cons_conf_idx, col="lightgreen", main="cons_conf_idx")
boxplot(data$euribor3m, col="lightgreen", main="euribor3m")
boxplot(data$Target, col="lightgreen", main="Target")


################## ANALISI ESPLORATIVA ###################

# Vediamo la frequenza assoluta di "Target"
tab<-data$Target%>%table()
tab
# Vediamo la frequenza relativa di "Target"
percentages<-tab%>%prop.table()%>%round(3)*100
percentages ## 88.7% --> 0 e 11.3% --> 1

# Raprresentiamo la frequenza relativa di Target con un grafico a torta
dev.new()
txt<-paste0(names(tab), '\n',percentages, '%')
txt
colors <- c("violet","orange")
pie(tab, labels = txt, col = colors)
legend("topright", legend = names(tab), fill = colors, title = "Target")


#### CORRELAZIONE TRA LE VARIABILI
data_aux <- data[,-c(1,3:11,16)]
dev.new()
matrcorr=cor(data_aux)
#library(ggcorrplot)
#ggcorrplot(matrcorr)
library(corrplot)
corrplot(matrcorr, method = "color", 
         addCoef.col = "black", # colore dei coefficienti
         number.cex = 0.7)

# codifico i mesi con i numeri e previous default in dummy 
library(writexl)
data$Month <- ifelse(mese == "jan", 1, ifelse(mese == "feb", 2, ifelse(mese == "mar", 3, ifelse(mese == "apr", 4, ifelse(mese == "may", 5, ifelse(mese == "jun", 6,ifelse(mese == "jul", 7,ifelse(mese == "aug", 8, ifelse(mese == "sep", 9, ifelse(mese == "oct", 10, ifelse(mese == "nov", 11, 12)))))))))))
data$Previous.Default<-ifelse(data$Previous.Default == "no", 0, ifelse(data$Previous.Default == "yes", 1, -1 ))

# Creiamo le fasce d'età
data$Age_Group <- cut(data$Age, 
                    breaks = c(18, 30, 40, 50, 60, 100), 
                    labels = c("18-30", "31-40", "41-50", "51-60", "60+"), 
                    right = TRUE)

# Controlliamo il risultato
table(data$Age_Group)

write_xlsx(data, "dataset_ass3.xlsx")

# Il più alto numero di yes ce l'abbiamo nel job = admin.
# Call duration maggiore porta ad una maggiore probabilità di dire yes
# House Ownership non influenza il valore della variabile target

#### Facciamo UNDERSAPMPLING DELLA CLASSE Target = 0
#("timechange")
library(caret)
library(e1071)
library(ROSE)

# Applichiamo l'undersampling stratificato con downSample()
set.seed(123)  # Per riproducibilità
new_data <- downSample(x = data[, -which(names(data) == "Target")],  
                          y = as.factor(data$Target))

# Rinominiamo la variabile target
colnames(new_data)[ncol(new_data)] <- "Target"

# Controlliamo il nuovo bilanciamento
table(new_data$Target)
summary(new_data)

############ CLUSTER #####################################à
# sistemo il dataset e creo le categorie numeriche
new_data$Marital.Status <- ifelse(new_data$Marital.Status == "married", 0, 
                                  ifelse(new_data$Marital.Status == "single", 1, 
                                         ifelse(new_data$Marital.Status == "divorced", 2, 3)))
new_data$Marital.Status <- as.factor(new_data$Marital.Status)

new_data$Contact.Channel <- ifelse(new_data$Contact.Channel == "cellular", 0, 1)
new_data$Contact.Channel <- as.factor(new_data$Contact.Channel)

new_data$poutcome <- ifelse(new_data$poutcome == "failure", 0, 
                            ifelse(new_data$poutcome == "success", 1, -1))
new_data$poutcome <- as.factor(new_data$poutcome)

new_data$Job <- as.factor(new_data$Job)
new_data$Education <- as.factor(new_data$Education)

library(dplyr)

# Creazione della colonna con le macro categorie
new_data <- new_data %>%
  mutate(job_category = case_when(
    Job %in% c("blue-collar", "housemaid", "services") ~ "Lavori Manuali e Operativi",
    Job %in% c("technician", "admin.") ~ "Professioni Tecnico-Amministrative",
    Job %in% c("management", "entrepreneur", "self-employed") ~ "Ruoli Dirigenziali e Imprenditoriali",
    Job %in% c("retired", "unemployed", "student", "unknown") ~ "Non Occupati e Altro",
    TRUE ~ "Altro" # Per gestire eventuali nuovi valori
  ))

# visualizzo tutte le categorie
unique(new_data$job_category)
# Creazione del mapping manuale delle categorie a numeri
new_data$job_category <- factor(new_data$job_category, 
                                levels = c("Lavori Manuali e Operativi", 
                                           "Professioni Tecnico-Amministrative", 
                                           "Ruoli Dirigenziali e Imprenditoriali", 
                                           "Non Occupati e Altro"))
                    

######### standardizzo escludendo variabili come RECORDID (1)
# 6 = previous default da capire come trattare! per ora esclusa
new_data_scaled <- new_data %>%
  mutate(
    across(.cols = where(is.numeric) & !c(1, 6), .fns = scale)  # Scala solo le variabili numeriche tranne 1 e 6
  )

View(new_data_scaled)
summary(new_data_scaled)


### Elbow method
library(ggplot2)
library(factoextra)

#rimuovo le variabili categoriali per fare clustering:
data_cluster <- new_data_scaled[, -c(1,3:11,16, 21,23)]
View(data_cluster)

# Calcolo del WCSS per diversi numeri di cluster
wcss <- vector()
for (k in 1:10) {
  kmeans_model <- kmeans(data_cluster, centers = k, nstart = 10000)
  wcss[k] <- kmeans_model$tot.withinss
}

# Grafico del metodo del gomito
dev.new()
elbow_plot <- data.frame(Clusters = 1:10, WCSS = wcss)
ggplot(elbow_plot, aes(x = Clusters, y = WCSS)) +
  geom_point() +
  geom_line()

# da valutare numero di cluster da 3 a 5 consigliato

## valuto la misura di silhouette al variare del numero di gruppi
library(cluster)
# Ciclo per i valori di k da 3 a 5
for(k in 3:5){
  dev.new()
  # Esegui K-Means
  km <- kmeans(data_cluster, centers = k, nstart = 10000)
  # Calcola il silhouette
  sil <- silhouette(km$cluster, dist(data_cluster))
  # Crea il grafico silhouette per il valore corrente di k
  plot(sil, main = paste("Silhouette Plot per k =", k), col = rainbow(k))
}

set.seed(124)
fit <- kmeans(data_cluster, 4, nstart=10000) # 4 cluster solution
print(fit)
print(fit$centers)
library(gridExtra)

dev.new()
fviz_cluster(fit,data_cluster_ok, geom = "point", stand=FALSE, palette= c( "green","red","yellow","blue"))

new_set=data.frame(as.factor(new_data_scaled$`Client ID`),data_aux,as.factor(fit$cluster))
names(new_set)=c("Client ID",names(data_cluster),"gruppo")
View(new_set)

gruppo1=new_set[which(new_set$gruppo==1),1:12]
View(gruppo1)

gruppo2=new_set[which(new_set$gruppo==2),1:12]
View(gruppo2)

gruppo3=new_set[which(new_set$gruppo==3),1:12]
View(gruppo3)

gruppo4=new_set[which(new_set$gruppo==4),1:12]
View(gruppo4)

summary(gruppo1)
# 84 elementi
# da 2 a 3 prodotti acquistati
# hanno usufruito di sconti per una media di 2900
# più della metà di coloro che appartengono a questo gruppo ha acquistato prodotto P

summary(gruppo2)
# 510 elementi
# Quasi tutti hanno acquistato un solo item
# la maggior parte ha acquistato prodotto A
# Hanno usufruito in media di uno sconto medio di 300

summary(gruppo3)
# 6 elementi
# tutti hanno acquistato e restituito un item 
# Hanno usufruito in media di uno sconto di 2700

summary(gruppo4)
# 2388 elementi
# tutti hanno acquistato ed ordinato uno/due item e nessuno ha effettuato resi
# La maggior parte di coloro che appartengono a questo gruppo hanno acquistato prodotto P
# Hanno usufruito in media di uno sconto di 2100



########### REGRESSIONE LOGISTICA #############
# Dividiamo il set di dati in training e test set
library(caTools)
set.seed(123)
sample_data<-sample.split(new_data, SplitRatio = 0.60)
train_data<-subset(new_data, sample_data==TRUE)
test_data<-subset(new_data, sample_data==FALSE)
prop.table(table(train_data$Target))
prop.table(table(test_data$Target))

train_data1<-train_data[-1642,]

# Calcola l'IQR per la variabile Call.Duration
Q1 <- quantile(train_data1$Call.Duration, 0.25)
Q3 <- quantile(train_data1$Call.Duration, 0.75)
IQR_value <- Q3 - Q1

# Identifica gli outlier
lower_bound <- Q1 - 5 * IQR_value
upper_bound <- Q3 + 5 * IQR_value

# Filtra il dataset rimuovendo gli outlier
train_data2 <- subset(train_data1, Call.Duration > lower_bound & Call.Duration < upper_bound)
View(train_data2)
boxplot(train_data2$Call.Duration, col="lightgreen", main="Call.Duration")


# Calcola l'IQR per la variabile Call.Duration
Q1 <- quantile(train_data2$campaign, 0.25)
Q3 <- quantile(train_data2$campaign, 0.75)
IQR_value <- Q3 - Q1

# Identifica gli outlier
lower_bound <- Q1 - 5 * IQR_value
upper_bound <- Q3 + 5 * IQR_value

# Filtra il dataset rimuovendo gli outlier
train_data3 <- subset(train_data2, campaign > lower_bound & campaign < upper_bound)
View(train_data3)
boxplot(train_data3$campaign, col="lightgreen", main="campaign")

## Sistemazione dataset
train_data3$Marital.Status<-ifelse(train_data3$Marital.Status == "married", 0, ifelse(train_data3$Marital.Status == "single", 1, ifelse(train_data3$Marital.Status == "divorced", 2,3 )))
train_data3$Marital.Status <- as.factor(train_data3$Marital.Status)
train_data3$Contact.Channel<-ifelse(train_data3$Contact.Channel == "cellular", 0, 1)
train_data3$Contact.Channel <- as.factor(train_data3$Contact.Channel)
train_data3$poutcome<-ifelse(train_data3$poutcome == "failure", 0, ifelse(train_data3$poutcome == "success" , 1, -1))
train_data3$poutcome <- as.factor(train_data3$poutcome)
train_data3$Job <- as.factor(train_data3$Job)
train_data3$Education <- as.factor(train_data3$Education)


library(dplyr)

# Creazione della colonna con le macro categorie
train_data3 <- train_data3 %>%
  mutate(job_category = case_when(
    Job %in% c("blue-collar", "housemaid", "services") ~ "Lavori Manuali e Operativi",
    Job %in% c("technician", "admin.") ~ "Professioni Tecnico-Amministrative",
    Job %in% c("management", "entrepreneur", "self-employed") ~ "Ruoli Dirigenziali e Imprenditoriali",
    Job %in% c("retired", "unemployed", "student", "unknown") ~ "Non Occupati e Altro",
    TRUE ~ "Altro" # Per gestire eventuali nuovi valori
  ))

library(car)
library(lmtest)
mod0=glm( Target ~ Age + Marital.Status + Previous.Default + House.Ownershi + Existing.Loans + Month + Call.Duration + campaign + pdays + previous + emp_var_rate + cons_conf_idx + cons_price_idx + euribor3m + job_category, family = binomial( link = logit ) , data = train_data3)
summary(mod0)
vif(mod0)

mod1=glm(Target ~ Age + Marital.Status + Previous.Default + House.Ownershi + Existing.Loans + Month + Call.Duration + campaign + pdays + previous + emp_var_rate + cons_conf_idx + cons_price_idx + job_category, family = binomial( link = logit ) , data = train_data3)
summary(mod1)
vif(mod1)
lrtest(mod0,mod1) # accettiamo H0 ossia il modello con meno regressori (modello 1)

mod2=glm(Target ~ Age + Marital.Status + Previous.Default + House.Ownershi + Month + Call.Duration + campaign + pdays + previous + emp_var_rate + cons_conf_idx + cons_price_idx + job_category, family = binomial( link = logit ) , data = train_data3)
summary(mod2)
vif(mod2)
lrtest(mod1,mod2) # accettiamo H0 (modello 2 è più significativo)

mod3=glm( Target ~ Age + Marital.Status + Previous.Default + House.Ownershi + Month + Call.Duration + pdays + previous + emp_var_rate + cons_conf_idx + cons_price_idx + job_category, family = binomial( link = logit ) , data = train_data3)
summary(mod3)
vif(mod3)
lrtest(mod2,mod3) # accettiamo H0 (il modello 3 è più significativo)

mod4=glm( Target ~ Age + Marital.Status + Previous.Default + Month + Call.Duration + pdays + previous + emp_var_rate + cons_conf_idx + cons_price_idx + job_category, family = binomial( link = logit ) , data = train_data3)
summary(mod4)
vif(mod4)
lrtest(mod3,mod4) # accettiamo H0 (il modello 4 è più significativo)

mod5=glm( Target ~ Age + Marital.Status + Previous.Default + Call.Duration + pdays + previous + emp_var_rate + cons_conf_idx + cons_price_idx + job_category, family = binomial( link = logit ) , data = train_data3)
summary(mod5)

lrtest(mod4,mod5) # accettiamo H0 (il modello 5 è più significativo)

mod6=glm( Target ~ Age + Previous.Default + Call.Duration + pdays + emp_var_rate + cons_conf_idx + cons_price_idx + job_category, family = binomial( link = logit ) , data = train_data3)
summary(mod6)
lrtest(mod5,mod6)

table(train_data3$Marital.Status, train_data3$Target)
chisq.test(table(train_data3$Marital.Status, train_data3$Target))


# Calcola i residui Pearson dal modello logit
residuals_pearson <- residuals(mod4, type = "pearson")

# Esegue il test di Durbin-Watson sui residui
durbinWatsonTest(residuals_pearson)

library(stats)

# Test di Ljung-Box per autocorrelazione sui residui Pearson
Box.test(residuals_pearson, type = "Ljung-Box")

## influential values: grafico delle leve per osservazioni più influenti
dev.new()
plot(mod4, which=4, id.n = 10)

library(car)
dev.new()
influencePlot(mod4)

# separation: se il modello separa bene tra categorie 0 e 1
library(detectseparation)
separation_check <- detect_separation(mod4)
print(separation_check)

# Vediamo se c'è overfitting
# Accuracy del training
predictions_train<-predict(mod4, train_data3, type="response")

predictions.hat_train<-ifelse(predictions_train>0.5,1,0)

accuracy_training<-table(predictions.hat_train, train_data3$Target)
accuracy_training
sum(diag(accuracy_training))/sum(accuracy_training)*100 # accuracy=84.95%

# Accuracy del testing
predictions<-predict(mod4, test_data, type="response")

predictions.hat<-ifelse(predictions>0.5,1,0)

accuracy_test<-table(predictions.hat, test_data$Target)
accuracy_test
sum(diag(accuracy_test))/sum(accuracy_test)*100 # accuracy=85.24%

#pseudo Rquadro McFadden
library(pscl)
pR2(mod4) ## 0.47 quindi molto buono

