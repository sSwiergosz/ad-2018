#zad1

'''
rats1 <- c(143, 165,
           188, 188, 190, 192, 206, 208, 212, 216, 220, 227, 230, 235, 246, 265, 303,
           216+, 244+)

rats2 <- c(142, 157, 163, 198, 205, 232, 232, 232, 233, 233, 233,
           233, 239, 240, 261, 280, 280, 295, 295, 323, 204+, 344+)
'''

library(survival)
library(survminer)

rats1 <- c(143, 165,
           188, 188, 190, 192, 206, 208, 212, 216, 220, 227, 230, 235, 246, 265, 303,
           216, 244)

rats2 <- c(142, 157, 163, 198, 205, 232, 232, 232, 233, 233, 233,
           233, 239, 240, 261, 280, 280, 295, 295, 323, 204, 344)

allRats <- c(rats1,rats2)

rats1.cens <- Surv(rats1, c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0))
rats2.cens <- Surv(rats2, c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0))
allRats.cens <- Surv(allRats, c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0))

### MODELE OSBNE DLA GRUP ##

model.rats1 <- survfit(rats1.cens ~ 1 )
summary(model.rats1)
print(model.rats1, rmean = 'common')

model.rats2 <- survfit(rats2.cens ~ 1 )
summary(model.rats2)
print(model.rats2, rmean = 'common')

length(rats1.cens)
length(rats2.cens)

rats.data <- data.frame(grupa = c(rep("g1", times=length(rats1.cens)), rep("g2", times=length(rats2.cens))), czas_przezycia = allRats.cens)
rats.data
rats.model <- survfit(rats.data$czas_przezycia ~ rats.data$grupa)
summary(rats.model)
plot(rats.model,col = c("red","blue"))
ggsurvplot(rats.model, 
           data = rats.data,
           risk.table = TRUE,       
           pval = TRUE, 
           conf.int = TRUE,
           fun = 'pct',
           legend.labs = c('Group1', 
                           'Group2'),
           legend.title = 'Rats')
#data.frame(result=c(rep("yes", times=length(yesFiles)), rep("no", times=length(noFiles))), name=c(yesFiles, noFiles))

#Zad 2
complete.cases(cancer)
str(cancer)
cancer.without.nas <- cancer[complete.cases(cancer),]
str(cancer.without.nas)
complete.cases(cancer.without.nas)

cancer.without.nas



