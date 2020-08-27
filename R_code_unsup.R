library(readxl)
library(devtools)
library(ggfortify)
library(ggrepel)
library(ggplot2)
library(corrplot)
Dati_Covid <- data.frame(read_excel("C:/Users/Marzio/Desktop/Progetti ML/Dati Covid_prov_FULL.xlsx"))
row.names(Dati_Covid)<- Dati_Covid[,2]
peppo<-prcomp(Dati_Covid[,5:11], center = TRUE,scale. = TRUE)
autoplot(peppo,loadings = TRUE,loadings.size= 4, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 4,label=TRUE,label.vjust=1.5,loadings.label.repel=T,label.repel=T)+ggtitle('Totale') + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
corrplot(cor(Dati_Covid[,5:11]))
palette = colorRampPalette(c("green", "blue", "red")) (20)
heatmap(x = cor(Dati_Covid[,5:11]), col = palette, symm = TRUE, margins = c(10, 10), main = 'Province TOT',dist(Dati_Covid_reg[,2:11],method = 'euclidean'))


Dati_Covid_prov_AUG <- data.frame(read_excel("C:/Users/Marzio/Desktop/Progetti ML/Dati Covid_prov_AUG.xlsx"))
row.names(Dati_Covid_prov_AUG)<- Dati_Covid_prov_AUG[,2]
peppo1<-prcomp(Dati_Covid_prov_AUG[,5:11], center = TRUE,scale. = TRUE)
autoplot(peppo1,loadings = TRUE,loadings.size= 4, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 4,label=TRUE,label.vjust=1.5,loadings.label.repel=T,label.repel=T) +ggtitle('Agosto 2020') +theme_bw()+theme(plot.title = element_text(hjust = 0.5))
corrplot(cor(Dati_Covid_prov_AUG[,5:11]))
palette = colorRampPalette(c("green", "blue", "red")) (20)
heatmap(x = cor(Dati_Covid[,5:11]), col = palette, symm = TRUE, margins = c(10, 10),main = 'Province Aug',dist(Dati_Covid_reg[,2:11],method = 'euclidean'))




Dati_Covid_reg <- data.frame(read_excel("C:/Users/Marzio/Desktop/Progetti ML/Dati Covid reg_FULL.xlsx"))
row.names(Dati_Covid_reg) <-Dati_Covid_reg[,1]
peppo2<-prcomp(Dati_Covid_reg[,2:12], center = TRUE,scale. = TRUE)


autoplot(peppo2,loadings = TRUE,loadings.size= 4, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 4,label=FALSE,label.vjust=1.5,loadings.label.repel=T,label.repel=T) +ggtitle('Dati regionali - Totale') +theme_bw()+theme(plot.title = element_text(hjust = 0.5))
summary(peppo2)
corrplot(cor(Dati_Covid_reg[,2:11]))
palette = colorRampPalette(c("green", "blue", "red")) (20)
heatmap(x = cor(Dati_Covid_reg[,2:11]), col = palette, symm = TRUE, margins = c(10, 10),main = 'Regioni Tot', dist(Dati_Covid_reg[,2:11],method = 'euclidean'))



Dati_Covid_reg_AUG <- data.frame(read_excel("C:/Users/Marzio/Desktop/Progetti ML/Dati Covid reg_AUG.xlsx"))
row.names(Dati_Covid_reg_AUG) <-Dati_Covid_reg_AUG[,1]
peppo3<-prcomp(Dati_Covid_reg_AUG[,2:11], center = TRUE,scale. = TRUE)


autoplot(peppo3,loadings = TRUE,loadings.size= 4, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 4,label=FALSE,label.vjust=1.5,loadings.label.repel=T,label.repel=T)+ggtitle('Dati Regionali - Agosto 2020') +theme_bw()+theme(plot.title = element_text(hjust = 0.5)) 
summary(peppo3)
corrplot(cor(Dati_Covid_reg_AUG[,2:11]))
palette = colorRampPalette(c("green", "blue", "red")) (20)
heatmap(x = cor(Dati_Covid_reg_AUG[,2:11]), col = palette, symm = TRUE, margins = c(10, 10),main = 'Regioni Aug',dist(Dati_Covid_reg[,2:11],method = 'euclidean'))


+geom_text_repel(aes(label = row.names(Dati_Covid_reg)))