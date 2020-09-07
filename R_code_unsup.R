library(readxl)
library(devtools)
library(ggfortify)
library(ggrepel)
library(ggplot2)
library(corrplot)
library(factoextra)
library(dendextend)
library(mdendro)


# Dati Province dall'inizio dell'Epidemia 
Dati_Covid <- data.frame(read_excel("C:/Users/Marzio/Desktop/Progetti ML/Dati Covid_prov_FULL.xlsx"))
row.names(Dati_Covid)<- Dati_Covid[,2]
prov_full.pca<-prcomp(Dati_Covid[,5:11], center = TRUE,scale. = TRUE)
summary(prov_full.pca)
autoplot(prov_full.pca,loadings = TRUE,loadings.size= 4, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 4,label=TRUE,label.vjust=1.5,loadings.label.repel=T,label.repel=T)+ggtitle('Totale') + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
fviz_eig(prov_full.pca,barcolor = "red",barfill = "red",geom = c("bar"),addlabels= TRUE )+labs(title = "Variances - PCA",x = "Principal Components", y = "% of variances")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))
fviz_pca_var(prov_full.pca,col.var = "contrib",gradient.cols = c("red","orange","blue"),repel = TRUE,col.circle = "black",arrowsize = 1,labelsize = 0.5,jitter = list(what = "both", width = 1, height = 1) ) +theme_bw()+theme(plot.title = element_text(hjust = 0.5)) 

corrplot(cor(Dati_Covid[,5:11]))
palette = colorRampPalette(c("green", "blue", "red")) (20)
heatmap(x = cor(Dati_Covid[,5:11]), col = palette, symm = TRUE, margins = c(10, 10), main = 'Provinces',dist(Dati_Covid[,5:11],method = 'euclidean'))

# Dati Province nel mese di Agosto 
Dati_Covid_prov_AUG <- data.frame(read_excel("C:/Users/Marzio/Desktop/Progetti ML/Dati Covid_prov_AUG.xlsx"))
row.names(Dati_Covid_prov_AUG)<- Dati_Covid_prov_AUG[,2]
prov_aug.pca<-prcomp(Dati_Covid_prov_AUG[,5:11], center = TRUE,scale. = TRUE)
autoplot(prov_aug.pca,loadings = TRUE,loadings.size= 4, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 4,label=TRUE,label.vjust=1.5,loadings.label.repel=T,label.repel=T) +ggtitle('Agosto 2020') +theme_bw()+theme(plot.title = element_text(hjust = 0.5))
fviz_eig(prov_aug.pca,barcolor = "red",barfill = "red",geom = c("bar"),addlabels= TRUE )+labs(title = "Variances - PCA",x = "Principal Components", y = "% of variances")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))
fviz_pca_var(prov_aug.pca,col.var = "contrib",gradient.cols = c("red","orange","blue"),repel = TRUE,col.circle = "black",arrowsize = 1,labelsize = 0.5,jitter = list(what = "both", width = 1, height = 1) ) +theme_bw()+theme(plot.title = element_text(hjust = 0.5)) 
corrplot(cor(Dati_Covid_prov_AUG[,5:11]))
palette = colorRampPalette(c("green", "blue", "red")) (20)
heatmap(x = cor(Dati_Covid[,5:11]), col = palette, symm = TRUE, margins = c(10, 10),main = 'Province Aug',dist(Dati_Covid[,5:11],method = 'euclidean'))



# Dati Regioni Full
Dati_Covid_reg <- data.frame(read_excel("C:/Users/Marzio/Desktop/Progetti ML/Dati Covid reg_FULL.xlsx"))
row.names(Dati_Covid_reg) <-Dati_Covid_reg[,1]
reg_full.pca<-prcomp(Dati_Covid_reg[,2:11], center = TRUE,scale. = TRUE)
autoplot(reg_full.pca,loadings = TRUE,loadings.size= 4, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 4,label=FALSE,label.vjust=1.5,loadings.label.repel=T,label.repel=T) +ggtitle('Regions') +theme_bw()+theme(plot.title = element_text(hjust = 0.5))
summary(reg_full.pca)
fviz_eig(reg_full.pca,barcolor = "red",barfill = "red",geom = c("bar"),addlabels= TRUE )+labs(title = "Variances - PCA",x = "Principal Components", y = "% of variances")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))
fviz_pca_var(reg_full.pca,col.var = "contrib",gradient.cols = c("red","orange","blue"),repel = TRUE,col.circle = "black",arrowsize = 1,labelsize = 0.5,jitter = list(what = "both", width = 1, height = 1) ) +theme_bw()+theme(plot.title = element_text(hjust = 0.5))
corrplot(cor(Dati_Covid_reg[,2:11]))
palette = colorRampPalette(c("green", "blue", "red")) (20)
heatmap(x = cor(Dati_Covid_reg[,2:11]), col = palette, symm = TRUE, margins = c(10, 10),main = 'Regions', dist(Dati_Covid_reg[,2:11],method = 'euclidean'))
dend.dat<-dist(Dati_Covid_reg[,2:11] )
ciccio<-as.ggdend(as.dendrogram(hclust(dend.dat,method= "average")))
ggplot(ciccio,horiz = TRUE)
fviz_dend(hclust(dend.dat,method= "average"),k = 10,palette="npg", repel=TRUE,color_labels_by_k = TRUE,cex = 0.5) +theme(plot.title = element_text(hjust = 0.5)) 
ntb(as.dendrogram(hclust(dend.dat,method= "average")))



Dati_Covid_reg_AUG <- data.frame(read_excel("C:/Users/Marzio/Desktop/Progetti ML/Dati Covid reg_AUG.xlsx"))
row.names(Dati_Covid_reg_AUG) <-Dati_Covid_reg_AUG[,1]
reg_aug.pca<-prcomp(Dati_Covid_reg_AUG[,2:11], center = TRUE,scale. = TRUE)
autoplot(reg_aug.pca,loadings = TRUE,loadings.size= 4, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 4,label=FALSE,label.vjust=1.5,loadings.label.repel=T,label.repel=T)+ggtitle('Dati Regionali - Agosto 2020') +theme_bw()+theme(plot.title = element_text(hjust = 0.5)) 
summary(reg_aug.pca)
fviz_eig(reg_aug.pca,barcolor = "red",barfill = "red",geom = c("bar"),addlabels= TRUE )+labs(title = "Variances - PCA",x = "Principal Components", y = "% of variances")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))
fviz_pca_var(reg_aug.pca,col.var = "contrib",gradient.cols = c("red","orange","blue"),repel = TRUE,col.circle = "black",arrowsize = 1,labelsize = 0.5,jitter = list(what = "both", width = 1, height = 1) ) +theme_bw()+theme(plot.title = element_text(hjust = 0.5))
corrplot(cor(Dati_Covid_reg_AUG[,2:11]))
palette = colorRampPalette(c("green", "blue", "red")) (20)
heatmap(x = cor(Dati_Covid_reg_AUG[,2:11]), col = palette, symm = TRUE, margins = c(10, 10),main = 'Regioni Aug',dist(Dati_Covid_reg_AUG[,2:11],method = 'euclidean'))





Covid_mond <- data.frame(read_excel("C:/Users/Marzio/Desktop/Progetti ML/Covid_mond.xlsx"))
row.names(Covid_mond) <-Covid_mond[,1]
world.pca<-prcomp(Covid_mond[,6:14], center = TRUE,scale. = TRUE)
fviz_pca_var(world.pca,col.var = "contrib",gradient.cols = c("red","orange","blue"),repel = TRUE,col.circle = "black",arrowsize = 1,labelsize = 0.5,jitter = list(what = "both", width = 1, height = 1) ) +theme_bw()+theme(plot.title = element_text(hjust = 0.5)) 
fviz_eig(world.pca,barcolor = "red",barfill = "red",geom = c("bar"),addlabels= TRUE )+labs(title = "Variances - PCA",x = "Principal Components", y = "% of variances")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))

autoplot(peppo4,loadings = TRUE,loadings.size= 4, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 5,label=FALSE,label.vjust=1.5,loadings.label.repel=T,label.repel=T)+ggtitle('World') +theme_bw()+theme(plot.title = element_text(hjust = 0.5)) 
summary(world.pca)
corrplot(cor(Covid_mond[,6:14]))
palette = colorRampPalette(c("green", "blue", "red")) (20)
heatmap(x = cor(Covid_mond[,6:14]), col = palette, symm = TRUE, margins = c(10, 10),main = 'World',dist(Covid_mond[,6:14],method = 'euclidean'))





+geom_text_repel(aes(label = row.names(Dati_Covid_reg)))