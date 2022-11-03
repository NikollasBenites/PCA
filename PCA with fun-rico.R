library(MASS)
library(tidyverse)
library(readr)
library(tidyr)
library(matlib)
library(pracma)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ape)
library(ggdendro)
library(ggpubr)
library(ggcorrplot)
library(pvclust)
library(GGally)
library(ggplot2)
library(plotly)
library(ggrepel)
library(generics)
library(h2o)
library(dplyr)
library(grid)
library(geoR)
library(candisc)
library(car)
library(extrafont)

source("fun-rico.R")

filename = "Rico-PCA-v4.CSV"
col = colorRampPalette(c('darkred','yellow', 'darkblue'))

### INPUT ###

if(0){
  base = list()
  # dados brutos
  base$raw = data_input(filename)
  
  # dados sem id, Fenotype, p12 (open/closed), Age
  base$clean0 = base$raw %>%  select(!c(id, Fenotype, p12, Age))
  
  # mesmos dados anteriores mas com Age
  base$clean1 = base$raw %>%  select(!c(id, Fenotype, p12))
  
  # dados transformados em normais sem Age
  # nome da variavel = final em "0" sem age; final em "1" com age
  base$norm0 = base$clean0 %>% map2_dfc(colnames(base$clean0), norm_all)
  
  # dados tranformados em nomais com Age
  base$norm1 = base$clean1 %>% map2_dfc(colnames(base$clean1), norm_all)
}

### MAKE PCA ###
if(0){
  #browser()
  base$PCA = list() 
  base$PCA$class = list()
  
  # PCA sobre dados puros sem Age
  #mInvert = base$clean0[1:84,1:12]* -1
  base$PCA$class$no_norm = PCA(as.matrix(base$clean0), ncp = 8, graph = T)
  vpn = base$PCA$class$no_norm$eig[,1]
  plot(vpn, type="b", ylim=c(0,6))
  
  # PCA sobre dados transformados em normais sem Age
  #mInvert2 = base$norm0[1:84,1:12]* -1
  base$PCA$class$norm = PCA(as.matrix(base$norm0), ncp = 8, graph = T)
  vpy = base$PCA$class$norm$eig[,1]
  lines(vpy, lty = 3)
}


### CORR PLOTS PCA 
if(0){
 
  par(mfrow=c(1,2))
  make_corr_plots(base$PCA$class$no_norm$var$cos2, 
                  title = "PCA no_norm" )
  make_corr_plots(base$PCA$class$norm$var$cos2, 
                  title = "PCA norm" )
  
  colnames(base$PCA$class$norm$var$cos2) = paste0("PC",1:8) #muda o nome dos Dim. pra PC
  colnames(base$PCA$class$no_norm$var$cos2) = paste0("PC",1:8) #muda o nome dos Dim. pra PC
  # corrplot(base$PCA$class$norm$var$cos2, method = 'square', outline = 'white',is.corr=F,
  #          addgrid.col = 'black',
  #        addCoefasPercent = F,
  #          #order = 'alphabet',
  #          tl.col = 'black',
  #          tl.cex = 0.5,
  #          tl.srt = 90,
  #          tl.offset = 0.3,
  #          col= col(15),mar=c(0,0,0,8),
  #          title="")
  # 
  # corrplot(base$PCA$class$no_norm$var$cos2, method = 'square', outline = 'white',is.corr=F,
  #          addgrid.col = 'black',
  #          addCoefasPercent = F,
  #          #order = 'alphabet',
  #          tl.col = 'black',
  #          tl.cex = 0.5,
  #          tl.srt = 90,
  #          tl.offset = 0.3,
  #          col= col(15),mar=c(0,0,0,8),
  #          title="")
}

### CIRCLE PLOTS PCA ###
if(0){
  p1 = fviz_pca_var(base$PCA$class$no_norm, col.var = "black", axes = c(1,2),
                   select.var = list(contrib = 9), 
                   title = "Var PCA no_norm")
  #print(p1)
  p2 = fviz_pca_var(base$PCA$class$norm, col.var = "black", axes = c(1,2),
                    select.var = list(contrib = 9), 
                    title = "Var PCA norm")
  #print(p2)
  p12 = ggarrange(p1, p2, ncol = 2)
  print(p12)
}

### VIOLIN PLOTS PCA ###
if(0){
  p1 = make_wallys(base$PCA$class$no_norm$ind$coord, 
                   base$clean1$Age,
                   title = "PCA no_norm")
  #print(p1)
  p2 = make_wallys(base$PCA$class$norm$ind$coord, 
                   base$clean1$Age,
                   title = "PCA norm")
  #print(p2)
  p12 = ggarrange(p1, p2, ncol = 2)
  print(p12)
}

### BIPLOT PCA ###
if(0){
  
  # [1] "Age"                "Rinput ss"          "Rinput instantly"   "Tau membrane"      
  # [5] "Sag mV"             "Rheobase pA"        "Activity threshold" "Threshold AP"      
  # [9] "Amplitude AP"       "Latency 1o PA s"    "Halfwidth PA"       "fAHP"              
  # [13] "Max rate of Rise"
  
  var.name = "Max rate of Rise" # VARIAVEL A DESTACAR O VETOR
  lab.name = var.name
  #m = base$CDA$no_norm$scores %>% as_tibble() %>% 
  #  relocate(Age, .after = last_col())
  m = base$PCA$class$no_norm$ind$coord
  p1 = make_ind_plots(ind.coord = m,
                      var.coord = base$PCA$class$no_norm$var$coord,
                      df = base$clean1, 
                      dim = c(1,2),
                      var.name = var.name, 
                      lab.name = lab.name, 
                      col = col ,
                      labs = "PC",
                      sign = c("+","-"),
                      title = "PCA no_norm",
                      xlim = c(-6,6),
                      ylim = c(-6,6))
  print(p1)
}
if(0){
  # m = base$CDA$norm$scores %>% as_tibble() %>%
  #   relocate(Age, .after = last_col())
  m = base$PCA$class$norm$ind$coord
  p2 = make_ind_plots(ind.coord = m,
                      var.coord = base$PCA$class$norm$var$coord,
                      df = base$clean1,
                      dim = c(1,2),
                      var.name = var.name,
                      lab.name = lab.name,
                      col = col ,
                      labs = "PC",
                      sign = c("+","-"),
                      title = "PCA norm",
                      xlim = c(-6,6),
                      ylim = c(-6,6))
  
  p12 = ggarrange(p1, p2, nrow = 2)
  print(p12)
}

### BOXPLOT PCA ###

# [1] "Age"                "Rinput ss"          "Rinput instantly"   "Tau membrane"      
# [5] "Sag mV"             "Rheobase pA"        "Activity threshold" "Threshold AP"      
# [9] "Amplitude AP"       "Latency 1o PA s"    "Halfwidth PA"       "fAHP"              
# [13] "Max rate of Rise"


if(0){
  var.name = "Max rate of Rise" 
  p = make_box_plot(ind.coord = base$PCA$class$no_norm$ind$coord, 
                    clean.coord = base$clean1, 
                    var.name = var.name,
                    col = col)
                    
                    
  print(p)
  
}

### LDA (biblioteca mais limitada) ###
if(0){
  #browser()
  par(mfrow=c(1,1))
  base$LDA = list()
  base$LDA$no_norm = make_LDA(base$clean1)
  v = base$LDA$no_norm$svd^2/sum(base$LDA$no_norm$svd^2)
  v = round(base$LDA$no_norm$svd^2,3)
  plot(v, type="b", ylim=c(0,30))
  base$LDA$norm = make_LDA(base$norm1)
  v = base$LDA$norm$svd^2/sum(base$LDA$norm$svd^2)
  v = round(base$LDA$norm$svd^2,3)
  lines(v, lty = 3)
  #plda <- predict(object = base$LDA$norm, newdata = )
}
#x = lda(as.factor(Species)~.,data=iris)

### LDA com candic (recomendado) ###
### LDA e CDA sao a mesma coisa!
if(0){
  par(mfrow=c(1,1))
  base$CDA = list()
  base$CDA$no_norm = make_CDA(base$clean1)
  vcn = round(base$CDA$no_norm$eigenvalues,3)
  plot(vcn, type="b", ylim=c(0,6))
  base$CDA$norm = make_CDA(base$norm1)
  vcy = round(base$CDA$norm$eigenvalues,3)
  lines(vcy, lty = 3)
  # Anova(x)
  # cqplot(x)
  # plot(y)
  
}

### CORR PLOT LDA ###
if(0){
  par(mfrow=c(1,2))
  m1 = abs(t(t(as.matrix(base$CDA$no_norm$structure)) * sqrt(base$CDA$no_norm$eigenvalues)))
  m2 = abs(t(t(as.matrix(base$CDA$norm$structure)) * sqrt(base$CDA$norm$eigenvalues)))
  make_corr_plots(m1[,1:8], 
                  title = "LDA/CDA no_norm")
  make_corr_plots(m2[,1:8], 
                  title = "LDA/CDA norm")
}


### P-VALUES GRAPHICS
if(0){
  
  m = tibble(id = 1:(nrow(m)),
             PCA = vpn, PCA_Trans = vpy,
             LDA = vcn, LDA_Trans = vcy )
  
  x = m %>% select(id,starts_with("PCA")) %>% 
    pivot_longer(-id, names_to = "model", values_to = "eigen") 
  p1 = x %>% ggplot(aes(x = id, y = eigen, color = model )) + 
    geom_point() +
    geom_line () +
    ylim(0,6) + 
    theme_minimal()
  x = m %>% select(id,starts_with("LDA")) %>% 
    pivot_longer(-id, names_to = "model", values_to = "eigen") 
  p2 = x %>% ggplot(aes(x = id, y = eigen, color = model )) + 
    geom_point() +
    geom_line () +
    ylim(0,6) +
    theme_minimal()
  p12 = ggarrange(p1, p2, ncol=2, align = "v", legend = "bottom")
  p12 = annotate_figure(p12,top = text_grob("PCA/LDA eigenvalues"))
  print(p12)
                     
}

### CIRCLE PLOT COMPARATIVE FOR PCA LDA ###
if(0){

  p1 = make_var_plot(base$PCA$class$no_norm$var$coord, dim = c(1,2),
                     title = "PCA", sign=c("+","-"), labs=c("PC"))
  p2 = make_var_plot(base$PCA$class$norm$var$coord, dim = c(1,2),
                     title = "PCA Trans", sign=c("+","-"), labs=c("PC"))
  p3 = make_var_plot(base$CDA$no_norm$structure, dim = c(1,2), 
                     title = "LDA", sign=c("+","-"), labs=c("LD"))
  p4 = make_var_plot(base$CDA$norm$structure, dim = c(1,2), 
                     title = "LDA Trans", sign=c("+","-"), labs=c("LD"))
  p1234 = ggarrange(p1,p2,p3,p4, nrow=2,ncol=2, align = "hv")
  print(p1234)
  
}

### BIPLOT LDA ###
if(0){
  
  # [1] "Age"                "Rinput ss"          "Rinput instantly"   "Tau membrane"      
  # [5] "Sag mV"             "Rheobase pA"        "Activity threshold" "Threshold AP"      
  # [9] "Amplitude AP"       "Latency 1o PA s"    "Halfwidth PA"       "fAHP"              
  # [13] "Max rate of Rise"
  
  var.name = "Rheobase pA" # VARIAVEL A DESTACAR O VETOR
  lab.name = var.name
  m = base$CDA$no_norm$scores %>% as_tibble() %>% 
    relocate(Age, .after = last_col())
  p3 = make_ind_plots(ind.coord = m,
                      var.coord = base$CDA$no_norm$structure,
                      df = base$clean1, 
                      dim = c(1,2),
                      var.name = var.name, 
                      lab.name = lab.name, 
                      col = col ,
                      labs = "LD",
                      sign = c("-","-"),
                      title = "LDA no_norm",
                      xlim = c(-6,6),
                      ylim = c(-5,5))
  #print(p1)
  m = base$CDA$norm$scores %>% as_tibble() %>%
    relocate(Age, .after = last_col())
  p4 = make_ind_plots(ind.coord = m,
                      var.coord = base$CDA$norm$structure,
                      df = base$clean1,
                      dim = c(1,2),
                      var.name = var.name,
                      lab.name = lab.name,
                      col = col ,
                      labs = "LD",
                      sign = c("-","-"),
                      title = "LDA norm",
                      xlim = c(-6,6),
                      ylim = c(-5,5))

  p34 = ggarrange(p3, p4, nrow = 2)
  print(p34)
  #p1234 = ggarrange(p12,p34, ncol = 2, common.legend = T)
  #print(p1234)
}




############################ DAQUI PARA BAIXO APENAS TESTES ####################
### REFUGO/TESTES ###

if(0){
  par(mfrow=c(1,1))
  m = base$LDA$no_norm$scaling
  #m1 = t(t(as.matrix(m)) * base$LDA$no_norm$svd)
  make_corr_plots(m, pal = 2, 
                  title = "LDA no_norm")
  # make_corr_plots(m1,  pal = 2,
  #                 title = "LDA norm" )
}

if(0){
  #browser()
  base$LDA = list()
  
  base$LDA$no_norm = lda(formula = Age ~ ., data = as.data.frame(scale(base$clean1)))
  v = base$LDA$no_norm$svd^2/sum(base$LDA$no_norm$svd^2)
  v = base$LDA$no_norm$svd^2
  plot(v, type="b")
  base$LDA$norm = lda(formula = Age ~ ., data = as.data.frame(scale(base$norm1)))
  v = base$LDA$norm$svd^2/sum(base$LDA$norm$svd^2)
  v = base$LDA$norm$svd^2
  lines(v, lty = 3)
}


if(0){
  d = as.data.frame(iris)
  d$Species = as.factor(d$Species)
  x = lda(formula = Species ~ ., data = d)
  w = predict(object = x, newdata = d)
  w$r = w$x %>% as_tibble() %>% mutate(Species = d$Species)
  p = w$r %>% ggplot(aes(x = -LD1, y = LD2, color = Species)) +
    geom_point() +
    ylim(-6,6) + 
    theme_minimal()
  print(p)
  
  y = lm(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, data=d)
  z = candisc(y)
  browser()
  plot(z)
  m = abs(t(t(as.matrix(z$structure)) * sqrt(z$eigenvalues)))
  m = z$structure
  make_corr_plots(m, title = "LDA/CDA no_norm")
  d0 = iris %>% select(-Species)
  k = PCA(d0, ncp = 4, graph = F)
  var.name = "Species"
  #zvar = round(d0[[var.name]],2)
  var = iris[,var.name]
  col.ind = var
  nrow = nrow(d0)
  p = fviz_pca_biplot(k, col.ind = col.ind,
                   geom.ind = c("point"),
                   axes=c(1,2),
                   pointsize = 4,
                   gradient.cols = col(nrow) #,
                   #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
                   #repel = TRUE # Avoid text overlapping (slow if many points)
  )
  p = p + labs(color = var.name)
  print(p)
  
  
}


