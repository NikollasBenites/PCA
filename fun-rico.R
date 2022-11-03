
make_box_plot = function(ind.coord, clean.coord, var.name, col, xlab = "PC1",
                         dim = c(1),
                         sign = c("-"),
                         interv = c(-4:4,7),
                         title = "Boxplot by PC1"){
  
  #browser()
  ind.coord = as.data.frame(ind.coord)
  if (sign[1] == "-"){
    ind.coord[dim[1]] = -ind.coord[dim[1]]
    interv = c(-7,-4:4)
  }
  
  title = paste0(title," for ",var.name)
  ind.coord = ind.coord %>% as_tibble() %>%
    mutate(Bin = cut(Dim.1,interv, dig.lab = 2)) %>%
    mutate({{var.name}} := clean.coord[[var.name]])
  
  #browser()
  nrow = nrow(ind.coord)
  p = ind.coord %>% ggplot(aes(Bin, !!sym(var.name), color = !!sym(var.name))) +
    geom_boxplot(outlier.shape = NA) +
    #geom_violin(trim = F, draw_quantiles = c(0.5)) +
    geom_jitter(height = 0, width = 0.2, size = 3) +
    ggpubr::gradient_color(col(nrow)) +
    theme_minimal()
  p = p + labs(color = var.name) + xlab(xlab) +
    ggtitle(title) + theme(plot.title = element_text(hjust=0.5,size = 20))
  return(p)
  
}

make_var_plot = function(var.coord,
                         dim = c(1,2),
                         dim.names = colnames(var.coord),
                         var.names = rownames(var.coord),
                         title = "Title...",
                         sign = c("+","+"),
                         labs = NULL){
  
  var.coord = as.data.frame(var.coord)
  
  #browser()
  
  if (sign[1] == "+"){
    var.coord[dim.names[dim[1]]] = -var.coord[dim.names[dim[1]]]
  }
  if (sign[2] == "+"){
    var.coord[dim.names[dim[2]]] = -var.coord[dim.names[dim[2]]]
  } 
  
  dim.name1 = dim.names[dim[1]]
  dim.name2 = dim.names[dim[2]]
  p = var.coord %>% as_tibble() %>% ggplot(aes(x = !!sym(dim.name1), y = !!sym(dim.name2))) +
      #geom_point() +
      geom_segment(aes(x=0,y=0,xend=!!sym(dim.name1),yend=!!sym(dim.name2)),
                   arrow = arrow(length = unit(0.1, "inches"),
                                 ends = "last",
                                 type = "closed")
      ) +
     ylim(-1,1) +
     xlim(-1,1) + 
     ggtitle(title) + 
     geom_text_repel(aes(label = var.names)) +
     theme_minimal()

  if (!is.null(labs)){
    labs = paste0(labs,dim)
    p = p + labs(x = labs[1], y = labs[2])
  }
  
  p = p + theme(plot.title = element_text(hjust=0.5,size = 20))
  return(p)
  
}


make_ind_plots = function(ind.coord, df,
                          var.coord = NULL,
                          dim = c(1,2),
                          var.name, 
                          lab.name,
                          dim.names = colnames(ind.coord),
                          labs = "PC", 
                          sign = c("+","+"),
                          col, title = "title...",
                          xlim, ylim){

  #browser()
  ind.coord = as.data.frame(ind.coord)
  var.coord = as.data.frame(var.coord)
  #sign 1 abaixo muda a orienta??o do gr?fico no plot do eixo x e o sign2 no eixo y
  if (sign[1] == "+"){
    ind.coord[dim.names[dim[1]]] = -ind.coord[dim.names[dim[1]]]
    if(!is.null(var.coord)){
      var.coord[dim.names[dim[1]]] = -var.coord[dim.names[dim[1]]]
    }
  }
  if (sign[2] == "+"){
    ind.coord[dim.names[dim[2]]] = -ind.coord[dim.names[dim[2]]]
    if(!is.null(var.coord)){
      var.coord[dim.names[dim[2]]] = -var.coord[dim.names[dim[2]]]
    }
  } 
  
  dim.name = paste0(labs,dim)
  var = round(df[[var.name]],2)
  var.label = round(df[[lab.name]],2)
  #XL = plda$x[,1:5] %>% as_tibble()
  dim.n1 = dim.names[dim[1]]
  dim.n2 = dim.names[dim[2]]
  nrow = nrow(df)
  p <- ind.coord %>% as_tibble %>% ggplot(aes(!!sym(dim.n1), !!sym(dim.n2), colour = var)) + 
    geom_point(size = 2.5) + #modifica o tamanho do s?mbolo (c?rculo) no graph
    xlim(xlim[1],xlim[2]) + #orienta??o do gr?fico dentro do plot no eixo X
    ylim(ylim[1],ylim[2]) +
    ggpubr::gradient_color(col(nrow)) +
    ggtitle(title) + 
    theme_test()#modifica o estilo do gr?fico
  p = p + labs(color = var.name)#, size = "Age")
  p = p + geom_text_repel(aes(label=var.label, color=var)) + #, color = var)) +
    ggpubr::gradient_color(col(nrow))
  if (!is.null(labs)){
    labs = paste0(labs, dim)
    p = p + labs(x = labs[1], y = labs[2])
  }
  #browser()
  if (!is.null(var.coord)){
    var.max = max(abs(ind.coord[,1]))
    var.coord = var.coord * var.max
    p = p + geom_segment(data=var.coord, 
                         mapping=aes(x=0,y=0,xend=!!sym(dim.n1),yend=!!sym(dim.n2)),
                         color = "black",
                         alpha = 0.1,
                         arrow = arrow(length = unit(0.1, "inches"),
                                       ends = "last",
                                       type = "closed")
    )
    var.name = gsub(" ","_", var.name)
    #var.coord = var.coord %>% rename_with(~ gsub(" ","_", .))
    var.rownames = rownames(var.coord)
    var.rownames = gsub(" ","_", var.rownames)
    rownames(var.coord) = var.rownames
    var.coord = var.coord[gsub(" ","_", var.name),] %>% as_tibble()
    p = p + geom_segment(data=var.coord, 
                         mapping=aes(x=0,y=0,xend=!!sym(dim.n1),yend=!!sym(dim.n2)),
                         color = "red",
                         size = 1,
                         arrow = arrow(length = unit(0.1, "inches"),
                                       ends = "last",
                                       type = "closed")
    )
    # p = p + geom_text_repel(data=var.coord, mapping = aes(label = var.name),
    #                         color = "red")
  }
  p = p + theme(plot.title = element_text(hjust=0.5,size = 20))
  return(p)
  #print(p1)
  #p3 = p1
}


make_corr_plots = function(m, pal=1, title = ""){
  if (pal==1) col = colorRampPalette(c('white', 'yellow','red'))
  if (pal==2) col = colorRampPalette(c('darkred','white', 'darkblue'))
  if (pal==3) col = colorRampPalette(c('darkred','yellow', 'darkblue'))
  corrplot(m, is.corr = FALSE, method = "square", col = col(10), mar = c(4,0.1,2,0),
           cl.offset = 0.2, addgrid.col = NULL,
           tl.col = 'black',tl.cex = 1.2,
           title=title)
}

make_CDA = function(base){
  
  #browser()
  b = base %>% rename_with(~ gsub(" ","_", .))
  #b$Age = round(b$Age,3)
  b = scale(b) %>% as_tibble()
  b$Age = round(b$Age,3)
  b$Age = as.factor(b$Age)
  x = lm(cbind(Rinput_ss, Rinput_instantly, Tau_membrane,
               Sag_mV, Rheobase_pA, Activity_threshold, Threshold_AP,
               Amplitude_AP, Latency_1o_PA_s, Halfwidth_PA, fAHP,
               Max_rate_of_Rise) ~ Age, data = as.data.frame(b))
  y = candisc(x)
  return(y)
}

make_LDA = function(base){
  
  b = scale(base) %>% as_tibble()
  b$Age = round(b$Age,3)
  b$Age = as.factor(b$Age)
  x = lda(formula = Age ~ ., data = as.data.frame(b))
  return(x)
}

make_wallys = function(m, Age, title = "Title..."){
  
  set.seed(123)
  m = m %>% as_tibble() %>% mutate(Age = Age)
  m = m %>% mutate(Wally = case_when(
    Dim.1 < 0 ~ 1,
    Dim.2 < 0 ~ 2,
    TRUE ~ 3
  ))
  
  vt = m %>% select(Age, Wally) %>% group_by(Wally) %>%
    nest() %>% spread(key = Wally, value = data)
  
  vt = vt %>% mutate(
    t_test1_2 = map2(`1`, `2`, ~{t.test(.x$Age, .y$Age) %>% tidy()}),
    t_test1_3 = map2(`1`, `3`, ~{t.test(.x$Age, .y$Age) %>% tidy()}),
    t_test2_3 = map2(`2`, `3`, ~{t.test(.x$Age, .y$Age) %>% tidy()})
  )
  #browser()
  t23 = round(vt$t_test2_3[[1]]$p.value,3)
  
  p = m %>% ggplot(aes(x = as.factor(Wally), y = Age, color = as.factor(Wally))) +
    geom_violin(trim = F, draw_quantiles = c(0.5)) + 
    geom_jitter(height = 0, width = 0.2) +
    labs(color = "Wally", x = "Wally") +
    ggtitle(title) + 
    theme_minimal()
  p = p + annotate("text", x = 2.6, y = 1.5, 
                   label = paste0("t.test"), fontface = 2)
  p = p + annotate("text", x = 2.6, y = 0, 
                   label = paste0("p.value: ",t23))
  p = p + theme(plot.title = element_text(hjust=0.5,size = 20))
  return(p)
}


#################################
# Data input and transformations
#################################
data_input = function(filename){
  #browser()
  df0 = read_delim(filename,
                   skip=1,
                   delim=",",  
                   name_repair = "universal"#,
                   #locale = locale(decimal_mark = ","),
                   #col_types = cols(.default = col_double())
  )
  df0 = df0[!map_lgl(df0, ~ all(is.na(.)))] #elimina coluna toda na
  df0 = df0 %>% rename_with(~ gsub("\\."," ", .))
  df0 = df0 %>% rename_with(~ gsub("   ","_", .)) #`Fenotype   1`
  df0 = df0 %>% rename_with(~ gsub("_ ","_", .))
  df0 = df0 %>% rename_with(~ gsub("  "," ", .))
  #browser()
  df1 = df0 %>% select(Fenotype_1:'Max rate of Rise_14')
  df1 = df1 %>% rename_with(~ gsub("_.*$","", .))
  # df1 = df1 %>% filter(rowSums(across(everything(), ~ is.na(.))) < ncol(.))
  df1 = df1 %>% mutate(Fenotype = replace_na(Fenotype, "Center"))
  df1 = df1 %>% mutate(Age = replace_na(Age, mean(Age,na.rm=T)))
  df1$p12 = "Pre"
  #df2 = df0[,15:28]
  df2 = df0 %>% select(Fenotype_16:'Max rate of Rise_29')
  df2 = df2 %>% rename_with(~ gsub("_.*$","", .))
  
  # df2 = df2 %>% filter(rowSums(across(everything(), ~ is.na(.))) < ncol(.))
  df2$p12 = "Post"
  df2 = df2 %>% mutate(Fenotype = replace_na(Fenotype, "Center"))
  df2 = df2 %>% mutate(Age = replace_na(Age, mean(Age,na.rm=T)))
  df2 = df2 %>% drop_na()
  df = bind_rows(df1,df2)
  #df = df %>% mutate(across(everything(), ~ replace_na(.x, 0)))
  #browser()
  df = df %>% mutate(id = row_number())
  df = df %>% relocate(id)
}



# Define function to colour panels according to correlation
# From: https://stackoverflow.com/questions/68093071/how-to-highlight-high-correlations-in-ggpairs-correlation-matrix

cor_func <- function(data, mapping, method, symbol, ...){
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor(x, y, method=method, use='complete.obs')
  
  colFn <- colorRampPalette(c("firebrick", "white", "dodgerblue"), 
                            interpolate ='spline')
  rampcols <- colFn(100)
  match <- c(rampcols[1:40], rep("#FFFFFF", 20), rampcols[60:100])
  fill <- match[findInterval(corr, seq(-1, 1, length = 100))]
  
  ggally_text(
    label = paste(symbol, as.character(round(corr, 2))), 
    mapping = aes(),
    xP = 0.5, yP = 0.5,
    color = 'black',
    ...) + 
    theme_void() +
    theme(panel.background = element_rect(fill = fill))
}



norm_all = function(v, n, k = 1.05, plot = F){
  
  
  v = scale(v)
  v.min = k * min(v)
  v.pos = v - v.min
  bc = boxcoxfit(v.pos)
  if (round(bc$lambda,1) != 0){
    v.bc = (BCtransform(v.pos, bc$lambda))$data
  } else {
    v.bc = log(v.pos)
  }
  v.bc = scale(v.bc)
  if (plot){
    #print(bc$lambda)
    plot(density(v), xlim = c(-8,8), ylim = c(0,1), main = n)
    mtext(paste0("lambda = ", round(bc$lambda,3)), side = 3, line = 0.5)
    lines(density(v.pos), lty=2)
    lines(density(v.bc), lty=3)
  }
  #browser()
  return(as.vector(v.bc))
  
}
