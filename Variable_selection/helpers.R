library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(gtable)
library(InformationValue)

# Restore the object
work_data <- readRDS(file = "work_data.rds")

### Create loop for 'Bad Rate' (add columns with BR for every variable)

# first column to calc 'Bad Rate'
frst_var <- 2
target_calc <- match("target_for_calc",names(work_data))
col<-ncol(work_data)

br_func <- function(work_data){
  for (i in frst_var:col){
    
    #create 'br_table'. It consists of 2 column("BR" + name_of_variables, BR_value)
    var_for_group <- names(work_data)[i]
    column_br <- paste("BR", 
                       names(work_data)[i]
                       , sep="_")
    
    br_table <- work_data %>%
      select(c(i,target_for_calc)) %>%
      group_by_(.dots = var_for_group) %>%
      summarise_all(funs(!!column_br := (n() - sum(.))/n()))
    
    # join 'br_table' to the table with bining variables
    work_data <- left_join(work_data, br_table,by=names(work_data)[i])
  }
  work_data  
}


### IV - statistic table
iv_func <- function(work_data){
  iv_table <- arrange(cbind.data.frame(variables = names(work_data[frst_var:col])
                                       ,IV = sapply(work_data[frst_var:col], function(x) round(IV(X=x, Y=work_data$target_for_calc)[1],4)), row.names = NULL),
                      desc(IV))
  
  
  # Add strength of variables
  iv_table$Strength <- ifelse(iv_table$IV>=1, "Suspicious",
                              ifelse(iv_table$IV>=.5, "Very strong",
                                     ifelse(iv_table$IV>=.2, "Strong",
                                            ifelse(iv_table$IV>=.1, "Average",
                                                   ifelse(iv_table$IV>=.02, "Weak", "Wery weak")))))
  iv_table
}

### Create statistical plot and table for every variable
plot_func <- function(var_name, iv_table, work_data){
  
  Total<-length(work_data$target_for_calc)
  Good<-sum(work_data$target_for_calc)
  Bad<-Total-Good
  j <- match(var_name, names(work_data))
  
  plot1_hist <- ggplot(work_data, aes(work_data[,j])) + 
    geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
    scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
    geom_text(aes( y = ((..count..)/sum(..count..)),
                   label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust =-.1)+
    theme(axis.text.x = element_text(angle=10, vjust=0.9) ) + 
    labs( y = "Class", x = "")
  
  # plot of 'Bad Rate'
  plot2_BR_line <- ggplot(work_data, aes(x=work_data[,j],y=work_data[,j-frst_var+col+1],group=1)) + 
    geom_line(color="indianred3",size=1)+
    geom_point(color="indianred3") +
    theme(axis.text.x = element_text(angle=10, vjust=1)) + 
    scale_y_continuous(limits=c(0, 1),breaks=c(.1, .3, .5, .7, .9), 
                       labels = function(x) paste0(x*100, "%"))+
    labs( y = "BR", x = "")    
  
  
  # union 2 graphics(plot1_hist, plot2_BR_line) in 1 
  # extract gtable
  g1 <- ggplot_gtable(ggplot_build(plot1_hist))
  g2 <- ggplot_gtable(ggplot_build(plot2_BR_line))
  
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                       pp$l, pp$b, pp$l)
  
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  #log(x) will produce NaN any time x is less than zero(calculating 'length(x)-sum(x)' we have '-' func 'log' see that and returns error
  options(warn = -1) 
  
  # calc statistic values for every column
  aggregate_table<-aggregate(. ~ work_data[,j], data = work_data[c(names(work_data)[target_calc],names(work_data)[j])],
                             FUN = function(x) c(good = sum(x),
                                                 bad=length(x)-sum(x),
                                                 total = length(x),
                                                 good2=  round((sum(x)*100)/Good,2),
                                                 bad2=round((length(x)-sum(x))*100/Bad,2),
                                                 total2=round((length(x)*100)/Total,2),
                                                 BR=round((length(x)-sum(x))*100/length(x),2),
                                                 WOE=round(log((sum(x)/Good)/((length(x)-sum(x))/Bad)),4)))[,c(1,2)]
  
  aggregate_table<-cbind(aggregate_table[,1],data.frame(aggregate_table[,2]))
  names(aggregate_table)<-c(names(work_data)[j],"good, #","bad, #","total, #","good, %","bad, %","total, %","BR, %","WOE")
  
  # chisq.test
  var_for_group <- names(work_data)[j]
  chisq_table <-  work_data %>%
    select(c(j,target_calc)) %>%
    group_by_(.dots = var_for_group) %>%
    summarise_all(funs(good = sum(.),
                       bad = (n() - sum(.)))) %>%
    select(-1) %>% 
    t() 
  
  # set chisq.test value and p_value
  chisq <- round(as.data.frame(chisq.test(chisq_table)[1])[1,1], 2)
  p_value_chisq <- round(as.data.frame(chisq.test(chisq_table)[3])[1,1], 4)
  
  ## Gathering all statistic to one sheet
  
  # value from 'aggregate_table' sets in the ggplot object
  table <- ggtexttable(aggregate_table, rows = NULL, theme = ttheme(base_style ="lRedWhite", base_size = 8))
  
  # set name of variable and her 'Strength'(dependense of IV: 'Strong', Weak, 'Very weak' and etc)
  text1 <- paste0("
                  ",names(work_data)[j],": ", iv_table$Strength[iv_table$variables == names(work_data)[j]], " 
                  ")
  # set style of 'text1'
  title1 <- ggparagraph(text = text1, face = "italic", size = 17,color = "black")
  
  
  text2 <- paste0("                  ","IV ="
                  ,round(iv_table$IV[iv_table$variables == names(work_data)[j]],4), sep = " ")
  title2 <- ggparagraph(text = text2, face = "italic", size = 14, color = "black")
  
  
  text3 <- paste0("                  ","Chisq.test = "
                  ,chisq, "; p_value = ", p_value_chisq, sep = "  ")
  title3 <- ggparagraph(text = text3, face = "italic", size = 14, color = "black")
  
  # union 4 object in one file: 
  print(ggarrange(title1, title2, title3, g, table,
                  ncol = 1, nrow = 5,heights = c(.2, .08, .08, .6, .7)))
}


### Data preparating 
set.seed(123)
work_data_dev <- work_data
work_data_dev1 <- work_data[seq(round(nrow(work_data)/2, 0)),] 
work_data_dev2 <- work_data[-seq(round(nrow(work_data)/2, 0)),]
# use test set
work_data_test <- work_data[sample(nrow(work_data), round(nrow(work_data)/2, 0)),]

# add 'Bad rate' to the data
work_data_dev <- br_func(work_data_dev)
work_data_dev1 <- br_func(work_data_dev1)
work_data_dev2 <- br_func(work_data_dev2)
work_data_test <- br_func(work_data_test)



### Final function
var_stability <- function(var_name, dataset){
      if(dataset ==1){
        work_data <- work_data_dev
      } else if (dataset ==2){
        work_data <- work_data_dev1
      } else if (dataset ==3){
        work_data <- work_data_dev2  
      } else {
        work_data <- work_data_test
      }
  
  plot_func(var_name, iv_func(work_data), work_data)
  
}














