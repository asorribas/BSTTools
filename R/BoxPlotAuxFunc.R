
############ Tools for boxplot

BoxPlot <- function(x,g,data,notch=F,outlier.colour='red',
                    outlier.size=2) {
  xx <- enquo(x)
  gg <- enquo(g)
  df <- data %>% group_by(!!gg) %>% summarise(m=mean(!!xx))
  ggplot(data,aes(x=!!gg,y=!!xx,fill=!!gg))+
    geom_boxplot(outlier.colour = outlier.colour ,
                 outlier.size = outlier.size,
                 notch=notch)+
    geom_point(data=df,aes(x=!!gg,y=m),position=position_dodge(0.9),
               size=3,shape=23,fill='black')
}

#data(predimed)
#predimed %>% head()
#BoxPlot(age,group,predimed)
#BoxPlot(age,group,predimed,notch=T)

BoxPlotGroups <- function(x,g,f,data,notch=F,outlier.colour='red',
                          outlier.size=2) {
  xx <- enquo(x)
  gg <- enquo(g)
  ff <- enquo(f)
  df <- data %>% group_by(!!ff,!!gg) %>% summarise(m=mean(!!xx))

  ggplot(data,aes(x=!!gg,y=!!xx,fill=!!ff))+
    geom_boxplot(position = position_dodge(0.9),
                 outlier.colour = outlier.colour ,
                 outlier.size = outlier.size,
                 notch=notch)+
    geom_point(data=df,aes(x=!!gg,y=m,group=!!ff),position=position_dodge(0.9),
               size=3,shape=21,fill='black')
}



#BoxPlotGroups(age,sex,group,predimed,notch = T)
#BoxPlotGroups(age,sex,group,predimed,outlier.colour = 'blue',outlier.size = 2)
#BoxPlotGroups(age,sex,group,predimed,outlier.colour = 'blue',outlier.size = 2,notch=T)

#predimed %>% head()
#predimed %>% filter(diab=='No') %>% BoxPlot(bmi,group,data=.,notch=T)
#predimed %>% BoxPlotGroups(bmi,group,diab,data=.,notch=T)


