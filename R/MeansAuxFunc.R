

############################ Dealing with means

PlotMeans <- function(x,y,data) {
  group <- enquo(y)
  xvar <- enquo(x)

  res <- data %>% group_by(!!group) %>% summarise(m=mean(!!xvar),
                                                  low=t.test(!!xvar)$conf.int[1],
                                                  upper=t.test(!!xvar)$conf.int[2])


  ggplot(res,aes(x=!!group,y=m))+
    geom_errorbar(aes(ymax=upper,ymin=low), size=1,width=.3,color='blue')+
    geom_point(size=4,shape=21,color='black',fill='white')
}

PlotMeansGroups <- function(x,y,g,data) {
  group <- enquo(y)
  xvar <- enquo(x)
  fg <- enquo(g)
  res <- data %>% group_by(!!group,!!fg) %>% summarise(m=mean(!!xvar),
                                                       lower=t.test(!!xvar)$conf.int[1],
                                                       upper=t.test(!!xvar)$conf.int[2])
  ggplot(res,aes(x=!!group,y=m,group=!!fg))+
    geom_errorbar(aes(ymax=upper,ymin=lower,color=!!fg), size=1,width=.3,
                  position=position_dodge(0.9))+
    geom_point(size=4,shape=21,color='black',fill='white',position=position_dodge(0.9))
}


TableMeansCI <- function(x,y,data) {
  group <- enquo(y)
  xvar <- enquo(x)

  res <- data %>% group_by(!!group) %>% summarise(mean=mean(!!xvar)%>% round(2),
                                                  low=t.test(!!xvar)$conf.int[1] %>% round(2),
                                                  upper=t.test(!!xvar)$conf.int[2]%>% round(2))


  res  %>% kbl(caption='95% CI for means')%>%
    kable_styling(full_width = F, html_font = "Cambria")
}

