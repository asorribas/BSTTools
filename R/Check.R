
#### example predimed
#data(predimed)
#predimed %>% head()
#predimed %>% with(TableCI(sex,group))
#predimed %>% with(PlotPropCI(sex,group))
#predimed %>% with(AdjResTable(sex,group))
#predimed %>% with(PlotProp(sex,group))
#predimed %>% with(PlotProp(sex,group,
#                           stacked = T))
#predimed %>% with(PlotProp(sex,group,
#                           stacked = T))+coord_flip()
#predimed %>% with(PlotProp(sex,group,
#                           stacked = F,
#                           legend = 'Sex',
#                           xlab = 'Group',
#                           ylab='Percentage'))
#predimed %>% with(PlotProp(diab,group,
#                           stacked = F,
#                           legend = 'Diabetic',
#                           xlab = 'Group',
#                           ylab='Percentage'))

#predimed %>% head()
#predimed %>% with(PlotMeans(age,sex,data=predimed))
#predimed %>% with(PlotMeansGroups(age,diab,sex,data=predimed))
#predimed %>% with(PlotMeansGroups(age,diab,sex,data=predimed))+
#  ylim(60,70)+
#  ylab('95% CI for population means')+
#  xlab('Diabetes')+
#  ggtitle('Age means')


############ Other examples
