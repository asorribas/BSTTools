

#df <- birthwt %>% with(table(race,low))
#df 
#TablePropCI(df)


TablePropCI <- function(df,method='sisonglaz'){
  res <- c()
  names <- colnames(df)
  nomscols <- c()
  for (i in 1:dim(df)[2])
    nomscols<-c(nomscols,rep(names[i],dim(df)[1]))
  names <- rownames(df)
  nomsrows<-rep(names,dim(df)[2])
  for (i in 1:dim(df)[2])
    res <- rbind(res,MultinomCI(df[,i],method=method))
  dd <- data.frame(Cols=nomscols,Rows=nomsrows,res %>% round(3))
  row.names(dd) <- NULL
  
  dd   
}

TableCI <- function(x,y,method='sisonglaz'){
  if (class(x)!='factor') return(paste('Sorry, the first variable is not a factor. This function requires two factor variables'))
  if (class(y)!='factor') return(paste('Sorry, the second variable is not a factor. This function requires two factor variables'))

  res <- c()
  df <- table(x,y)
  names <- colnames(df)
  nomscols <- c()
  for (i in 1:dim(df)[2])
    nomscols<-c(nomscols,rep(names[i],dim(df)[1]))
  names <- rownames(df)
  nomsrows<-rep(names,dim(df)[2])
  for (i in 1:dim(df)[2])
    res <- rbind(res,MultinomCI(df[,i],method=method))
  dd <- data.frame(Cols=nomscols,Rows=nomsrows,res %>% round(3))
  row.names(dd) <- NULL
  dd
}

PlotPropTableCI <- function(df,xlab='',ylab='',legend='',method='sisonglaz') {
  res <- TablePropCI(df,method)

  ggplot(res,aes(x=Cols,fill=Rows,est))+

    geom_bar(stat='identity',
             position=position_dodge())+

    geom_errorbar(aes(ymin=lwr.ci,ymax=upr.ci),position=position_dodge(0.9),
                  size=1, color='black',width=.1)+
    geom_point(aes(x=Cols,fill=Rows),
               colour='red',size=3,position=position_dodge(0.9)
    )+
    scale_fill_manual(values=c("grey", "orange","brown","red"))+
    xlab(xlab)+ylab(ylab)+
    labs(fill =legend)+
    ylim(0,1)
}

PlotPropCI <- function(x,y,xlab='',ylab='',legend='',method='sisonglaz') {
  if (class(x)!='factor') return(paste('Sorry, the first variable is not a factor. This function requires two factor variables'))
  if (class(y)!='factor') return(paste('Sorry, the second variable is not a factor. This function requires two factor variables'))

  df <- table(x,y)
  res <- TablePropCI(df,method)

  ggplot(res,aes(x=Cols,fill=Rows,est))+

    geom_bar(stat='identity',
             position=position_dodge(),color='black')+

    geom_errorbar(aes(ymin=lwr.ci,ymax=upr.ci),position=position_dodge(0.9),
                  size=1, color='black',width=.1)+
    geom_point(aes(x=Cols,fill=Rows),
               colour='red',size=3,position=position_dodge(0.9)
    )+
    scale_fill_manual(values=c("grey", "orange","brown","red"))+
    xlab(xlab)+ylab(ylab)+
    labs(fill =legend)+
    ylim(0,1)
}

PlotProp <- function(x,y,xlab='',ylab='',legend='',main='',method='sisonglaz',
                     stacked=F,
                     percent=T,
                     colors=c("grey", "orange","lightblue","pink")) {
  if (class(x)!='factor') return(paste('Sorry, the first variable is not a factor. This function requires two factor variables'))
  if (class(y)!='factor') return(paste('Sorry, the second variable is not a factor. This function requires two factor variables'))

  df <- table(x,y)
  res <- TablePropCI(df,method)

  if (percent & stacked) {p <-geom_text(aes(label=paste(round(est*100,2),'%')),
                                        stat='identity',position=position_fill(vjust=0.5)) }
  else if (percent & stacked==F) {p <-geom_text(aes(label=paste(est*100,'%')), position=position_dodge(width=0.9), vjust=-0.25) }
  else {p <- NULL}

  if (stacked) {
    ggplot(res,aes(x=Cols,fill=Rows,est))+

      geom_bar(stat='identity',color='black')+
      scale_fill_manual(values=colors)+
      xlab(xlab)+ylab(ylab)+
      labs(fill =legend)+
      ylim(0,1)+
      ggtitle(main)+
      p
  }
  else {
    ggplot(res,aes(x=Cols,fill=Rows,est))+

      geom_bar(stat='identity',
               position=position_dodge(),
               color='black')+
      scale_fill_manual(values=colors)+
      xlab(xlab)+ylab(ylab)+
      labs(fill =legend)+
      ylim(0,1)+
      ggtitle(main)+
      p}
}

 

PlotPropTable <- function(df,xlab='',ylab='',legend='',main='',method='sisonglaz',
                     stacked=F,
                     percent=T,
                     colors=c("grey", "orange","lightblue","pink")) {
  #if (class(x)!='factor') return(paste('Sorry, the first variable is not a factor. This function requires two factor variables'))
  #if (class(y)!='factor') return(paste('Sorry, the second variable is not a factor. This function requires two factor variables'))
  
    res <- TablePropCI(df,method)
   
  if (percent & stacked) {p <-geom_text(aes(label=paste(round(est*100,2),'%')),
                                        stat='identity',position=position_fill(vjust=0.5)) }
  else if (percent & stacked==F) {p <-geom_text(aes(label=paste(est*100,'%')), position=position_dodge(width=0.9), vjust=-0.25) }
  else {p <- NULL}
  
  if (stacked) {
    ggplot(res,aes(x=Cols,fill=Rows,est))+
      
      geom_bar(stat='identity',color='black')+
      scale_fill_manual(values=colors)+
      xlab(xlab)+ylab(ylab)+
      labs(fill =legend)+
      ylim(0,1)+
      ggtitle(main)+
      p
  }
  else {
    ggplot(res,aes(x=Cols,fill=Rows,est))+
      
      geom_bar(stat='identity',
               position=position_dodge(),
               color='black')+
      scale_fill_manual(values=colors)+
      xlab(xlab)+ylab(ylab)+
      labs(fill =legend)+
      ylim(0,1)+
      ggtitle(main)+
      p}
}

TableAdjRes <- function(x,y,caption='',full_width=T,count=T){
  if (class(x)!='factor') return(paste('Sorry, the first variable is not a factor. This function requires two factor variables'))
  if (class(y)!='factor') return(paste('Sorry, the second variable is not a factor. This function requires two factor variables'))

  tab <-table(x,y)
  print(tab)
  name <- levels(x)
  p1 <- tab %>%  chisq.residuals(std=T)
  p2 <- tab %>%  prop.table(2)*100
  p2 <- round(p2,2)

  if (count) {res <- c()
  for (i in 1:dim(p1)[1]) {
    r0 <- c(name[i],'n',tab[i,])
    r1 <- c('','%',p2[i,])
    r2 <- c('','Adj.res',p1[i,])
    res <- rbind(res,r0,r1,r2)
    rownames(res) <- NULL
  }}
  else {res <- c()
  for (i in 1:dim(p1)[1]) {
    r1 <- c(name[i],'%',p2[i,])
    r2 <- c('','Adj.res',p1[i,])
    res <- rbind(res,r1,r2)
    rownames(res) <- NULL
  }}

  res  %>% kbl(caption=caption)%>%
    kable_styling(full_width = full_width, html_font = "Cambria")

}



TableAdjResTable <- function(df,caption='',full_width=T,count=T){
  
  tab <-df
  
  #name <- levels(x)
  name <- rownames(tab)
  p1 <- tab %>%  chisq.residuals(std=T)
  p2 <- tab %>%  prop.table(2)*100
  p2 <- round(p2,2)
  
  if (count) {res <- c()
  for (i in 1:dim(p1)[1]) {
    r0 <- c(name[i],'n',tab[i,])
    r1 <- c('','%',p2[i,])
    r2 <- c('','Adj.res',p1[i,])
    res <- rbind(res,r0,r1,r2)
    rownames(res) <- NULL
  }}
  else {res <- c()
  for (i in 1:dim(p1)[1]) {
    r1 <- c(name[i],'%',p2[i,])
    r2 <- c('','Adj.res',p1[i,])
    res <- rbind(res,r1,r2)
    rownames(res) <- NULL
  }}
  
  res  %>% kbl(caption=caption)%>%
    kable_styling(full_width = full_width, html_font = "Cambria")
  
}

TableRR <- function(x,y) {
  df <- table(y,x)
  res <- riskratio(df)$measure %>% round(2)
  res  %>% kbl(caption='Risk ratio')%>%
    kable_styling(full_width = F, html_font = "Cambria")
}

TableOR <- function(x,y) {
  df <- table(y,x)
  res <- oddsratio(df)$measure %>% round(2)
  res  %>% kbl(caption='Odds ratio')%>%
    kable_styling(full_width = F, html_font = "Cambria")
}





