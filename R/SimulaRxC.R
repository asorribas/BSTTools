

AssignProb <- function(row) {
  ind <- sample(1:row,row,replace=F)
  prob <- rep(0,row)
  prob[ind[1]] <- runif(1,0,0.3)
  for (i in 2:row-1) {prob[ind[i]] <- runif(1,0,1-sum(prob))}
  prob[ind[row]] <- 1-sum(prob)
  prob %>% round(2)
}


SimulaProbRxC <- function(row=3,col=3) {
  
matrix(replicate(col,AssignProb(row)),ncol=col,byrow = F)
}

SimulaProbRxCEqual <- function(row=3,col=3) {
  p0 <- AssignProb(row)
  p <- function(row) {
    pp <-p0+runif(row,0,0.15)
    pp/(sum(pp))}
  matrix(replicate(col,p(row)),ncol=col,byrow = F)
}

 

SimulaRxC <- function(row=3,col=3,maxn=250,minn=100) {
  if (runif(1)<0.5) {res <- SimulaProbRxC(row,col)}
  else {res <- SimulaProbRxCEqual(row,col)}
   
  colnames(res)<-LETTERS[1:col]
  nn <- c()
  for (i in 1:row)  nn <- c(nn,paste0(rep('+',i),collapse=''))
  rownames(res)<-nn
  gg <- c()
  for (i in 1:col){
    n <- runif(1,minn,maxn) %>% ceiling()
    gg  <- c(gg,res[,i]*n) %>% ceiling()}
    data=matrix(gg,byrow = F,ncol=col)
    colnames(data)<-LETTERS[1:col]
    rownames(data)<-nn
    list(probs=res,data=data)
}    

#SimulaRxC(col=2,row=5)

Simula2x2Cohort <- function(maxn=300,minn=30) {
  row <- 2
  col <- 2
  res <- SimulaProbRxC(row,col)
  colnames(res)<-c('G1','G2')
  rownames(res)<-c('Disease','Healthy')
  gg <- c()
  for (i in 1:col){
    n <- runif(1,minn,maxn) %>% ceiling()
    gg  <- c(gg,res[,i]*n) %>% ceiling()}
  data=matrix(gg,byrow = F,ncol=col)
  colnames(data)<-c('G1','G2')
  rownames(data)<-c('Disease','Healthy')
  list(probs=res,data=data)
}     



