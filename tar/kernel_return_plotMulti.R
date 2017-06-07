library(qgraph)
library(Matrix)
tickerLabels=read.table(file="~/Desktop/tar/tickers",header=FALSE,sep = ",", quote = "", dec = ".", fill = TRUE, comment.char = "")
tickerLabels=as.vector(tickerLabels)
tickerLabels = t(tickerLabels)

A = c()
B = c()

C = matrix(, nrow = 1786, ncol = 223)
D = matrix(, nrow = 1786, ncol = 223)

for (rowPivot in (1:1786)){
  inputFileName = paste("~/Desktop/pass/201304_kernel_return_continuous_matrix/201304_", as.character(rowPivot), ".txt", sep="")
  ret=read.table(file=inputFileName,header=FALSE,sep = " ", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
  print(inputFileName)
  square = as.matrix(ret) %*% t(ret)
  
  
  eig = eigen(square)
  maxVal = max(eig$values)
  A = c(A, maxVal)
  
  
  
  ###plot max eigenvalue percentage at the certain time
  outputFileName = paste("~/Desktop/newrst/201304_kernel_return_plot_EigenvalPercent/201304_kernel_return_EigenvalPercent_", as.character(rowPivot), ".jpg", sep="")
  graphName = paste("201304_EigenvalPercent_", as.character(rowPivot),sep="")
  jpeg(outputFileName, width=1500, height=1500)
  
  B = eig$values
  numcreat = seq(1, length(B),by = 1)
  
  for (i in (1:223)){
    if (B[i] < 0){
      B[i] = 0
    }
  }
  
  if (sum(B) == 0){
    percent = 0}
  else{percent = B/sum(B)}
  
  
  par(mar=c(14,16,12,6))
  par(las=2)
  plot(percent,xlab="",ylab="",xaxt="n", lwd = 4, cex.axis=3, col="green") 
  
  axis(1, at=1:223, labels=tickerLabels,cex.axis=0.4)
  title(ylab = "EigenvaluePercent", cex.lab = 4,line = 8)
  title(xlab = "Tickers", cex.lab = 4,line = 5)
  title(graphName, line = 3, cex.main=5)
  dev.off()
  
  ###plot eigenvalue at the certain time
  options( scipen = 0 )###scientific notation
  options( digits = 6 )
  
  outputFileName = paste("~/Desktop/newrst/201304_kernel_return_plot_Eigenval/201304_kernel_return_Eigenval_", as.character(rowPivot), ".jpg", sep="")
  graphName = paste("201304_Eigenval_", as.character(rowPivot),sep="")
  jpeg(outputFileName, width=1500, height=1500)
  
 
  par(mar=c(14,16,12,6))
  par(las=2)
  plot(B,xlab="",ylab="",xaxt="n", lwd = 4, cex.axis=3, col="blue") 
  
  axis(1, at=1:223, labels=tickerLabels,cex.axis=0.4)
  title(ylab = "Eigenvalue", cex.lab = 4,line = 12)
  title(xlab = "Tickers", cex.lab = 4,line = 5)
  title(graphName, line = 3, cex.main=5)
  dev.off()
  
  
  
  
  C[rowPivot,] = B
  
  ###plot max eigenvector at the certain time
  outputFileName = paste("~/Desktop/newrst/201304_kernel_return_plot_maxEigenvec/201304_kernel_return_maxEigenvec_", as.character(rowPivot), ".jpg", sep="")
  graphName = paste("201304_maxEigenvec_", as.character(rowPivot),sep="")
  jpeg(outputFileName, width=1500, height=1500)

  max_eigenvec = eig$vectors[,which.max(eig$values)]
  
  par(mar=c(14,16,12,6))
  par(las=2)##rotate lable
  plot(max_eigenvec,xlab="",ylab="", xaxt="n", lwd = 4, cex.axis=3, col="blue")
  axis(1, at=1:223, labels=tickerLabels,cex.axis=0.4)#cex:lable size  ##run k-rv-04 change to "rows"
  title(ylab = "Max_Eigenvector", cex.lab = 4,line = 8)
  title(xlab = "Tickers", cex.lab = 4,line = 5)
  title(graphName, line = 3, cex.main=5)
  dev.off()
  
  D[rowPivot,] = max_eigenvec
  
}


###plot max eigenvalue at different times###########

options( scipen = 0 )###scientific notation
options( digits = 2 )


write.table(A, "201304_ARcoeff_Return_MaxEigenval_Difftime.csv", col.names = FALSE)
jpeg("~/Desktop/newrst/201304_kernel_return_maxEigenval_Difftime",width=1000, height=1000)
numcreat = seq(1, length(A),by = 1)
par(mar=c(8,12,8,6))
plot(numcreat, A, xlab="", ylab="",type="l",lty=1,lwd = 5,cex.axis=2)

title(ylab = "MaxEigenvalue", cex.lab = 3,line = 5)
title(xlab = "Date", cex.lab = 3,line = 5)
title("201304_ARcoeff_Return_MaxEigenval_Difftime", line = 3, cex.main=3)
dev.off()



###plot maxEigenvalue quantile at different times
q5 = seq(1,1786,by = 1)
q50 = seq(1,1786,by = 1)
q95 = seq(1,1786,by = 1)
for (i in (1:1786)){
  q5[i] = sort(C[i,])[floor(223 * 0.95)]
  q50[i] = sort(C[i,])[floor(223 * 0.5)]
  q95[i] = sort(C[i,])[floor(223 * 0.05)]
}


jpeg("~/Desktop/newrst/201304_kernel_return_maxEigenval_quantile_Difftime",width=1000, height=1000)
par(mar=c(8,12,8,6))
plot(numcreat, q5, xlab="", ylab="", type="l",lty=1, lwd = 5,cex.axis=2,col="blue")
lines(numcreat, q50, col="red")
lines(numcreat, q95,col="green")
legend('topright', legend=c("Quantile 5%", "Quantile 50%", "Quantile 95%"),col=c("green","red", "blue"), lty=1, cex=1.5)
title(ylab = "MaxEigenval_Quantile_Difftime", cex.lab = 3,line = 5)
title(xlab = "Date", cex.lab = 3,line = 5)
title("201304_ARcoeff_Return_MaxEigenval_Quantile_Difftime", line = 3, cex.main=2.5)
dev.off()

###plot maxEigenvec quantile at different times

p5 = seq(1,1786,by = 1)
p50 = seq(1,1786,by = 1)
p95 = seq(1,1786,by = 1)

for (i in (1:1786)){
  #D[,i][is.na(D[,i])] <- 0  there are "NA" inside, so transfer NA to zero
  p5[i] = sort(D[i,])[floor(223 * 0.95)]
  p50[i] = sort(D[i,])[floor(223 * 0.5)]
  p95[i] = sort(D[i,])[floor(223 * 0.05)]
}

jpeg("~/Desktop/newrst/201304_kernel_return_maxEigenvec_quantile_Difftime",width=1000, height=1000)
par(mar=c(8,12,8,6))
plot(x=numcreat, y=p5, xlab="", ylab="", type="l",lty=1, lwd = 3,cex.axis=2,ylim=c(-0.2, 0.2), col="blue")#####need to decide the limc()
lines(numcreat, p50,col="red")
lines(numcreat, p95,col="green")
legend('topright', legend=c("Quantile 5%", "Quantile 50%", "Quantile 95%"),col=c("green","red", "blue"), lty=1, cex=1)
title(ylab = "MaxEigenvec_Quantile_Difftime", cex.lab = 3,line = 5)
title(xlab = "Date", cex.lab = 3,line = 5)
title("201304_ARcoeff_Return_MaxEigenvec_Quantile_Difftime", line = 3, cex.main=2.5)
dev.off()




