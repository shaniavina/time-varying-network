

A = c()
B = c()
for (i in 1:279) {
  #for (j in 09:09) {
    #m = i * 100 + j
    #filename = paste("/Users/shanshanli/Desktop/Kernel_Matrix/" , as.character(m), ".txt", sep="")
    filename = paste("/Users/shanshanli/Desktop/Kernel_Matrix/201309_" , as.character(i), ".txt", sep="")
    x=read.table(file=filename,header=FALSE,sep = " ", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
    newValue=eigen(x, symmetric=TRUE, only.values = TRUE)
    maxValue = max(newValue$values)
    second=max((newValue$values)[(newValue$values)!=max(newValue$values)] )
    
    #maxPer = maxValue / sum(newValue$values)
    maxPer = maxValue / sum(abs(second+maxValue))
    A = c(A,maxValue)
    B = c(B, maxPer)
  #}
}


#model_AR = arima(A, order = c(1,0,0), method="ML")


numcreat = seq(1, length(A), by=1)



A = scale(A)
A = (A - min(A)) / (max(A) - min(A))
B = (B - min(B)) / (max(B) - min(B))
#plot(x=numcreat, y=A, xlab="Date", ylab="Eigenvalue", type="l",lty=1, ylim = c(0, 1))

plot(x=numcreat, y=B, xlab="Date", ylab="maxPercent", type="l",lty=1, ylim = c(0, 1))



