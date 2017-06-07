library(qgraph)
library(space)
library(lars)
library(Matrix)
library(glmnet)

tickerLabels=read.table(file="/Users/shanshanli/Desktop/Database/tickers",header=FALSE,sep = ",", quote = "", dec = ".", fill = TRUE, comment.char = "")
tickerLabels=as.matrix(tickerLabels)
tickerLabels=as.vector(tickerLabels)
for (i in 2002:2013) {
    for (j in 01:12) {
        m = i * 100 + j
        filename = paste("/Users/shanshanli/Desktop/10minReturn_02to13/" , as.character(m), "_return_10min", sep="")
        ret=read.table(file=filename,header=FALSE,sep = " ", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
        #ret[is.na(ret)]<-0
        #intervals:seq(1,len,step)
    indexes=seq(1, ncol(ret))
    ret = ret[, indexes]
	num_of_cols=ncol(ret)
    num_of_rows=nrow(ret)
    M1 = t(as.matrix(ret[,1:num_of_cols-1]))
    M2 = t(as.matrix(ret[,2:num_of_cols]))
    A = matrix(nrow = num_of_rows, ncol = num_of_rows)
    for (j in 1:num_of_rows){
        glmmod=glmnet(M1, M2[,j], alpha=1,family='gaussian', maxit=100000, lambda=0.0009)
        A[j,] = as.matrix(coef(glmmod)[2:(num_of_rows+1)])
    }
    #outputfilename = paste("/Users/shanshanli/Desktop/", as.character(i), "WithoutLabel", sep="")
    #qgraph(A,minimum=1e-3,directed=TRUE,layout="circular", legend = FALSE, filetype="jpg", width=20, height=20,node.width=0.5, filename=outputfilename)
    outputFileName = paste("/Users/shanshanli/Desktop/Directed_Graph/", as.character(m), ".jpg", sep="")
    graphName = paste(as.character(m),sep="")
    jpeg(outputFileName, width=4000, height=4000)
    qgraph(A,minimum=1e-3,directed=TRUE,layout="circular", legend = FALSE, width=40, height=40,node.width=1, labels=tickerLabels)
    title(graphName, line = -20, cex.main=15)
    dev.off()
    outputmatrixname = paste("/Users/shanshanli/Desktop/Directed_Matrix/", as.character(m), ".txt", sep="")
    write.table(A, file=outputmatrixname, row.names=FALSE, col.names=FALSE)
}
}