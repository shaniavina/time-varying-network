library(qgraph)
library(space)

for (i in 2002:2013) {
    for (j in 01:12) {
        m = i * 100 + j
    filename = paste("/Users/shanshanli/Desktop/30minReturn/" , as.character(m), "_return_30min", sep="")
	ret=read.table(file=filename,header=FALSE,sep = " ", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
    #ret[is.na(ret)]<-0
    #intervals:seq(1,len,step)
    indexes=seq(1, ncol(ret))
    ret = ret[, indexes]
	ret=t(ret)
	p=ncol(ret)
	n=nrow(ret)
	alpha=1
	l1=1/sqrt(n)*qnorm(1-alpha/(2*p^2))
	iter=5
    ret=scale(ret)
	res=space.joint(ret,lam1=l1*n*4.5,lam2=0,weight=1,iter=iter)#lamda
    tickerLabels=read.table(file="/Users/shanshanli/Desktop/Database/tickers",header=FALSE,sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
    tickerLabels=as.matrix(tickerLabels)
    tickerLabels=as.vector(tickerLabels)
   
    outputFileName = paste("/Users/shanshanli/Desktop/Space_Graph/", as.character(m), ".jpg", sep="")
    # qgraph(res$ParCor,minimum=1e-3,layout = "spring", legend = FALSE, filetype="jpg", labels=tickerLabels, node.width=1.1, filename=outputfilename)#minimum
    #graphName = paste(as.character(m),sep="")
    #jpeg(outputFileName, width=4000, height=4000)
    #qgraph(res$ParCor,minimum=1e-3,layout="circular", legend = FALSE, width=40, height=40,node.width=1, labels=tickerLabels)
    #title(graphName, line = -20, cex.main=15)
    #dev.off()
    
    outputmatrixname = paste("/Users/shanshanli/Desktop/Space_Matrix/", as.character(m), ".txt", sep="")
    write.table(res$ParCor, file=outputmatrixname, row.names=FALSE, col.names=FALSE)

    }
	}