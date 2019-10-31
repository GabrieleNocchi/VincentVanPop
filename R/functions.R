plotPlinkPca <- function(file, file2, x = 1, y = 2){
	
	library(ggplot2)
	library(grid)
	library(gridExtra)



	if(missing(file2)) {
        
		mydata <- data.frame(read.table(file))
		rownames(mydata) <- mydata[,2]
		mydata <- mydata[,3:ncol(mydata)]
		
		p <- ggplot(mydata,aes(x=mydata[,x],y=mydata[,y]))
		p <- p + geom_point() + ggtitle("PCA") + xlab(paste("PC",x, sep ="")) + ylab(paste("PC",y, sep = ""))

		return(p)
		
		
    } else {
        
		mydata <- data.frame(read.table(file))
		group <- read.table(file2, stringsAsFactors = FALSE)
		colnames(group) <- c("group")
	
		rownames(mydata) <- mydata[,2]
		mydata <- mydata[,3:ncol(mydata)]
		mydata <- cbind(mydata,group)

		p <- ggplot(mydata,aes(x=mydata[,x],y=mydata[,y],color=group))
		p <- p + geom_point() + scale_color_brewer(palette="Set1") + ggtitle("PCA") + xlab(paste("PC",x, sep ="")) + ylab(paste("PC",y, sep = ""))

		return(p)
	
	}
}
