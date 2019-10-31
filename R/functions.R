plotPlinkPca <- function(file,file2, x = 1, y = 2){
	
	library(ggplot2)
	library(grid)
	library(gridExtra)



	if(missing(file2)) {
        
		mydata <- data.frame(read.table(file))
		rownames(mydata) <- mydata[,2]
		mydata <- mydata[,3:ncol(mydata)]
		colnames(mydata) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")
		
		p <- ggplot(mydata,aes(file=PC1,file2=PC2))
		p <- p+geom_point() + ggtitle("PCA")

		return(p)
		
		
    } else {
        
		mydata <- data.frame(read.table(file))
		group <- read.table(file2, stringsAsFactors = FALSE)
	
		rownames(mydata) <- mydata[,2]
		mydata <- mydata[,3:ncol(mydata)]
		mydata <- cbind(mydata,group)
		colnames(mydata) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "Population")

		p <- ggplot(mydata,aes(x=mydata[,x],y=mydata[,y],color=Population))
		p <- p+geom_point() + scale_color_brewer(palette="Set1") + ggtitle("PCA") + xlab(paste("PC",x, sep ="")) + ylab(paste("PC",y, sep = ""))

		return(p)
	
	}
}
