plotPlinkPca <- function(x,y){
	
	library(ggplot2)
	library(grid)
	library(gridExtra)



	if(missing(y)) {
        
		mydata <- data.frame(read.table(x))
		rownames(mydata) <- mydata[,2]
		mydata <- mydata[,3:ncol(mydata)]
		colnames(mydata) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")
		
		p <- ggplot(mydata,aes(x=PC1,y=PC2))
		p <- p+geom_point() + ggtitle("PCA")

		return(p)
		
		
    } else {
        
		mydata <- data.frame(read.table(x))
		group <- read.table(y, stringsAsFactors = FALSE)
	
		rownames(mydata) <- mydata[,2]
		mydata <- mydata[,3:ncol(mydata)]
		mydata <- cbind(mydata,group)
		colnames(mydata) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "Population")

		p <- ggplot(mydata,aes(x=PC1,y=PC2,color=Population))
		p <- p+geom_point() + scale_color_brewer(palette="Set1") + ggtitle("PCA")

		return(p)
	
	}
}
