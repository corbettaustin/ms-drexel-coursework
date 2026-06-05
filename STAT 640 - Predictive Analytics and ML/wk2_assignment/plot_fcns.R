# loads/installs libraries as needed
my.library <- function(package, ..., character.only = FALSE) {
  if (!character.only)
    package <- as.character(substitute(package))
  yesno <- require(package, ..., character.only = TRUE)
  if (!yesno) {
    try(install.packages(package, dependencies = TRUE))
    yesno <- require(package, ..., character.only = TRUE)
  }
  invisible(yesno)
} 

## Plotting
my.library("ggplot2")
my.library("scales")
my.library("RColorBrewer")

## Data manipulation
my.library("dplyr")

barp <- function(data,x,freq="freq",main="") {
  X = data[[x]]
  if (freq == "freq" | freq == "count") {
    p <- ggplot(data,aes(x=X, fill=as.factor(X))) + geom_bar() + theme_bw() +
      labs(x=x, fill=x, y="count") + theme(legend.position="none")
  } else {
    Y<- as.data.frame(table(X))
    p <- ggplot(Y,aes(x=X, y=Freq/sum(Freq), fill=X)) + geom_bar(stat="identity") + 
      theme_bw() +
      labs(x=x, fill=x, y="Relative Frequencies") + 
      theme(legend.position="none")
  }
  
  if (main != "") {
    p <- p + ggtitle(label=main) + theme(plot.title = element_text(hjust = 0.5))
  }
  print(p)
}

### Class Conditional Barplot
cc_barplot <- function(data,x,y, freq = "condProb", main = "") {
  Y <- data[[y]]
  X <- data[[x]]
  
  #require(graphics)
  # my_bar <- barplot(X, beside = TRUE, xlab ="student" , ylim=c(0,1), 
  #         col = c("red","darkblue"), 
  #         ylab = "Relative Frequency")
  
  require(ggplot2)
  if (freq == "count" | freq =="freq") {
    p <- ggplot(data, aes(x = X, fill = Y)) + geom_bar(position = "dodge") + 
      labs(fill=y, x=x, y="count") + theme_bw() 
  } else if (freq=="relfreq") {
    
    tab <- table(Y,X)
    cl <- colSums(tab)
    for (i in 1:ncol(tab)) {
      tab[,i] <- tab[,i]/cl[i]
    }
    Y <- as.data.frame(tab)
    p <- ggplot(Y, aes(x=X, y=Freq, fill=Y)) + geom_col(position = "dodge") + 
      labs(fill=y, x=x, y="Relative Frequency") + theme_bw() +
      geom_text(aes(x=X, y=Freq+ 0.03, label=signif(Freq,2)), position=position_dodge(width=0.9))
    
  } else {
    tab <- table(Y,X)
    cl <- rowSums(tab)
    for (i in 1:nrow(tab)) {
      tab[i,] <- tab[i,]/cl[i]
    }
    Y <- as.data.frame(tab)
    p <- ggplot(Y, aes(x=X, y=Freq, fill=Y)) + geom_col(position = "dodge") + 
      labs(fill=y, x=x, y=paste0("P(",x," | ",y,")")) + theme_bw() +
      geom_text(aes(x=X, y=Freq+ 0.03, label=signif(Freq,2)), position=position_dodge(width=0.9)) 
    #ggtitle(paste("Conditional Probability of ",x,"given",y))
  }
  
  if (main != "") {
    p <- p + ggtitle(label=main) + theme(plot.title = element_text(hjust = 0.5))
  }
  print(p)
}

### Class Conditional Histogram
cc_hist <- function(data,x,y, freq="cond", breaks = "Sturges", main="") {
  # class label column
  Y <- data[[y]]
  if (is.factor(Y)) {
    l <- levels(Y)
  } else {
    l <- unique(Y)
  }
  # Predictor column
  X <- data[[x]]
  
  #browser()
  rangeX <- c(min(X),max(X))
  rangeY <- c(0,0)
  
  out <- list()
  # Compute relative frequencies
  for (i in 1:length(l)) {
    out[[i]] <- hist(X[Y==l[i]], plot=FALSE, breaks = breaks)
    
    if (freq=="cond") {
      out[[i]]$counts = out[[i]]$counts/sum(out[[i]]$counts)
    }
    
    if (max(out[[i]]$counts) > rangeY[2]) { 
      rangeY[2] = max(out[[i]]$counts)
    }
  }
  
  if (freq == "cond") {
    yl = "Relative Frequency of Conditional Distribution"
    fr = TRUE
  } else if (freq=="freq") {
    yl = "Frequency" 
    fr = TRUE
  } else {
    yl = "Density" 
    fr = FALSE
  }
  
  # Set colours
  cols <- brewer.pal(n=max(3,length(l)), "Dark2")
  if (length(l) < 3) {
    cols <- c(rgb(1,0,0,0.5), rgb(0,0,1,0.5))
  }
  
  plot(out[[1]], freq=fr, xlim=rangeX, ylim=rangeY, col=alpha(cols[1],0.3),cex=2.5, 
       main= ifelse(main=="",paste("Histogram of",x,"conditional on",y), main), 
       ylab=yl,xlab=x)
  for (i in 2:length(l)) {
    plot(out[[2]], freq=fr,add=TRUE, xlim=rangeX, ylim=rangeY, col=alpha(cols[i],0.3), cex=2.5)
  }
  
  legend("topright",legend=as.factor(l), lty=rep(1,length(l)), 
         lwd=rep(5,length(l)), col=alpha(cols, 0.3))
}

### Boxplot
cc_boxplot <- function(data,x,y,main="") {
  X <- data[[x]]
  Y <- as.factor(data[[y]])
  #if (!is.factor(Y)) { stop("y has to be a categorical variable")  }
  
  p <- ggplot(data,aes(x=Y,y=X,fill=Y)) + geom_boxplot(notch=TRUE) +
    labs(x=y,y=x) + theme_bw() + theme(legend.position="none") 
  
  if (main != "") {
    p <- p + ggtitle(label=main) + theme(plot.title = element_text(hjust = 0.5))
  }
  print(p)
}
