
setwd("F:/OneDrive - Rowan University/Documents/Rowan Grad/Classwork/7-21 Summer/Statistical Data Analysis/Project")

data <- read.csv(file='./codon_usage.csv',header=TRUE, stringsAsFactors=FALSE)

cleaned_data = data#[data$DNAtype == 0, ] 

summary(cleaned_data[,c(1,4,6:69)])

cleaned_data$Kingdom = as.numeric(factor(cleaned_data$Kingdom))

require(mosaic)
#require(ggpubr)
king_cleaned_data = cleaned_data[,c(1,4,6:69)] 
headers = colnames(king_cleaned_data)

for (i in 2:length(king_cleaned_data)){
  king_cleaned_data[, i] = as.numeric(king_cleaned_data[,i])
  path = file.path(".","Hist",paste(headers[i],".jpeg"))
  jpeg(file=path)
  print(hist(king_cleaned_data[,i],main=c('Codon: ',headers[i]),xlab='Frequency',ylab='Count',  nclass=100))
  dev.off()
  #path = file.path(".","QQ_plots",paste(headers[i],".jpeg"))
  #jpeg(file=path)
  #print(ggqqplot(king_cleaned_data[,i],main=c('Codon: ',headers[i])))
  #dev.off()
  print(headers[i])
  print(shapiro.test(king_cleaned_data[1:5000,i]))
}

king_data = data.frame(king_cleaned_data)

king_fit <- lm(Kingdom ~ Ncodons + UUU + UUC +
                                  UUA + UUG + CUU + CUC + CUA + CUG +
                                  AUU + AUC + AUA + AUG + GUU +
                                  GUC + GUA + GUG + GCU + GCC +
                                  GCA + GCG + CCU + CCC + CCA +
                                  CCG + UGG + GGU + GGC + GGA +
                                  GGG + UCU + UCC + UCA + UCG +
                                  AGU + AGC + ACU + ACC + ACA +
                                  ACG + UAU + UAC + CAA + CAG +
                                  AAU + AAC + UGU + UGC + CAU +
                                  CAC + AAA + AAG + CGU + CGC +
                                  CGA + CGG + AGA + AGG + GAU +
                                  GAC + GAA + GAG + UAA + UAG +
                                  UGA, data = king_cleaned_data)

summary(king_fit)
anova(king_fit)
confint(king_fit)

kings = unique(cleaned_data$Kingdom)

data <- read.csv(file='./codon_usage_counts.csv',header=TRUE, stringsAsFactors=FALSE)

cleaned_data = data#[data$DNAtype == 0, ] 

cleaned_data$Kingdom = as.numeric(factor(cleaned_data$Kingdom))
king_cleaned_data = cleaned_data[,c(1,4,6:69)] 
kings = c('arc', 'bct', 'inv', 'mam', 'phg', 'plm', 'pln', 'pri', 'rod', 'vrl', 'vrt')

counts = matrix(nrow=length(unique(cleaned_data$Kingdom)),ncol=length(3:ncol(king_cleaned_data)),
                dimnames=list(kings, headers[3:66]))

for (i in 3:ncol(king_cleaned_data)){
  for (j in unique(king_cleaned_data$Kingdom)){
    counts[j,i-2] <- sum(king_cleaned_data[king_cleaned_data$Kingdom == j, i])
  }
}
require("gplots")

dt <- as.table(as.matrix(counts))

chisq <- chisq.test(dt)
chisq
chisq$expected
path = file.path(".","corr_plot.jpeg")
jpeg(file=path,width = 2000, height = 500, quality = 100,)
corrplot(chisq$residuals, is.cor = FALSE,number.cex=0.4)
dev.off()
