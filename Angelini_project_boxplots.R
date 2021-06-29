
setwd("F:/OneDrive - Rowan University/Documents/Rowan Grad/Classwork/7-21 Summer/Statistical Data Analysis/Project")

data <- read.csv(file='./codon_usage.csv',header=TRUE, stringsAsFactors=FALSE)

cleaned_data = data#[data$DNAtype == 0, ] 
cleaned_data$Kingdom = as.numeric(factor(cleaned_data$Kingdom))
kings = unique(cleaned_data$Kingdom)
cleaned_data = cleaned_data[,c(1,4,6:69)]
king_labels = c('Archaea', 'Bacteria', 'Invertebrate', 'Mammal', 'Bacteriophage', 'Plasmid', 'Plant', 'Primate', 'Rodent', 'Virus', 'Vertebrate')

for (i in kings){
  king = cleaned_data[cleaned_data$Kingdom == i, ]
  
  for (j in 1:length(king)){
    king[, j] = as.numeric(king[,j])
    
  }
  path = file.path(".","BoxPlots",paste(king_labels[i],".jpeg"))
  jpeg(file=path,width = 1080, height = 720, quality = 100,)
  print(boxplot(king$UUU, king$UUC, king$UUA,
          king$UUG, king$CUU, king$CUC, king$CUA, king$CUG,
          king$AUU, king$AUC, king$AUA, king$AUG, king$GUU,
          king$GUC, king$GUA, king$GUG, king$GCU, king$GCC,
          king$GCA, king$GCG, king$CCU, king$CCC, king$CCA,
          king$CCG, king$UGG, king$GGU, king$GGC, king$GGA,
          king$GGG, king$UCU, king$UCC, king$UCA, king$UCG,
          king$AGU, king$AGC, king$ACU, king$ACC, king$ACA,
          king$ACG, king$UAU, king$UAC, king$CAA, king$CAG,
          king$AAU, king$AAC, king$UGU, king$UGC, king$CAU,
          king$CAC, king$AAA, king$AAG, king$CGU, king$CGC,
          king$CGA, king$CGG, king$AGA, king$AGG, king$GAU,
          king$GAC, king$GAA, king$GAG, king$UAA, king$UAG,
          king$UGA, names =c("UUU", "UUC","UUA",
                             "UUG", "CUU", "CUC", "CUA", "CUG",
                             "AUU", "AUC", "AUA", "AUG", "GUU",
                             "GUC", "GUA", "GUG", "GCU", "GCC",
                             "GCA", "GCG", "CCU", "CCC", "CCA",
                             "CCG", "UGG", "GGU", "GGC", "GGA",
                             "GGG", "UCU", "UCC", "UCA", "UCG",
                             "AGU", "AGC", "ACU", "ACC", "ACA",
                             "ACG", "UAU", "UAC", "CAA", "CAG",
                             "AAU", "AAC", "UGU", "UGC", "CAU",
                             "CAC", "AAA", "AAG", "CGU", "CGC",
                             "CGA", "CGG", "AGA", "AGG", "GAU",
                             "GAC", "GAA", "GAG", "UAA", "UAG",
                             "UGA"),
          main=paste(king_labels[i]," Kingdom Codon Frequency Histogram"),las=2,
          xlab='Codons',ylab='Frequency'))
  
  par(cex.axis=0.4) # is for x-axis
  dev.off()
}

