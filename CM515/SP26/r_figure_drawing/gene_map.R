setwd("~/Documents/ColoradoState/Teaching/CM580A3/2022/r_figure_drawing/")

lengths=read.delim("data/contig_lengths.txt")

genes=read.delim("data/gene_map.txt")

plot.new()

max_length=max(lengths$Length)
box_height = 0.05

for (i in 1:dim(lengths)[1]){
  segments(0,1-lengths$Contig[i]/5, lengths$Length[i]/max_length, 1-lengths$Contig[i]/5)
  text(lengths$Length[i]/max_length + 0.01, 1-lengths$Contig[i]/5, paste("Contig", lengths$Contig[i]), adj=0, font=2)
}

for (i in 1:dim(genes)[1]){
  polygon(c(genes$Start[i]/max_length, genes$End[i]/max_length, genes$End[i]/max_length, genes$Start[i]/max_length), c(1-genes$Contig[i]/5 + box_height/2, 1-genes$Contig[i]/5 + box_height/2, 1-genes$Contig[i]/5 - box_height/2, 1-genes$Contig[i]/5 - box_height/2), col="steelblue3") 
  text((genes$Start[i] + (genes$End[i] - genes$Start[i])/2)/max_length, 1-genes$Contig[i]/5 - box_height/2 -0.02, genes$Gene[i], adj=c(0.5,1), font=3, cex=0.75)
}  
