#usage: Rscript thisScript geneCoordFile homologyFile outputFileName

#geneCoordFile should be a five-column tab delimited text file with the following columns
#gene start nucleotide position
#gene end nucleotide position
#gene orientation (1 or -1)
#genome number (1 and 2). Number 2  will be drawn on top
#gene color (use R color names)

#homology file should be a six-colum tab delimited text file with the following columns describing links between genes in pairs of genomes
#genomeA number (corresponding to number in column four in the above file
#genomeB number (corresponding to number in column four in the above file
#geneA start nucleotide position
#geneA end nucleotide position
#geneB start nucleotide position
#geneB end nucleotide position


args <- commandArgs(TRUE)

#read in input files and open pdf output file
genes=read.table(args[1], header=TRUE);
connections = read.table(args[2], header=TRUE);
pdf (args[3]);

#determine min and max positions and calculate lengths
species1_min = min(genes[which(genes$Species==1), "Start"])
species1_max = max(genes[which(genes$Species==1), "End"])
species1_length = species1_max - species1_min + 1
species2_min = min(genes[which(genes$Species==2), "Start"])
species2_max = max(genes[which(genes$Species==2), "End"])
species2_length = species2_max - species2_min + 1
largestLength = max (species1_length, species2_length)

#define clean plot for drawing and height of the two genomes to compare
plot.new()
height1 = 0.4
height2 = 0.5

#Also define the length of the triangle portion of each arrow and the height of our gene shapes:
arrowLen = 0.01;
boxHeight = 0.04;

#draw shading connecting homologous gene blocks (do this first do it doesn't cover teh other features
for (i in 1:dim(connections)[1]){
  left1 = (connections[i, "StartA"] - species1_min)/largestLength
  right1 = (connections[i, "EndA"] - species1_min)/largestLength
  left2 = (connections[i, "StartB"] - species2_min)/largestLength
  right2 = (connections[i,"EndB"] - species2_min)/largestLength
  polygon(c(left1, left2, right2, right1), c(height1, height2, height2, height1), col = "gray85", border = NA)
} 

#draw line segments for each "genome"
segments(0, height1, species1_length/largestLength, height1, lwd=3)
segments(0, height2, species2_length/largestLength, height2, lwd=3)

#draw genes blocks/arrows
for (i in 1:dim(genes)[1]){
  #Define the "left" and "right" x-coordinates.
  if (genes[i, "Species"] == 1){
    left = (genes[i,"Start"]-species1_min)/largestLength
    right = (genes[i,"End"]-species1_min)/largestLength
    height = height1
  }else if (genes[i, "Species"] == 2){
    left = (genes[i,"Start"]-species2_min)/largestLength
    right = (genes[i,"End"]-species2_min)/largestLength
    height = height2
  }
  
  #we have already defined the gene colors in our input data file.
  geneFillColor=as.character(genes[i,"Color"]);

  #Draw arrows, distinguishing between forward and reverse oriented genes
  if (genes[i,"Strand"] == 1){
		arrowStart = max (left, right - arrowLen)
		polygon (c(left, arrowStart, right, arrowStart, left), c(height-boxHeight/2, height-boxHeight/2, height, height+boxHeight/2, height+boxHeight/2), col = geneFillColor, lwd = 0.5)
	}else if (genes[i,"Strand"] == -1){
		arrowStart = min (right, left + arrowLen)
		polygon (c(left, arrowStart, right, right, arrowStart), c(height, height-boxHeight/2, height-boxHeight/2, height+boxHeight/2, height+boxHeight/2), col = geneFillColor, lwd = 0.5)
	}
}

#close pdf file
dev.off();

q(status=0)