library(ggplot2)

#Read data
eigvectors <- read.table("manypops.eigenvec")
eigenval <- read.table("manypops.eigenval")
Metadata <- read.delim("~/Documents/Courses/IB161_2021/labs_2021/Lab6/Metadata.txt")

#Samples present
samples<-data.frame(Sample.name=eigvectors$V1)
presentS<-merge(samples,Metadata)

#Extract vectors
vecs<-eigvectors[c(3:22)]

#Percent of variance explained
eigvalue <- eigenval$V1/sum(eigenval$V1);
cat(signif(eigvalue , digits=3)*100,"\n");

#Prepare all labels
colnames(vecs) <- paste0("PC",c(1:20))
rownames(vecs)<-eigvectors$V2
vecs$Population <- presentS$Superpopulation.code

# Plot
comp<-c(1,2)
title <- paste("PC",comp[1]," (",signif(eigvalue[comp[1]], digits=3)*100,"%)"," / PC",comp[2]," (",signif(eigvalue[comp[2]], digits=3)*100,"%)",sep="",collapse="")
xlabel = paste("PC",comp[1]," (",signif(eigvalue[comp[1]], digits=3)*100,"%)",sep="")
ylabel = paste("PC",comp[2]," (",signif(eigvalue[comp[2]], digits=3)*100,"%)",sep="")
x_axis = paste("PC",comp[1],sep="")
y_axis = paste("PC",comp[2],sep="")


cbPalette <- c("#D55E00", "#CC79A7","#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
ggplot() + geom_point(data=vecs, aes_string(x=x_axis, y=y_axis, color="Population",shape="Population"),size=2,alpha = .9)+
  scale_colour_manual(values=cbPalette)+
  theme_bw(base_size = 18) +xlab(xlabel)+ylab(ylabel)

#Save image
ggsave(filename=paste0("PopulationsbyContinent","PC",comp[1],"PC",comp[2],".png"),width=10,height=6)


#Explained variance per component
barplot(eigvalue,xlab = "Number of Components",names.arg=c(1:length(eigenval$V1)),
        ylab="Variance (%)",main="PCA Variance explained per component")
barplot(cumsum(eigvalue),xlab = "Number of Components",names.arg=c(1:length(eigenval$V1)),
        ylab="Variance (%)",main="PCA Variance explained by # of components")

