library(ggplot2)



# This function prints chromosomes as bars and transcripts as rectangles (/!\ in order to be visible on the grah, transcript lengths need to be augmented by 200 000 base pairs) 
# Input :
#       - Chromosome lengths in a data frame with columns : "chromosome_number" and "length"
#       - Genes positions : chr, start, end (given as different vectors)
#       - Log Fold Change (lfc) is optional, if given, the function will color each transcripts according to it's regulation (green = up and red = down)
#       - doPlotly : if T, the ggplot can be saved as an interactive plotly graph in html
#       - title : name of the plotly title (needed ony if doPlotly == T)

# Output :
#     - ggplot graph
#     - by uncommenting the code and giving a title, the ggplot can be saved as an interactive plotly graph in html

# Other:
#     - Chr lengths can be obtained from reference fasta index (fai)

produce_chr_graph_with_positions <- function(chr_lengths, id, chr,  start, end,  lfc=NULL, doPlotly=F, title=NULL) {
  
  if(doPlotly == T){
    library(plotly)
  }
  
  # Annotation is used for plotly only, to print data on hovering
  annotation = paste0("Name: ", id, "<br>",
                      "start: ", start, "<br>",
                      "stop: ", end, "<br>")
  if(!is.null(lfc)){ annotation <- paste0(annotation, "LFC: ", lfc) }
  
  
  # Create marks = rectangles representing the transcripts
  marks <- data.frame(chromosome = chr, 
                      start = start,
                      end = end,
                      annotation = annotation
                      )
  
  gg<- ggplot()
  gg <- gg + theme_bw()
  
  # add the chr on the graph
  gg<- gg+ geom_bar(data = chr_lengths, aes(x = chromosome_number, y = length), stat = "identity", fill = "lightgrey", colour = "grey") +
    labs(title = "Transcripts Positions", xlab = "Chromosome Number", ylab = "Chromosome Length") +
    theme(legend.position="none")
  
 
  # If we have log fold change data, color Down regulated transcripts in red, up in green
  if(!is.null(lfc)){
    transcripts <- data.frame(x1 = marks[,1]-0.5, x2 = marks[,1]+0.5, y1 =marks[,2]-100000, y2 = marks[,3]+100000, t=marks[,4], LFC = lfc)
    gg<- gg+  geom_rect(data=transcripts, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color = ifelse(transcripts$LFC>0, 'chartreuse3', "firebrick2")) 
  }
  # else just print random colors
  else {
    #Print the transcripts, we add 200 000 bp on transcript size or else there are not visible on the graph
    transcripts <- data.frame(x1 = marks[,1]-0.5, x2 = marks[,1]+0.5, y1 =marks[,2]-100000, y2 = marks[,3]+100000, t=marks[,4])
    gg<- gg+ geom_rect(data=transcripts, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t)) 
  }
  
  
  if(doPlotly == T){
    if(!is.null(title)){
      (p <- plotly_build(gg))
      
      p %>% layout( hovermode="compare")
      htmlwidgets::saveWidget(p, selfcontained = F, file = title)
    }else {
      stop("You need to provide a title name for plotly graph")  
    }
  }else{
    x11()
    gg
  }
  
}


# Run examples
ChL <- data.frame(chromosome_number = c(0,1,2,3,4,5,6,7,8,9,10,11,12), 
                  length = c(48012048, 88663952, 48614681, 62290286, 72208621, 52070158, 59532096, 56760843, 56938457, 61540751, 59756223, 45475667, 61165649))

genes <- data.frame(chr = c(1,4,10), id = c("geneA", "geneB", "geneC"), start = c(10000, 28614681, 49776223), end = c(11000, 28624681, 49756223), lfc = c(1, -5, 3))

# Exemple gg plot with potato genome        
produce_chr_graph_with_positions(chr_lengths = ChL, id = genes$id, chr = genes$chr, start = genes$start, end = genes$end)

# Exemple plotly with potato genome        
produce_chr_graph_with_positions(chr_lengths = ChL, id = genes$id, chr = genes$chr, start = genes$start, end = genes$end, lfc = genes$lfc, doPlotly = T, title = "test.html")
