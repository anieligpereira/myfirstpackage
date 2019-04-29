# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' @author Anieli G Pereira, \email{anieligpreira@@gmail.com}
#'@title Write Total Evidence File in Nexus Format
#'@description A function to write a total evidence matrix in nexus format.
#'@import corHMM
#'@param morpho Morphological alignment
#'@param mol Molecular alignment
#'@param outfile A file name specified
#'@examples writeTE(MorphologicalDataset, MolecularDataset, "outfile.nex")
#'@export

writeTE<-function(morpho, mol, outfile){

  cat(paste(nrow(morpho), (ncol(morpho)+ncol(mol)), "\n"), file=outfile)

  espaco<-max(nchar(rownames(morpho)))+2

  for (i in 1:nrow(morpho)){
    name<-rownames(morpho)[i]
    cat(name, file=outfile, append=T)
    times<-espaco-nchar(name)
    cat(rep(" ", times), sep="", file=outfile, append=T)

    temp<-mol[i,]
    cat(paste(temp, collapse=""), file=outfile, append=T)

    temp<-morpho[i,]
    seq<-unlist(temp)
    multi<-grep("&", levels(seq))
    if (length(multi) >=1){
      temp<-strsplit(as.character(levels(seq)[multi]), "")
      levels(seq)[multi]<-paste0("{", temp[[1]][1], temp[[1]][3], "}")
    }
    cat(paste(seq, collapse=""), file=outfile, append=T)

    cat("\n", file=outfile, append=T)
  }
}

detectDiferences<-function(morpho, mol){
  #morpho
  dif<-setdiff(labels(mol), rownames(morpho))
  dat<-matrix(nrow=length(dif), ncol=ncol(morpho), data = "?")
  colnames(dat)<-c(1:ncol(dat))
  rownames(dat)<-dif
  morpho<-rbind(morpho, dat)
  morpho<-morpho[sort(rownames(morpho)),]

  #molecular
  dif<-setdiff(rownames(morpho), labels(mol))
  len<-ncol(mol)
  final<-comp[,1:len]
  complete_t<-rbind(phy, final)
  complete<-complete_t[sort(rownames(complete_t)),]

}
