library(testthat)
library(BSgenome.Hsapiens.UCSC.hg19)


gr = GRanges(
  seqnames="chr1",
  ranges=IRanges(start=c(50000, 60000), width=1),
  strand=c("+", "-"),
  partner=c("high", "low"))
names(gr)=c("low", "high")

gr

gr
seqnames(gr)[strand(gr)=="+"]
seqnames(gr)
strand(gr)




  getSeq(BSgenome.Hsapiens.UCSC.hg19, names=seqnames(gr[strand(gr)=="+"]), start=start(gr[strand(gr)=="+"])-flank.length, width=flank.length)

getSeq(BSgenome.Hsapiens.UCSC.hg19, names=seqnames(gr), start=start(gr), width=10)



flanking.gr=#extract 250bps of the leanding sequence from hg19
  #if strand = "+"
  #extract 250bps of the following sequence from hg19
  #if strand = "-"
       
# paste0(
#   getSeq(BSgenome.Hsapiens.UCSC.hg19, GRanges(seqnames="chr1", ranges=IRanges(start=49991, width=10))),
#   getSeq(BSgenome.Hsapiens.UCSC.hg19, GRanges(seqnames="chr1", ranges=IRanges(start=60000, width=10))))
  
  
getBreakpointFlankingSeq=function(gr, bsgenome = BSgenome.Hsapiens.UCSC.hg19, flank.length = 250) {
  paste0(
  getSeq(bsgenome, names=seqnames(gr[strand(gr)=="+"]), start=start(gr[strand(gr)=="+"])-flank.length+1, width=flank.length),
  getSeq(bsgenome, names=seqnames(gr[strand(gr)=="-"]), start=start(gr[strand(gr)=="-"]), width=flank.length))
}


getBreakpointFlankingSeq(query.gr, flank.length=10)

names(query.gr)




getSeq(BSgenome.Hsapiens.UCSC.hg19, "chr1", 60000, 60009)


reverseComplement(getSeq(BSgenome.Hsapiens.UCSC.hg19, "chr1", 49991, 50000))

