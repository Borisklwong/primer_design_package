
bsgenome = BSgenome.Hsapiens.UCSC.hg19
flank.length = 10

#################################

getBreakpointFlankingSeq=function(gr, bsgenome = BSgenome.Hsapiens.UCSC.hg19, flank.length = 250) {
  #extract the leading seq of the high breakpoint start position
  if ( length(runValue(strand(gr[strand(gr)=="+"])=="+")) > 0  & length(runValue(strand(gr[strand(gr)=="-"])=="-")) > 0 & length(gr$insSeq) == 0)
    
  {
    paste0(
      getSeq(bsgenome, names=seqnames(gr[strand(gr)=="+"]), start=start(gr[strand(gr)=="+"])-flank.length+1, width=flank.length),
      getSeq(bsgenome, names=seqnames(gr[strand(gr)=="-"]), start=start(gr[strand(gr)=="-"]), width=flank.length))
  }
  
  else if (length(runValue(strand(gr[strand(gr)=="+"])=="+")) == 0 & length(runValue(strand(gr[strand(gr)=="-"])=="-")) > 0 & length(gr$insSeq) == 0)
  {
    paste0(
      reverseComplement(getSeq(bsgenome, names=seqnames(gr[names(gr)=="high"]), start=start(gr[names(gr)=="high"]), width=flank.length)),
      getSeq(bsgenome, names=seqnames(gr[names(gr)=="low"]), start=start(gr[names(gr)=="low"]), width=flank.length))
  }
  else if (length(runValue(strand(gr[strand(gr)=="-"])=="-")) == 0 & length(runValue(strand(gr[strand(gr)=="+"])=="+")) > 0 & length(gr$insSeq) == 0)
  {
    paste0(
      getSeq(bsgenome, names=seqnames(gr[names(gr)=="low"]), start=start(gr[names(gr)=="low"])-flank.length+1, width=flank.length),
      reverseComplement(getSeq(bsgenome, names=seqnames(gr[names(gr)=="high"]), start=start(gr[names(gr)=="high"])-flank.length+1, width=flank.length)))
  }
  else {
    print("still writting")
  }
}

########################################

# test_that("inversion-like --", {
#   # TODO: test case with both breakends having -ve orientation
  gr = GRanges(
    seqnames="chr1",
    ranges=IRanges(start=c(50000, 60000), width=1),
    strand=c("-", "-"),
    partner=c("high", "low"))
  names(gr)=c("low", "high")

getBreakpointFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 10)
    #c("AGTGATATTTTAAACAGGTT" & "AACCTGTTTAAAATATCACT"))
    #c("still writting"))


getSeq(BSgenome.Hsapiens.UCSC.hg19, names=seqnames(gr[names(gr)=="high"]), start=start(gr[names(gr)=="high"]), width=flank.length)
reverseComplement(getSeq(bsgenome, names=seqnames(gr[names(gr)=="high"]), start=start(gr[names(gr)=="high"]), width=flank.length))
names(gr)
seqnames(gr[strand(gr)=="-"])
seqnames(gr[names(gr)=="high"])
start(gr[names(gr)=="high"])
length(gr$insSeq)

if ("try-error" %in% class(try(getSeq(bsgenome, names=seqnames(gr[strand(gr)=="+"]), start=start(gr[strand(gr)=="+"])-flank.length+1, width=flank.length)))) 

str(gr[strand(gr)=="+"]$insSeq)