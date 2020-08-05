library(seqinr)
library(BSgenome.Hsapiens.UCSC.hg19)
library(StructuralVariantAnnotation)
library(spgs)

#simple deletion

gr = GRanges(
  seqnames="chr1",
  ranges=IRanges(start=c(50000, 60000), width=1),
  strand=c("+", "-"),
  partner=c("high", "low"))
names(gr)=c("low", "high")


write.fasta(as.list(getBreakpointFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 10)), names = names(gr), file.out = "simple_deletion.fasta")
#########################
#2 pairs


gr = GRanges(
  seqnames="chr1",
  ranges=IRanges(start=c(50000, 60000, 70000, 80000), width=1),
  strand=c("+","+","-","-"),
  partner=c("high1","high2","low1","low2"))
names(gr)=c("low1","low2","high1","high2")

