library(testthat)
library(BSgenome.Hsapiens.UCSC.hg19)


getBreakJunctionFlankingSeq =function (gr, bsgenome= BSgenome.Hsapiens.UCSC.hg19, flank.length=250) {

  ifelse(
    as.logical(strand(gr)=="+"),
    getSeq(bsgenome,names = seqnames(gr), start=start(gr)-flank.length+1, width = flank.length),
    Biostrings::reverseComplement(getSeq(bsgenome,names = seqnames(gr), start=start(gr), width = flank.length))
  )
}
###
test_that("getBreakJunctionFlankingSeq", {
  gr = GRanges(
    seqnames="chr1",
    ranges=IRanges(start=c(50000, 60000, 70000, 80000), width=1),
    strand=c("+", "+", "-", "-"))
  expect_equal(
    getBreakJunctionFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 2),
    c("AT", "GA", "TT", "AT"))
  
})



