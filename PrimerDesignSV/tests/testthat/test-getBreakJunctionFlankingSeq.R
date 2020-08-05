library(testthat)
library(BSgenome.Hsapiens.UCSC.hg19)

test_that("getBreakJunctionFlankingSeq", {
  gr = GRanges(
    seqnames="chr1",
    ranges=IRanges(start=c(50000, 60000, 70000, 80000), width=1),
    strand=c("+", "+", "-", "-"))
  expect_equal(
    getBreakJunctionFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 2),
    c("AT", "GA", "TT", "AT"))

})
