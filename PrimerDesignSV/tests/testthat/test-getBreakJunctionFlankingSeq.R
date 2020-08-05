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

test_that("extends past contig bounds", {
  gr = GRanges(
    seqnames="chrMT",
    ranges=IRanges(start=c(5), width=1),
    strand=c("+"))
  expect_equal(
    getBreakJunctionFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 10),
    c("NNNNNGATCA"))
})
