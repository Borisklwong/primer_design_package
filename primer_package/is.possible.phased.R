gr = GRanges(
  seqnames="chr1",
  ranges=IRanges(start=c(50000, 60000, 70000, 80000), width=1),
  strand=c("+", "+", "-", "-"),
  partner=c("high1", "high2", "low1", "low2"))
names(gr)=c("low1", "low2", "high1", "high2")


gr

#############################

test_that("simple deletion", {
  gr = GRanges(
    seqnames="chr1",
    ranges=IRanges(start=c(50000, 60000), width=1),
    strand=c("+", "-"),
    partner=c("high", "low"))
  names(gr)=c("low", "high")
  expect_equal(
    getBreakpointFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 10),
    c("GATCCTCTATAAATATCACT"))
  
})


test_that("simple duplication", {
  #TODO
  gr = GRanges(
    seqnames="chr1",
    ranges=IRanges(start=c(50000, 60000), width=1),
    strand=c("-", "+"),
    partner=c("high", "low"))
  names(gr)=c("low", "high")
  expect_equal(
    getBreakpointFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 10),
    c("TATAAGATGATAAACAGGTT"))
})
test_that("inversion-like --", {
  # TODO: test case with both breakends having -ve orientation
  gr = GRanges(
    seqnames="chr1",
    ranges=IRanges(start=c(50000, 60000), width=1),
    strand=c("-", "-"),
    partner=c("high", "low"))
  names(gr)=c("low", "high")
  expect_equal(
    getBreakpointFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 10),
    c("AGTGATATTTTAAACAGGTT"))
  
})
test_that("inversion-like ++", {
  # TODO
  gr = GRanges(
    seqnames="chr1",
    ranges=IRanges(start=c(50000, 60000), width=1),
    strand=c("+", "+"),
    partner=c("high", "low"))
  names(gr)=c("low", "high")
  expect_equal(
    getBreakpointFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 10),
    c("GATCCTCTATTCATCTTATA"))
})

test_that("interchromosomal", {
  # TODO
  gr = GRanges(
    seqnames=c("chr1","chr12"),
    ranges=IRanges(start=c(50000, 60000), width=1),
    strand=c("+", "-"),
    partner=c("high", "low"))
  names(gr)=c("low", "high")
  expect_equal(
    getBreakpointFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 10),
    c("GATCCTCTATNAGGCTGGCC"))
})

test_that("with microhomology", {
  # TODO: test case with breakpoint microhomology4
  #Look up breakpoint
})

test_that("with sequence inserted at breakpoint", {
  # TODO:
  gr = GRanges(
    seqnames="chr1",
    ranges=IRanges(start=c(50000, 50001), width=1),
    strand=c("+", "-"),
    partner=c("high", "low"),
    insSeq="AATG")
  names(gr)=c("low", "high")
  expect_equal(
    getBreakpointFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 10),
    c("GATCCTCTATAATGAAACAGGTTA"))
  
})
test_that("out of chromosomal bounds", {
  # TODO
  gr = GRanges(
    seqnames="chr1",
    ranges=IRanges(start=c(500000000, 50001), width=1),
    strand=c("+", "-"),
    partner=c("high", "low"))
  names(gr)=c("low", "high")
  expect_equal(
    getBreakpointFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 10),
    c("NNNNNNNNNNNNNNNNNNNN"))
})