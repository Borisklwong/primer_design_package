library(testthat)
library(BSgenome.Hsapiens.UCSC.hg19)



test_that("simple deletion", {
  gr = GRanges(
    seqnames="chr1",
    ranges=IRanges(start=c(50000, 60000), width=1),
    strand=c("+", "-"),
    partner=c("high", "low"))
  names(gr)=c("low", "high")
  expect_equal(
    getBreakpointFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 10),
    c("GATCCTCTATAAATATCACT", "AGTGATATTTATAGAGGATC"))

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
    c("AACCTGTTTATCATCTTATA","TATAAGATGATAAACAGGTT"))
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
    c("AACCTGTTTAAAATATCACT","AGTGATATTTTAAACAGGTT"))

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
    c("GATCCTCTATTCATCTTATA","TATAAGATGAATAGAGGATC"))
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
    c("GATCCTCTATNAGGCTGGCC","GGCCAGCCTNATAGAGGATC"))
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
    c("GATCCTCTATAATGAAACAGGTTA","TAACCTGTTTCATTATAGAGGATC"))

})

test_that("out of chromosomal bounds", {
  # TODO
  gr = GRanges(
    seqnames="chr1",
    ranges=IRanges(start=c(500000000, 60000), width=1),
    strand=c("+", "-"),
    partner=c("high", "low"))
  names(gr)=c("low", "high")
  expect_equal(
    getBreakpointFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 10),
    c("NNNNNNNNNNNNNNNNNNNN"))
})


#test across mtSeq, sex chr, partly out of bounds



###################
#2 pairs






######################
#Next
#function: creat data.frame contains primer

#try primer3, see the output and modify


#####
#Creat another file

#export to fasta with break points (name) and seq
#Sanity test - check all the results pairs are reverse complement to each other
#Handling of breakpoint homology, check GRDISS result, if width is > 1, homology, shows the homology bouds in a new col
#getBreakpointSeq SVA



#TODO "is.possible.phased function , use findOverlaps function"
#TODO get.potentially.phased function, use findOverlaps function"
