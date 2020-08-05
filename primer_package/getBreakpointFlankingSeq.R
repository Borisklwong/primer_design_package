library(testthat)
library(BSgenome.Hsapiens.UCSC.hg19)
library(StructuralVariantAnnotation)
library(spgs)

getBreakJunctionFlankingSeq =function (gr, bsgenome= BSgenome.Hsapiens.UCSC.hg19, flank.length=250) {
  
  ifelse(
    as.logical(strand(gr)=="+"), 
    getSeq(bsgenome,names = seqnames(gr), start=start(gr)-flank.length+1, width = flank.length), 
    Biostrings::reverseComplement(getSeq(bsgenome,names = seqnames(gr), start=start(gr), width = flank.length))
  )
}


getBreakpointFlankingSeq=function(gr, bsgenome = BSgenome.Hsapiens.UCSC.hg19, flank.length = 250) {
  if (is.null(gr$insSeq)) {
    gr$insSeq = ""
  }
  if ("try-error" %in% class(try(paste0(getBreakJunctionFlankingSeq(gr, bsgenome, flank.length), gr$insSeq, spgs::reverseComplement(getBreakJunctionFlankingSeq(partner(gr), bsgenome, flank.length), case = "upper"))))) {
    print("NNNNNNNNNNNNNNNNNNNN")

  } else {
  paste0(getBreakJunctionFlankingSeq(gr, bsgenome, flank.length), gr$insSeq, spgs::reverseComplement(getBreakJunctionFlankingSeq(partner(gr), bsgenome, flank.length), case = "upper"))
  }
}



#########################################################




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
    ranges=IRanges(start=c(50000, 60000), width=1),
    strand=c("+", "-"),
    partner=c("high", "low"))
    names(gr)=c("low", "high")
  expect_equal(
    getBreakpointFlankingSeq(gr, BSgenome.Hsapiens.UCSC.hg19, flank.length = 5000000000),
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