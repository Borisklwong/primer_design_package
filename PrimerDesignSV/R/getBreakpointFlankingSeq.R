#' @export
getBreakpointFlankingSeq=function(gr, bsgenome = BSgenome.Hsapiens.UCSC.hg19, flank.length = 250) {
  if (is.null(gr$insSeq)) {
    gr$insSeq = ""
  }
  if ("try-error" %in% class(try(paste0(getBreakJunctionFlankingSeq(gr, bsgenome, flank.length), gr$insSeq, spgs::reverseComplement(getBreakJunctionFlankingSeq(StructuralVariantAnnotation::partner(gr), bsgenome, flank.length), case = "upper"))))) {
    print("NNNNNNNNNNNNNNNNNNNN")

  } else {
  paste0(getBreakJunctionFlankingSeq(gr, bsgenome, flank.length), gr$insSeq, spgs::reverseComplement(getBreakJunctionFlankingSeq(StructuralVariantAnnotation::partner(gr), bsgenome, flank.length), case = "upper"))
  }
}


