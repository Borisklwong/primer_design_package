#'
#' @export
getBreakJunctionFlankingSeq = function (gr, bsgenome=BSgenome.Hsapiens.UCSC.hg19, flank.length=250) {
  ifelse(
    as.logical(strand(gr)=="+"),
    Biostrings::getSeq(bsgenome,names = GenomeInfoDb::seqnames(gr), start=BiocGenerics::start(gr)-flank.length+1, width = flank.length),
    Biostrings::reverseComplement(Biostrings::getSeq(bsgenome,names = GenomeInfoDb::seqnames(gr), start=BiocGenerics::start(gr), width = flank.length))
  )
}
