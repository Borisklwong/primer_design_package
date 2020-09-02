#'
#' @export
getBreakJunctionFlankingSeq = function (gr, bsgenome=BSgenome.Hsapiens.UCSC.hg19, flank.length=250) {
  left_flank_gr = GRanges(
    seqnames = GenomeInfoDb::seqnames(gr),
    ranges = IRanges(start=BiocGenerics::start(gr)-flank.length+1, width = flank.length),
    strand = "+",
    seqinfo = seqinfo(bsgenome))
  left_flank_gr = trim(left_flank_gr)
  left_flank = Biostrings::getSeq(bsgenome,left_flank_gr)

  right_flank_gr = GRanges(
    seqnames = GenomeInfoDb::seqnames(gr),
    ranges = IRanges(start=BiocGenerics::start(gr), width = flank.length),
    strand = "-",
    seqinfo = seqinfo(bsgenome))
  right_flank_gr = trim(right_flank_gr)
  right_flank = Biostrings::getSeq(bsgenome,right_flank_gr)
  seq = ifelse(as.logical(strand(gr)=="+"),left_flank, right_flank)
  return(DNAStringSet(seq))
}


