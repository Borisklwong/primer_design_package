#' @export
getBreakpointFlankingSeq=function(gr, bsgenome = BSgenome.Hsapiens.UCSC.hg19,
                                  flank.length = 250, export_fasta=TRUE,
                                  fasta_file_name="BreakpointFlankingSeq.fasta")
  {

  if (is.null(gr$insSeq)) {
    gr$insSeq = ""
  }
  local = getBreakJunctionFlankingSeq(gr, bsgenome, flank.length)
  insert = gr$insSeq
  remote = Biostrings::reverseComplement(getBreakJunctionFlankingSeq(StructuralVariantAnnotation::partner(gr), bsgenome, flank.length))
  #adding names
  BreakpointFlankingSeq=DNAStringSet(paste0(local, insert, remote))
  names(BreakpointFlankingSeq) = names(gr)
  return(BreakpointFlankingSeq)
  #Writing output as fasta
  if (export_fasta==TRUE) {
    writeXStringSet(BreakpointFlankingSeq, fasta_file_name)
  } else {print("export_fasta is off")}
}

