library("docxtractr")
library("here")

worddoc <- docxtractr::read_docx("EmoryCampusModel_Preprint_v2.docx")

tab1 = docxtractr::docx_extract_tbl(worddoc, tbl_number = 3)

tablefile = paste0(here('tables'),'/paramtable.Rds')
saveRDS(tab1, file = tablefile)
