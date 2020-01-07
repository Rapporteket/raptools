# Sjekk at du faktisk har:

prodChecklist <- list(`sett at alt fungere som det skal i QA?`="ok",
                      `fått godkjenning fra registeret?`="ok",
                      `tatt over alle relevante endringer i 'master'?`="ok",
                      `fått en kollega til å se over alle endringer?`="ok",
                      `tagget versjonen som skal legges ut?`="ok")

usethis::use_data(`prodChecklist`, overwrite = TRUE)
