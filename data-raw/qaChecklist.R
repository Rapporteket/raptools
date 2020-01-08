# Sjekk at du faktisk har:

qaChecklist <- list(`testet at alt fungere som det skal`="ok",
                    `fått godkjenning fra registeret`="ok",
                    `tatt over alle relevante endringer i 'rel' eller master'`="ok")

usethis::use_data(`qaChecklist`, overwrite = TRUE)
