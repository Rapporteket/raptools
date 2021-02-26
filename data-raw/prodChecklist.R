# Sjekk at du faktisk har:

prodChecklist <- list(`sett at alt fungere som det skal i QA?`="ok",
                      `faatt godkjenning fra registeret?`="ok",
                      `faatt en kollega til aa se over alle endringer?`="ok",
                      `fjernet alle avhengigheter (Imports) til pakker som ikke er produksjonsklare`="ok",
                      `oppdatert versjonsnummer i DESCRIPTION`="ok",
                      `dokumentert endringer siden forrige versjon`="ok")

usethis::use_data(`prodChecklist`, overwrite = TRUE)
