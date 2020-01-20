# Sjekk at du faktisk har:

prodChecklist <- list(`sett at alt fungere som det skal i QA?`="ok",
                      `faatt godkjenning fra registeret?`="ok",
                      `tatt over alle relevante endringer i 'master'?`="ok",
                      `faatt en kollega til aa se over alle endringer?`="ok",
                      `fjernet alle avhengigheter (Imports) til pakker som ikke er produksjonsklare`="ok",
                      `oppdatert versjonsnummer i DESCRIPTION`="ok",
                      `tilsvarende tagget versjonen i git som skal legges ut`="ok")

usethis::use_data(`prodChecklist`, overwrite = TRUE)
