# Sjekk at du faktisk har:

qaChecklist <- list(`testet at alt fungere som det skal`="ok",
                    `faatt godkjenning fra registeret`="ok",
                    `fjernet alle avhengigheter (Imports) til pakker som ikke er produksjonsklare`="ok",
                    `oppdatert versjonsnummer i DESCRIPTION`="ok")

usethis::use_data(`qaChecklist`, overwrite = TRUE)
