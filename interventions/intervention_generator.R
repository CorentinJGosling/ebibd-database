library(readxl)
library(tableHTML)
library(tidyverse)
dat = readxl::read_excel(
  paste0("C:/Users/Corentin Gosling/drive_gmail/Recherche/EBI_DATABASES/Bipolar Disorder - EBIBD/website-bd/interventions/",
         "BD_interventions 30march2024.xlsx"))
dat[is.na(dat)] <- ""
dat$reco_sum = paste0(ifelse(dat$`Recommended Dose` == "", "",
                             paste0(" with a recommeded dose of ", dat$`Recommended Dose`)))

dat$max_sum = paste0(ifelse(dat$`Maximum Dose` == "", "",
                            paste0(" up to a maximum dose of ", dat$`Maximum Dose`)))
# dat$reco_sum[which(dat$Indications == "" | dat$`Recommended Dose` == "")] <- ""
# dat$max_sum[which(dat$Indications == "" | dat$`Maximum Dose` == "")] <- ""

dat$ind_reco = paste0(dat$Indications,
                      dat$reco_sum, dat$reco_sum,
                      dat$max_sum, dat$max_sum)

dat = dat %>%
  group_by(Interventions) %>%
  mutate(RECO_TOT = paste0("<br><u>This intervention is indicated in</u> <ul>", paste("<li>", ind_reco, collapse = " </li>"), "</ul>"))

dat$'Descriptions/mechanisms of action' = paste0(dat$Interventions, " ", dat$'Descriptions/mechanisms of action')
dat$RECO_TOT = gsub("\\\r", " ", dat$RECO_TOT)
dat$RECO_TOT = gsub("\\\n", " ", dat$RECO_TOT)
nrow = 1:nrow(dat)
dat$text = with(dat, paste0(
  Interventions, " is a <u>", Group, "</u> intervention. <br>",
  ifelse(Group == "Pharmacological", " Its mechanism of action is ",
         paste0(Interventions, " ")),
  dat$'Descriptions/mechanisms of action',
  ". ",
  ifelse(Indications == "", paste0("The use of ", Interventions, " in BD is off-label in most countries."),
         RECO_TOT)
))
dat$'More information' = paste0('<button onclick="openModal',
                   # "('", dat$Interventions, "')",
                   "(this)",
                   '">Show Modal</button>')
html_tbl = dat[, c("Group", "Interventions", "Descriptions/mechanisms of action", "text", "More information")] %>%
  tableHTML(class='table-fill', escape = FALSE,
            rownames = FALSE)




writeLines(as.character(html_tbl),
           paste0("C:/Users/Corentin Gosling/drive_gmail/Recherche/EBI_DATABASES/",
           "Bipolar Disorder - EBIBD/website-bd/interventions/",
                  "interventions_list",
                  ".html"))
