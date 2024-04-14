library(readxl)
library(tableHTML)
library(tidyverse)
remove_first_capital <- function(string) {
  first_letter <- substr(string, 1, 1)  # Get the first letter
  rest_of_string <- substr(string, 2, nchar(string))  # Get the rest of the string
  return(paste(tolower(first_letter), rest_of_string, sep = ""))
}

dat = readxl::read_excel(
  paste0("C:/Users/Corentin Gosling/drive_gmail/Recherche/EBI_DATABASES/Bipolar Disorder - EBIBD/website-bd/interventions/",
         "BD_interventions 12april2024.xlsx"))
dat$Age[is.na(dat$Age)] <- "Not specified"

dat[is.na(dat)] <- ""
dat$reco_sum = ifelse(dat$`Recommended Dose` == "", "",
                      paste0(" with a recommeded dose of ", dat$`Recommended Dose`))

dat$max_sum = ifelse(dat$`Maximum Dose` == "", "",
                     paste0(" up to a maximum dose of ", dat$`Maximum Dose`, ". "))

dat$reco_max = ifelse(dat$reco_sum != "" & dat$max_sum != "",
                      paste0(dat$reco_sum, ", and ", dat$max_sum),
                      paste0(dat$reco_sum, dat$max_sum))

dat$ind_reco = paste0(dat$Indications,
                      dat$reco_max
                      # dat$reco_sum, dat$max_sum#,
                      #dat$max_sum, dat$max_sum
                      )

dat = dat %>%
  group_by(Interventions, Age) %>%
  mutate(RECO_TOT_age = paste0("<u>In ", remove_first_capital(Age), ", ", remove_first_capital(Interventions),
                           " is indicated for:</u> <ul class='list_inter'>",
                           paste("<li>", ind_reco, collapse = " </li>"), "</ul>")) %>%
  ungroup() %>%
  group_by(Interventions) %>%
  mutate(RECO_TOT = paste(unique(RECO_TOT_age), collapse = "<br>"),
         Age = paste(unique(Age), collapse = " / "))


# dat$'Descriptions/mechanisms of action' = paste0(dat$Interventions, " is a ", dat$'Descriptions/mechanisms of action')

# for (i in 1:nrow(dat)) {
#   if (dat$Group[i] == "Pharmacological") {
#
#   }
# }
dat$RECO_TOT = gsub("\\\r", " ", dat$RECO_TOT)
dat$RECO_TOT = gsub("\\\n", " ", dat$RECO_TOT)
nrow = 1:nrow(dat)
dat$text = with(dat, paste0(
  paste0("<div class='desc_tab'>More information about ", remove_first_capital(Interventions), "</div>"),

  "<div class='header_tab'>Type of intervention.</div>",
  stringr::str_to_title(Interventions), " is a <u>", remove_first_capital(Group), "</u> intervention. <br>",

  ifelse(Group == "Pharmacological",
         "<div class='header_tab'>Mecanism of action.</div>",
         "<div class='header_tab'>Description of the intervention.</div>"),
  ifelse(Group == "Pharmacological",
         paste0(" The mecanism of action of ",
                remove_first_capital(dat$'Descriptions/mechanisms of action')),
         dat$'Descriptions/mechanisms of action'),
  ". ",

  "<div class='header_tab'>Indication for this intervention.</div>",
  ifelse(Indications == "",
         paste0("The use of ", remove_first_capital(Interventions),
                " in BD is off-label in most countries."),
         RECO_TOT)
))



dat$'More information' = paste0('<button onclick="openModal',
                   # "('", dat$Interventions, "')",
                   "(this)",
                   '">Learn more</button>')
html_tbl = dat[, c("Group", "Interventions", "Age", "Descriptions/mechanisms of action", "text", "More information")] %>%
            distinct() %>%
  tableHTML(class='table-fill', escape = FALSE,
            rownames = FALSE)




writeLines(as.character(html_tbl),
           paste0("C:/Users/Corentin Gosling/drive_gmail/Recherche/EBI_DATABASES/",
           "Bipolar Disorder - EBIBD/website-bd/interventions/",
                  "interventions_list",
                  ".html"))
