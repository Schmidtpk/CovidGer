library(tidyverse)

df <- readxl::read_excel("data-raw/contacts/contact matrices.xlsx",
                sheet = "Scenerio 1, all contacts")

df <- df |> rename(age= `participant's age`,
                   age2= `contact's age`,
                   contact = `contacts values`,
                   setting = `Studies/Settings`)

df <- df |> mutate(age = case_when(
  age %in% c("[0,5)") ~ "A00-A04",
  age %in% c("[5,10)",  "[10,15)") ~ "A05-A14",
  age %in% c("[15,20)", "[20,25)", "[25,35)") ~ "A15-A34",
  age %in% c("[35,45)", "[45,55)","[55,65)") ~  "A35-A59",
  age %in% c("[65,70)","[70,75)","[75,80)") ~   "A60-A79",
  age %in% c("80+") ~ "A80+"),
  age2 = case_when(
    age2 %in% c("[0,5)") ~ "A00-A04",
    age2 %in% c("[5,10)",  "[10,15)") ~ "A05-A14",
    age2 %in% c("[15,20)", "[20,25)", "[25,35)") ~ "A15-A34",
    age2 %in% c("[35,45)", "[45,55)","[55,65)") ~  "A35-A59",
    age2 %in% c("[65,70)","[70,75)","[75,80)") ~   "A60-A79",
    age2 %in% c("80+") ~ "A80+"))

df <- df |>
  group_by(age,age2,setting) |>
  summarise(contact = sum(contact))

dfs <- df |> filter(setting=="Wave 1, overall") |> arrange(age2,age)

# create contact matrix
contact.matrix <- array(dfs$contact,
                        dim = rep(n_distinct(dfs$age),2),
                        dimnames = list(unique(dfs$age),unique(dfs$age)))

contacts <- df

usethis::use_data(contacts)
usethis::use_data(contact.matrix)
