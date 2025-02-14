
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CovidGer

<!-- badges: start -->
<!-- badges: end -->

The goal of CovidGer is to provide relatively easy access to the data
used in “Inference under superspreading” by Patrick Schmidt.

The repository contains additional code on the generation of the data
files in the data-raw folder.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Schmidtpk/CovidGer")
```

## Case data by the rki

This is a basic example which shows you how to use the case data by the
rki. See `?rki_new` for the data source.

The following example aggregates German wide cases and deaths by symptom
onset.

``` r
library(CovidGer)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.1.3
library(tidyr)
#> Warning: package 'tidyr' was built under R version 4.1.3
library(ggplot2)

df<-rki_new %>%
  dplyr::filter(!is.na(Refdatum))%>%
  group_by(Refdatum,age)%>%
  summarise(
    positive= sum(AnzahlFall[Neuer.Fall%in%c(0,1)]),
    deaths = sum(AnzahlTodesfall[Neuer.Todesfall%in%c(0,1)])
  ) %>%
  rename(
    date=Refdatum,
  )%>%pivot_longer(c(positive,deaths))

ggplot(df,aes(x=date,y=value))+
  geom_point()+
  geom_line()+
  facet_grid(name~age,scale="free_y")+
  xlab("date of symptom onset")
```

<img src="man/figures/README-example-1.png" width="100%" />

## Delay from Symptom onset to reporting to health departement

The following code computes the delay from symptom onset to reporting.
Symptom onset is given in `Refdatum` and reporting date in `Meldedatum`.

``` r
rki_new %>%
  dplyr::filter(Refdatum>=as.Date("2020-03-01"),
                Refdatum<as.Date("2020-09-01"))%>%
  mutate(
    delay = as.numeric(Meldedatum-Refdatum),
    delay = if_else(delay>14,14,delay),
    delay = if_else(delay<(-7),-7,delay)) %>%
  group_by(Refdatum)%>%
  summarise(
    delaym = mean(delay,na.rm=TRUE),
    delay1 = quantile(delay,na.rm=TRUE,probs = .1),
    delay9 = quantile(delay,na.rm=TRUE,probs = .9))%>%
  rename(date = Refdatum)%>%
  ggplot(aes(x=date,y=delaym))+
  geom_ribbon(aes(ymin=delay1,ymax=delay9),alpha=.2)+
  geom_line()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

### Other data

The Package also contains data on population statistics
(Regionaldatenbank) in `regionaldatenbank`, on location specific weather
from the German Weather Services (DWD) in `weather_dwd`, and on policy
interventions in `interventions.list` and `interventions`.

The intervention data was generated in a spreadsheet, which is directly
accessible
[here](https://docs.google.com/spreadsheets/d/1cmGBMUhBt5y6jwiqaF7lh7VQNMN6D5FCoIltlOzhMfI/edit#gid=0).
