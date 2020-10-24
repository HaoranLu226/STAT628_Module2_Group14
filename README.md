# A Simple, Accurate and Robust Way for Body Fat Prediction


This repository provides a simple, accurate and robust way to predict body fat percentage for men. The descriptions of four directories are as following.

* `data`: Contains the original and cleaned dataset, named `BodyFat.csv` and `cleaned_bodyfat.csv`, respectiely.

* `code`: Contains R codes for data cleaning (`data_cleaning.R`), model construction (`model.R`), figure producing (`figure.R`), as well as codes to support for a web-based R shiny app (`server.R` and `ui.R`). 

* `figure`: Contains figures for model diagnostics, including homoscedasticity (`homoscedasticity.jpeg`), linearity (`linearity.jpeg`) and normality (`normality.jpeg`), as well as two figures to check outliers (`CoodDistance.jpeg` and `LOOCVres.jpeg`).

To reproduce our model, one can use `data_cleaning.R` in the `code` directory to process `BodyFat.csv` in the `data` directory. It's expected to write a file same as `cleaned_bodyfat.csv` in the `data` directory. After that, one can run the `model.R` for the 10 models we've constructed and compare there pros and cons (the first two models are based on the original data `data/BodyFat.csv`). The recommended model is selected in the end, which is our proposed one for building the app. 

To reproduce the figures and app, one can run `figure.R`, `ui.R` and `server.R` in the `code` directory. 

Our [web-based app](https://jzhao55.shinyapps.io/addt/) implements our proposed model. One can first type down the weight, abdomen and wrist circumference measurements with corresponding units, and then click th `Calculate!` button to estimate th body fat percentage. The image on the right shows one possible body shape. And a histogram below shows the body fat (%) among men and the highlights the user's body fat percentage. 


