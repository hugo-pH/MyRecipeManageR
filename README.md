
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MyRecipeManageR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This aim of the package is simple, create a recipe manager where you can
add new recipes, access them and modify them.

In package contains the functionalities to both create the(`sqlite`)
database and create a `Shiny` app that interacts with it.

## Installation

You can install the package by running:

``` r
pak::pak("hugo-pH/MyRecipeManageR")
```

## Running the app

The app needs a database. If no database is provided, the app will run
with a test database contained in the package.

You can see how to create a database in `data-raw/test_database.R`

To start the app, run:

``` r
run_app(db_path = "PATH/TO/YOUR/SQLITE-DB.sqlite")
```
