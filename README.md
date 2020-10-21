
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dfuzz

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/dfuzz)](https://CRAN.R-project.org/package=dfuzz)
<!-- badges: end -->

The goal of **{dfuzz}** is to help you cleaning up a messy column of
strings of characters in your `tibble` or `data.frame`.

This package is **highly experimental** and is not yet ready for being
used for real applications.

It is build around two dependencies which themselves have no
dependencies:

-   [**{rlang}**](https://github.com/r-lib/rlang)

-   [**{stringdist}**](https://github.com/markvanderloo/stringdist), and
    it is possible to use the full power of the function `stringdist()`
    from this excellent package.

**{dfuzz}** aims at being compatible with both *tidyverse* and *base* R
dialects.

## Installation

You can install this package using **{remotes}** (or **{devtools}**):

    remotes::install_github("courtiol/dfuzz")

## Example

    library(dfuzz)

    ## a toy example:
    test_df <- data.frame(fruit = c("banana", "blueberry", "limon", "pinapple",
                                    "aple", "apple", "ApplE", "bonana"))
    test_df
    #>       fruit
    #> 1    banana
    #> 2 blueberry
    #> 3     limon
    #> 4  pinapple
    #> 5      aple
    #> 6     apple
    #> 7     ApplE
    #> 8    bonana

    ## fast and dirty workflow:
    clean_df1 <- fuzzy_tidy(test_df, fruit)
    clean_df1
    #>       fruit fruit.clean fruit.cleaned fruit.tidy
    #> 1    banana        <NA>        banana     banana
    #> 2 blueberry   blueberry          <NA>  blueberry
    #> 3     limon       limon          <NA>      limon
    #> 4  pinapple    pinapple          <NA>   pinapple
    #> 5      aple        <NA>          aple       aple
    #> 6     apple        <NA>          aple       aple
    #> 7     ApplE       ApplE          <NA>      ApplE
    #> 8    bonana        <NA>        banana     banana

    ## more subtle workflow:
    template_fruit <- fuzzy_match(test_df, fruit)
    template_fruit
    #>   selected  syn_1  syn_2
    #> 1     aple   aple  apple
    #> 2   banana banana bonana
    template_fruit$selected[1] <- "apple"
    clean_df2 <- fuzzy_tidy(test_df, fruit, template_fruit)
    clean_df2
    #>       fruit fruit.clean fruit.cleaned fruit.tidy
    #> 1    banana        <NA>        banana     banana
    #> 2 blueberry   blueberry          <NA>  blueberry
    #> 3     limon       limon          <NA>      limon
    #> 4  pinapple    pinapple          <NA>   pinapple
    #> 5      aple        <NA>         apple      apple
    #> 6     apple        <NA>         apple      apple
    #> 7     ApplE       ApplE          <NA>      ApplE
    #> 8    bonana        <NA>        banana     banana

    ## fast and dirty workflow with {tidyverse}:
    library(tidyverse)
    #> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
    #> ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    #> ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    #> ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    #> ✓ readr   1.4.0     ✓ forcats 0.5.0
    #> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    #> x dplyr::filter() masks stats::filter()
    #> x dplyr::lag()    masks stats::lag()
    test_df %>%
      fuzzy_tidy(fruit) %>%
      mutate(fruit = fruit.tidy) %>%
      select(-contains("fruit."))
    #> # A tibble: 8 x 1
    #>   fruit    
    #>   <chr>    
    #> 1 banana   
    #> 2 blueberry
    #> 3 limon    
    #> 4 pinapple 
    #> 5 aple     
    #> 6 aple     
    #> 7 ApplE    
    #> 8 banana

    ## more subtle workflow with {tidyverse}:
    test_df %>%
      mutate(fruit = str_to_title(fruit)) %>%
      fuzzy_match(fruit) -> template_fruit
    template_fruit
    #> # A tibble: 2 x 3
    #>   selected syn_1  syn_2 
    #>   <chr>    <chr>  <chr> 
    #> 1 Aple     Aple   Apple 
    #> 2 Banana   Banana Bonana

    template_fruit %>%
      mutate(selected = fct_recode(selected, Apple = "Aple")) -> better_template_fruit

    better_template_fruit
    #> # A tibble: 2 x 3
    #>   selected syn_1  syn_2 
    #>   <fct>    <chr>  <chr> 
    #> 1 Apple    Aple   Apple 
    #> 2 Banana   Banana Bonana

    test_df %>%
      mutate(fruit = str_to_title(fruit)) %>%
      fuzzy_tidy(fruit, better_template_fruit) -> clean_df3
    clean_df3
    #> # A tibble: 8 x 4
    #>   fruit     fruit.clean fruit.cleaned fruit.tidy
    #>   <chr>     <chr>       <chr>         <chr>     
    #> 1 Banana    <NA>        Banana        Banana    
    #> 2 Blueberry Blueberry   <NA>          Blueberry 
    #> 3 Limon     Limon       <NA>          Limon     
    #> 4 Pinapple  Pinapple    <NA>          Pinapple  
    #> 5 Aple      <NA>        Apple         Apple     
    #> 6 Apple     <NA>        Apple         Apple     
    #> 7 Apple     <NA>        Apple         Apple     
    #> 8 Bonana    <NA>        Banana        Banana

    clean_df3 %>%
      mutate(fruit = fruit.tidy) %>%
      select(-contains("fruit."))
    #> # A tibble: 8 x 1
    #>   fruit    
    #>   <chr>    
    #> 1 Banana   
    #> 2 Blueberry
    #> 3 Limon    
    #> 4 Pinapple 
    #> 5 Apple    
    #> 6 Apple    
    #> 7 Apple    
    #> 8 Banana

## Help & feedbacks wanted!

If you find that this package is an idea worth pursuing, please let me
know. Developing is always more fun when it becomes a collaborative
work. So please also email me (or leave an issue) if you want to get
involved!
