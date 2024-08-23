---
editor_options: 
  markdown: 
    wrap: 72
---

## Resubmission

This is a resubmission. In this version I have:

-   adapted the examples that exceeded the \> 5s runtime

## R CMD check results

0 errors \| 0 warnings \| 1 note

-   This is a new release.

### Additional notes

#### Notes returned after running `devtools::check(remote = TRUE, manual = TRUE)`:

❯ checking CRAN incoming feasibility ... [4s/13s] NOTE Maintainer:
‘Eduard Klapwijk
[et.klapwijk\@gmail.com](mailto:et.klapwijk@gmail.com){.email}’

New submission

❯ checking HTML version of manual ... NOTE

Found the following HTML validation problems:

`estim_corr.html:4:1 (estim_corr.Rd:5): Warning: <link> inserting "type" attribute     estim_corr.html:12:1 (estim_corr.Rd:5): Warning: <script> proprietary attribute "onload"    estim_corr.html:12:1 (estim_corr.Rd:5): Warning: <script> inserting "type" attribute    estim_corr.html:17:1 (estim_corr.Rd:5): Warning: <table> lacks "summary" attribute    estim_corr.html:36:1 (estim_corr.Rd:10): Warning: <table> lacks "summary" attribute    estim_diff.html:4:1 (estim_diff.Rd:5): Warning: <link> inserting "type" attribute    estim_diff.html:12:1 (estim_diff.Rd:5): Warning: <script> proprietary attribute "onload"   estim_diff.html:12:1 (estim_diff.Rd:5): Warning: <script> inserting "type" attribute    estim_diff.html:17:1 (estim_diff.Rd:5): Warning: <table> lacks "summary" attribute    estim_diff.html:37:1 (estim_diff.Rd:10): Warning: <table> lacks "summary" attribute    feedback.html:4:1 (feedback.Rd:6): Warning: <link> inserting "type" attribute    feedback.html:12:1 (feedback.Rd:6): Warning: <script> proprietary attribute "onload"    feedback.html:12:1 (feedback.Rd:6): Warning: <script> inserting "type" attribute    feedback.html:17:1 (feedback.Rd:6): Warning: <table> lacks "summary" attribute    gambling.html:4:1 (gambling.Rd:6): Warning: <link> inserting "type" attribute    gambling.html:12:1 (gambling.Rd:6): Warning: <script> proprietary attribute "onload"    gambling.html:12:1 (gambling.Rd:6): Warning: <script> inserting "type" attribute    gambling.html:17:1 (gambling.Rd:6): Warning: <table> lacks "summary" attribute    pipe.html:4:1 (pipe.Rd:5): Warning: <link> inserting "type" attribute    pipe.html:12:1 (pipe.Rd:5): Warning: <script> proprietary attribute "onload"    pipe.html:12:1 (pipe.Rd:5): Warning: <script> inserting "type" attribute    pipe.html:17:1 (pipe.Rd:5): Warning: <table> lacks "summary" attribute    pipe.html:19:1 (pipe.Rd:5): Warning: <h2> attribute "id" has invalid value "+25+26gt+3B+25"    pipe.html:35:1 (pipe.Rd:10): Warning: <table> lacks "summary" attribute    pipe.html:36:14 (pipe.Rd:10): Warning: <code> attribute "id" has invalid value "+25+26gt+3B+25_:_lhs"    pipe.html:40:14 (pipe.Rd:12): Warning: <code> attribute "id" has invalid value "+25+26gt+3B+25_:_rhs"    self_eval.html:4:1 (self_eval.Rd:6): Warning: <link> inserting "type" attribute    self_eval.html:12:1 (self_eval.Rd:6): Warning: <script> proprietary attribute "onload"    self_eval.html:12:1 (self_eval.Rd:6): Warning: <script> inserting "type" attribute    self_eval.html:17:1 (self_eval.Rd:6): Warning: <table> lacks "summary" attribute    vicar_char.html:4:1 (vicar_char.Rd:6): Warning: <link> inserting "type" attribute    vicar_char.html:12:1 (vicar_char.Rd:6): Warning: <script> proprietary attribute "onload"    vicar_char.html:12:1 (vicar_char.Rd:6): Warning: <script> inserting "type" attribute    vicar_char.html:17:1 (vicar_char.Rd:6): Warning: <table> lacks "summary" attribute`

0 errors ✔ \| 0 warnings ✔ \| 2 notes ✖

***Author note on HTML validation problems:*** Seems to be a problem on
my local MacOS system (which was not fixed by updating roxygen2), not
encountered on remote system checks.

#### Notes returned after running `devtools::check_win_devel()`:

-   checking CRAN incoming feasibility ... [11s] NOTE

    Maintainer: 'Eduard Klapwijk
    [et.klapwijk\@gmail.com](mailto:et.klapwijk@gmail.com){.email}'

New submission
