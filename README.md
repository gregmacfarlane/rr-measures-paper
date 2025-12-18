# RR Measures Paper

This is the source code for some of the analysis in the paper "Towards Accelerating Transportation Research:  Measuring the Practice of Open Science." It contains the Quarto / LaTeX source for the paper, as well as the analysis code. 

## How to compile the paper

This document is written in Quarto, a literate programming tool for reproducible
research. To compile the paper, you need to install Quarto, R, and LaTeX. You can
find instructions on how to install Quarto
[here](https://quarto.org/docs/get-started/).
The main file is `rrmeasures-paper.qmd`. To compile the paper, you can use the following command:

```
quarto render rrmeasures-paper.qmd
```

This will create a PDF file named `rrmeasures-paper.pdf` in the current directory, assuming that you have the correct LaTeX installation and all the necessary R packages. The necessary packages are listed in the setup chunk at
the top of the `rrmeasures-paper.qmd` file.
