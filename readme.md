wealth-mortality-pimbwe
============

Data and analysis scripts for "Understanding the nature of wealth and its effects on human fitness", by Monique Borgerhoff Mulder and Bret Beheim, in Philosophical Transactions of the Royal Society B (2011), Volume 366, pp. 344-356
DOI: http://dx.doi.org/10.1098/rstb.2010.0231

Requirements:
- R (3.3.1 or greater) https://cran.r-project.org/
- rethinking package (v1.59 or greater), http://xcelab.net/rm/software/
- two R packages: `mice` and `survival`

Instructions:

In R, set the working directory to that containing this readme file. For example, on a Mac or Linux machine, you might say

```
    setwd('~/Desktop/wealth-mortality-pimbwe')
```

if the folder containing the project is named 'wealth-mortality-pimbwe' and on your Desktop. You can tell if you are in the right place by typing in `dir()` and seeing the folders 'code' and 'inputs' and this readme.txt file. The analysis takes as input one file:

- 'original_data.csv' - child survival outcome table from the Tanzanian Pimbwe

The analysis itself is broken up into independent modules that pass outputs to each other. The whole process runs by typing one command into R,

```
    source('./code/run_project.r')
```

with the project folder as the working directory. If all goes well, each step of the analysis will execute in sequence, and write the final tables and figures into an 'output' folder, along with a runtime log.

By default the analysis will delete all temporary files and folders, but if you want to see all intermediate steps you can disable this by flipping the `save_temp` variable in `project_variables.r` from `FALSE` to `TRUE`.

The total time until completion will vary by machine.

The project is maintained by Bret Beheim (beheim@gmail.com) and is hosted at https://github.com/babeheim.
