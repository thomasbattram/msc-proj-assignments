# Assigning projects to students

Steps for dissertation assignment:

1. Project list is obtained
2. Students choose top 10 projects
3. Projects are assigned to students with the aim of giving them their favourite project

Issue is that students may want to do the same projects - damn students! So projects need to be assigned in the fairest possible way. [`project-assignment-report.html`](project-assignment-report.html) shows the process of using the [`lpSolve`](https://cran.r-project.org/web/packages/lpSolve/index.html) R package to try and assign projects. 

The [`project-assignment-report.Rmd`](project-assignment-report.Rmd) file was used with data from the 2020/2021 project assignments (`FINAL student selections 2021.xlsx`) to generate the report. If you have access to the spreadsheet then the report can be generated by:

1. Installing all R packages seen in first code chunk of [`project-assignment-report.Rmd`](project-assignment-report.Rmd)
2. Putting `FINAL student selections 2021.xlsx` in the same directory as [`project-assignment-report.Rmd`](project-assignment-report.Rmd)
3. Running [`bash render-report.sh`](render-report.sh)

[`project-assignment-2022.R`](project-assignment-2022.R) does the assignment and extra checks for students of the year 2022.

[`project-assignment-2023.R`](project-assignment-2023.R) does the assignment and extra checks for students of the year 2023.

## NOTES

1. PH students should get prioritised for projects that are relevant to PH and EPI ("both")
	- this hasn't been coded, but rather flagged where EPI students have been assigned a "both" project

2. To solve 1. there are a few options:
	1. manually solve it at the end - doesn't look too difficult based on the data from MSc year 2020-2021
	2. add code to iteratively reassign EPI students projects until the PH students get prioritised
	3. weight PH students choosing "both" projects to start with - i.e. instead of it being 1st choice it is "0th" choice

3. Trying to put students together or do it separately has been tested. It seems best to put all students together and run the code. 
