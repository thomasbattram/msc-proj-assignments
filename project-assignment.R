# ------------------------------------------------------------------------
# assigning MSc students projects
# ------------------------------------------------------------------------

## Aim: Use a method to assign students projects in the fairest way possible to maximise 
##		favoured choices

## Date: 2021-09-22

## pkgs
# install.packages("lpSolve")
library(tidyverse) # tidy code and data
library(readxl) # read excel spreadsheets
library(lpSolve) # to solve the problem

## data 
filenam <- "FINAL student selections 2021.xlsx"
epi_data <- read_xlsx(filenam, sheet = "EPI")
ph_data <- read_xlsx(filenam, sheet = "PH")

# ------------------------------------------------------------------------
# example 
# ------------------------------------------------------------------------

# Set assignment costs matrix
costs <- matrix(c(1, 2, 3,
                  2, 3, 1,
                  2, 1 ,3), nrow = 3, byrow = TRUE)

# Print assignment costs matrix
costs

# Final value (z)
lp.assign(costs)

# Variables final values
lp.assign(costs)$solution

### EXAMPLE END

# ------------------------------------------------------------------------
# Test using simulated data more like the real data
# ------------------------------------------------------------------------

## Setup test dataset:
#### students as rows, 5 columns indicating the top 5 project choices for the students

n <- 5 # number of people
n_proj <- 6 # number of projects
n_choices <- 5 # number of choices
tab <- map_dfr(1:n, function(i) {
	as.data.frame(t(matrix(sample(letters[1:n_proj], n_choices))))
})

## Change data so that you have a matrix with projects as columns and people as rows
proj <- unique(unlist(tab))
n_proj_choice <- length(proj)
didne_choose_val <- 10000 # value for when people didn't choose a project as one of their top 5
# proj <- letters[1:n_proj]
proj_choice <- lapply(proj, function(p) {
	sapply(1:nrow(tab), function(row) {
		vec <- as.character(tab[row, ])
		choice <- which(vec == p)
		if (length(choice) == 0) choice <- didne_choose_val
		return(choice)
	})
})
names(proj_choice) <- proj
proj_choice_mat <- bind_cols(proj_choice)
proj_choice_mat <- as.matrix(proj_choice_mat)

## Add in dummy individuals to make the matrix square
proj_people_diff <- n_proj_choice - n
for (i in 1:proj_people_diff) {
	proj_choice_mat <- rbind(proj_choice_mat, rep(didne_choose_val, ncol(proj_choice_mat)))
}
lp.assign(proj_choice_mat)
out_mat <- lp.assign(proj_choice_mat)$solution

## Then just need to assign people back to the project
colnames(out_mat) <- proj

out <- map_dfr(1:nrow(out_mat), function(row) {
	vec <- out_mat[row, ]
	proj_winner <- which(near(vec, 1))
	out_proj <- names(proj_winner)
	ori_choices <- as.character(tab[row, ])
	which_choice <- which(ori_choices == out_proj)
	out_tab <- tibble(person = row, project = out_proj, choice = which_choice)
	return(out_tab)
})

## Here is where you'd write out the results :D 


# ------------------------------------------------------------------------
# Testing the output when there are multiple solutions
# ------------------------------------------------------------------------
costs <- matrix(c(1, 2, 3,
                  1, 2, 3,
                  2, 1 ,3), nrow = 3, byrow = TRUE)

# Print assignment costs matrix
costs

# Final value (z)
lp.assign(costs)

# Variables final values
lp.assign(costs)$solution

### Unclear how it assigns projects, but looks as though the higher rows get worse project assignments

# ------------------------------------------------------------------------
# Using actual data
# ------------------------------------------------------------------------

## clean epi data
epi_data_c <- epi_data %>%
	dplyr::select(SURNAME, FIRSTNAME = `FIRST NAME`, C1 = `1`, C2 = `2`, C3 = `3`, C4 = `4`, C5 = `5`) %>%
	mutate(across(where(is.character), ~na_if(., "NA")))

## clean ph data
ph_data_c <- ph_data %>%
	dplyr::select(SURNAME, FIRSTNAME = `FIRST NAME`, C1 = `1`, C2 = `2`, C3 = `3`, C4 = `4`, C5 = `5`) %>%
	mutate(across(where(is.character), ~na_if(., "NA")))

## combine data
merged_data_c <- rbind(epi_data_c, ph_data_c)

## Removing people as they haven't got choices on here
rows_to_rm <- which(is.na(merged_data_c$C1) | is.na(merged_data_c$C2))
removed_people <- merged_data_c[rows_to_rm,]
merged_data_c <- merged_data_c[-rows_to_rm, ]

## split into people and choices tabs
id_cols <- c("SURNAME", "FIRSTNAME")
people_tab <- merged_data_c[, id_cols] %>%
	mutate(course = ifelse(SURNAME %in% epi_data_c$SURNAME & FIRSTNAME %in% epi_data_c$FIRSTNAME, "epi", "ph"))
tab <- merged_data_c[, !colnames(merged_data_c) %in% id_cols]

## Change data so that you have a matrix with projects as columns and people as rows
n <- nrow(tab)
proj <- unique(unlist(tab))
n_proj_choice <- length(proj)
didne_choose_val <- 10000 # value for when people didn't choose a project as one of their top 5
# proj <- letters[1:n_proj]
proj_choice <- lapply(proj, function(p) {
	sapply(1:nrow(tab), function(row) {
		vec <- as.character(tab[row, ])
		choice <- which(vec == p)
		if (length(choice) == 0) choice <- didne_choose_val
		return(choice)
	})
})
names(proj_choice) <- proj
proj_choice_mat <- bind_cols(proj_choice)
proj_choice_mat <- as.matrix(proj_choice_mat)

## Add in dummy individuals to make the matrix square
proj_people_diff <- n_proj_choice - n
for (i in 1:proj_people_diff) {
	proj_choice_mat <- rbind(proj_choice_mat, rep(didne_choose_val, ncol(proj_choice_mat)))
}
lp.assign(proj_choice_mat)
out_mat <- lp.assign(proj_choice_mat)$solution

## Then just need to assign people back to the project
colnames(out_mat) <- proj

out <- map_dfr(1:nrow(out_mat), function(row) {
	vec <- out_mat[row, ]
	if (row > nrow(people_tab)) {
		person <- tibble(FIRSTNAME = "dummy", SURNAME = "dummy", course = "dummy")
	} else {
		person <- people_tab[row, ]
	}
	proj_winner <- which(near(vec, 1))
	out_proj <- names(proj_winner)
	ori_choices <- as.character(tab[row, ])
	which_choice <- which(ori_choices == out_proj)
	if (length(which_choice) == 0) which_choice <- NA
	out_tab <- tibble(project = out_proj, choice = which_choice)
	out_tab <- cbind(person, out_tab)
	return(out_tab)
})

out_merged <- out %>%
	dplyr::filter(SURNAME != "dummy" & FIRSTNAME != "dummy")


## Checking "both" projects that EPI students have been assigned
chosen_b <- out_merged$project[grepl("B", out_merged$project)]
b_project_flags <- lapply(chosen_b, function(b) {
	temp_out <- out_merged %>%
		dplyr::filter(project == b)
	if (temp_out$course == "epi") {
		merged_data_c %>% 
			filter_all(any_vars(. %in% b)) %>%
			left_join(out_merged) %>%
			return()
	} else {
		return(NULL)
	}
})
names(b_project_flags) <- chosen_b
b_project_flags <- b_project_flags[!sapply(b_project_flags, is.null)]
b_project_flags

## COMPARISON TO ORIGINAL ASSIGNMENTS
# idea is we want to minimize adding the "choice" column

## Original assignments
sum(epi_data$CHOICE) + sum(ph_data$CHOICE, na.rm=T) # 154

## Algorithmic assignments
sum(out_merged$choice) # 123

out_merged %>%
	dplyr::filter(course == "ph") %>%
	pull(choice) %>%
	table()

out_merged %>%
	dplyr::filter(course == "epi") %>%
	pull(choice) %>%
	table()

# improvement of 31 is pretty good I think



