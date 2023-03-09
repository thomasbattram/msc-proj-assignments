# ------------------------------------------------------------------------
# assigning MSc students projects - 2023
# ------------------------------------------------------------------------

## Aim: Use a method to assign students projects in the fairest way possible to maximise 
##		favoured choices

## Date: 2023-03-09

## pkgs
# install.packages("lpSolve")
library(tidyverse) # tidy code and data
library(readxl) # read excel spreadsheets
library(openxlsx) # write excel spreadsheets
library(lpSolve) # to solve the problem

## data 
filenam <- "Dissertation project selections_2022_23_reformatted.xlsx"
# readxl::excel_sheets(filenam)
all_dat <- read_xlsx(filenam)

# ------------------------------------------------------------------------
# Clean data
# ------------------------------------------------------------------------

## ADD IN SECTION TO CHECK DATA HERE! - e.g. check students haven't duplicated values, etc.

proj_id_cols <- grep("Please enter the project ID number", colnames(all_dat), value = TRUE)
proj_id_cols <- proj_id_cols[!grepl(" - ", proj_id_cols)]
proj_id_cols ## CHECK THIS IS CORRECT
names(proj_id_cols) <- paste0("C", 1:10)

## We expect that `proj_id_cols` is "Please enter the project ID number for your XXX project choice"
## with XXX being "first", "second", "third", etc. 
## Assuming these are in order, the names of these columns will change to "C" plus their corresponding
## choice - e.g. for "Please enter the project ID number for your third project choice", the name should be C3
proj_id_cols

## select important columns (calling the tibble merged_data_c to make things easier copying code from last year)
merged_data_c <- all_dat %>%
	rename(all_of(proj_id_cols)) %>%
	dplyr::select(id = `Student number`, all_of(names(proj_id_cols)))

## Change ID column to a character vector if it isn't already
merged_data_c$id <- as.character(merged_data_c$id)
all_dat[["Student number"]] <- as.character(all_dat[["Student number"]])

## Check student number is unique
any(duplicated(merged_data_c$id)) # FALSE 

## Removing people as they haven't got choices on here
rows_to_rm <- which(is.na(merged_data_c$C1) | is.na(merged_data_c$C2))
removed_people <- merged_data_c[rows_to_rm,]
if (length(rows_to_rm) != 0) merged_data_c <- merged_data_c[-rows_to_rm, ]

## Manually remove people
nam_to_rm <- tibble(id = "") ## ADD NAMES HERE
if (all(nam_to_rm$id != "")) {
	for (i in 1:nrow(nam_to_rm)) {
		message("removing: ", nam_to_rm$id)
		merged_data_c <- merged_data_c %>%
			dplyr::filter(!id == nam_to_rm$id[i])
	}
}

## split into people and choices tabs
id_cols <- c("id")
people_tab <- merged_data_c[, id_cols] %>%
	left_join(all_dat, by = c("id" = "Student number")) %>%
	dplyr::select(Name, `Full student name`, id, course = `MSc Programme to which you are registered`)
tab <- merged_data_c[, !colnames(merged_data_c) %in% id_cols]

## Change data so that you have a matrix with projects as columns and people as rows
n <- nrow(tab)
proj <- unique(unlist(tab))
n_proj_choice <- length(proj)
didne_choose_val <- 10000 # value for when people didn't choose a project as one of their top 10
# proj <- letters[1:n_proj]
proj_choice <- lapply(proj, function(p) {
	sapply(1:nrow(tab), function(row) {
		vec <- as.character(tab[row, ])
		choice <- which(vec == p)
		if (length(choice) == 0) {
			choice <- didne_choose_val	
		} else if (is.na(p)) {
			choice <- didne_choose_val
		}
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

# ------------------------------------------------------------------------
# Run the analysis
# ------------------------------------------------------------------------

lp.assign(proj_choice_mat)
out_mat <- lp.assign(proj_choice_mat)$solution

## Then just need to assign people back to the project
colnames(out_mat) <- proj

out <- map_dfr(1:nrow(out_mat), function(row) {
	vec <- out_mat[row, ]
	if (row > nrow(people_tab)) {
		person <- tibble(Name = "dummy", `Full student name` = "dummy", id = "dummy", course = "dummy")
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
	dplyr::filter(Name != "dummy")

# ------------------------------------------------------------------------
# Check results
# ------------------------------------------------------------------------

## See how many people were assigned their 1st choice, 2nd choice, etc.
courses <- unique(out_merged$course)
course_choices_list <- lapply(courses, function(x) {
	out_merged %>%
		dplyr::filter(course == x) %>%
		pull(choice) %>%
		table()	
})
names(course_choices_list) <- courses
course_choices_list

table(out_merged$choice)

# ------------------------------------------------------------------------
# Extra checks on the project assignments
# ------------------------------------------------------------------------

## Checking to see that everyone has been assigned a project they chose
if (any(is.na(out_merged$choice))) {
	message("Shit balls. There are ", sum(is.na(out_merged$choice)), " people who have been assigned a project they didn't choose.")
	message("\nLet's find out why.\n")

	Sys.sleep(5)

	assigned_projects <- out_merged$project
	failed_assignment <- out_merged[is.na(out_merged$choice), ] %>%
		left_join(merged_data_c)
	message("Here are the individuals and the choices they put down: ")
	print(failed_assignment)
	failed_choices <- failed_assignment[, colnames(tab)] # colnames of tab should just be C1:C10
	choices_taken <- sapply(failed_choices, function(x) {
		if (is.na(x)) return(TRUE)
		return(x %in% assigned_projects)
	})
	if (all(choices_taken)) message("\nAll their choices were assigned to other people.\n")

	message("Let's check the choices of those other people\n")
	
	Sys.sleep(5)
	
	temp_out <- out_merged %>% 
		dplyr::filter(project %in% as.character(failed_choices))

	temp_merged_dat <- merged_data_c %>%
		dplyr::filter(id %in% temp_out$id)

	message("Here are the individuals that have been assigned the projects of choice by those who couldn't be assigned a project: ")
	print(temp_merged_dat)

	taken_projects <- sapply(1:10, function(x) {
		projs <- temp_merged_dat[[paste0("C", x)]]
		out <- sum(projs %in% assigned_projects)
		out + sum(is.na(projs))
	})

	if (all(taken_projects == nrow(temp_merged_dat))) {
		message("All their project choices are also taken...\n\n", 
				"It's going to need to some mental manual shit to sort out.")
	} else {
		message("Some manual changes should be able fix things.")
	}
}

# ------------------------------------------------------------------------
# Output the assigned projects
# ------------------------------------------------------------------------

## Tidy out merged
tidy_out <- out_merged %>%
	rename(`Student number` = id, `Project ID` = project, `Choice number` = choice, 
		   `MSc Programme to which you are registered` = course) %>%
	as_tibble()

## Split by subject?

## output data
wb <- createWorkbook()
addWorksheet(wb, sheetName = "Sheet1")
writeDataTable(wb, sheet = "Sheet1", x = tidy_out, colNames = TRUE, rowNames = FALSE)
saveWorkbook(wb, "msc-project-assignments-2023.xlsx", overwrite = TRUE)



