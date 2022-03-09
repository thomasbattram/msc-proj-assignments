# ------------------------------------------------------------------------
# assigning MSc students projects
# ------------------------------------------------------------------------

## Aim: Use a method to assign students projects in the fairest way possible to maximise 
##		favoured choices

## Date: 2022-03-08

## pkgs
# install.packages("lpSolve")
library(tidyverse) # tidy code and data
library(readxl) # read excel spreadsheets
library(openxlsx) # write excel spreadsheets
library(lpSolve) # to solve the problem

## data 
filenam <- "Student selections 2022_ph10_b42(pt)_b51_b25_removed.xlsx"
epi_data <- read_xlsx(filenam, sheet = "EPI")
ph_data <- read_xlsx(filenam, sheet = "PH")

# ------------------------------------------------------------------------
# Clean data
# ------------------------------------------------------------------------

## clean epi data
epi_data_c <- epi_data %>%
	dplyr::filter(is.na(ASSIGNED)) %>%
	dplyr::select(SURNAME, FIRSTNAME = `FIRST NAME`, C1 = `1`, C2 = `2`, C3 = `3`, C4 = `4`, C5 = `5`)

## clean ph data
ph_data_c <- ph_data %>%
	dplyr::filter(is.na(ASSIGNED)) %>%
	dplyr::select(SURNAME, FIRSTNAME = `FIRST NAME`, C1 = `1`, C2 = `2`, C3 = `3`, C4 = `4`, C5 = `5`)

## combine data
merged_data_c <- rbind(epi_data_c, ph_data_c)

## Removing people as they haven't got choices on here
rows_to_rm <- which(is.na(merged_data_c$C1) | is.na(merged_data_c$C2))
removed_people <- merged_data_c[rows_to_rm,]
if (length(rows_to_rm) != 0) merged_data_c <- merged_data_c[-rows_to_rm, ]

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

# ------------------------------------------------------------------------
# Extra checks on the project assignments
# ------------------------------------------------------------------------

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

## Checking to see that everyone has been assigned a project they chose
if (any(is.na(out_merged$choice))) {
	message("Shit balls. There are ", sum(is.na(out_merged$choice)), " people who have been assigned a project they didn't choose.")
	message("\nLet's find out why.\n")

	Sys.sleep(5)

	assigned_projects <- out_merged$project
	failed_assignment <- out_merged[is.na(out_merged$choice), ] %>%
		left_join(merged_data_c)
	failed_choices <- failed_assignment[,c("C1", "C2", "C3", "C4", "C5")]
	choices_taken <- sapply(failed_choices, function(x) {
		if (is.na(x)) return(TRUE)
		return(x %in% assigned_projects)
	})
	if (all(choices_taken)) message("All their choices were assigned to other people.\n")

	message("Let's check the choices of those other people\n")
	
	Sys.sleep(5)
	
	temp_out <- out_merged %>% 
		dplyr::filter(project %in% as.character(failed_choices))

	temp_merged_dat <- merged_data_c %>%
		dplyr::filter(SURNAME %in% temp_out$SURNAME)

	taken_projects <- sapply(1:5, function(x) {
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

final_out_tab_ph <- merged_data_c %>%
	left_join(out_merged) %>%
	dplyr::filter(course == "ph") %>%
	dplyr::select(-course)

final_out_tab_epi <- merged_data_c %>%
	left_join(out_merged) %>%
	dplyr::filter(course == "epi") %>%
	dplyr::select(-course)

wb <- createWorkbook()
addWorksheet(wb, sheetName = "PH")
addWorksheet(wb, sheetName = "EPI")
writeDataTable(wb, sheet = "PH", x = final_out_tab_ph, colNames = TRUE, rowNames = FALSE)
writeDataTable(wb, sheet = "EPI", x = final_out_tab_epi, colNames = TRUE, rowNames = FALSE)
saveWorkbook(wb, "msc-project-assignments-2022.xlsx", overwrite = TRUE)


