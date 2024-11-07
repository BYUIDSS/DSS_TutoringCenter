## About

The math tutoring center wants some help! They would like a tool to analyze students' attendance so they can more accurately forecast how many tutors they need. The goal is to get them a dashboard that they can upload a csv and play with some premade graphs they can filter.

## Work to do:

-   Cleaning: Times with 60 means they didn't clock out, remove nulls and tiny clock times (\< 1 min-ish)
-   UI Framing: Get the Shiny framework somewhat resembling the whiteboard
-   Backend: Allow user to add in a csv, probably dynamically clean it
-   Exploration and graphing: Just that. Come up with something useful to include!
-   Documentation (as needed)

## UI

-   Semester filter
-   Graphs
    -   Histogram by time & \# people
    -   Boxplot day of the week
-   Optional tabs

## Starting Place

Here is our day 1 brainstorm:

![Day 1 Whiteboard Plan](raw_data/Day1_Whiteboard_Plan.jpg)

## High-level folders

-   `personal_folders` - This folder has a folder named [lastnamefirstnameletter] for each person on the project.
-   `results` - contains the deliverables from the project.
-   `scripts` - contains the final scripts that built the items in the `results` folder. Each script should be concise and mapped to the deliverables in the `results` folder.
-   `raw_data` has files that are smaller than 100mb provided by the client. Note that a `data` folder can be created in the repository and that it will be ignored for data larger than 100mb.
-   `derived_data` has derived data files. Each script should start from a file in the `raw_data` or `data` folder and create an item in this folder. The script that created the derived data object would be in the `scripts` folder and could have the same name as the derived data object.
-   `documents` contains a folder for the project proposal and any other `reference_material.`

## blank_project_repository

A blank project repository for folder structure and naming guidance. The `confidentiality_agreement.md` can be posted as an issue and each student in the repository must sign it after getting access or their access will be removed.
