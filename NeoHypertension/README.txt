######################################################################################################################################
# Organisation 
# https://www.stat.ubc.ca/~jenny/STAT545A/block19_codeFormattingOrganization.html#organization
# https://stackoverflow.com/a/3031551
# https://rmarkdown.rstudio.com/articles_docx.html
######################################################################################################################################

data. This is where data from the outside world goes. If I do extensive cleaning, I might also write my ready-to-analyze dataset here.
meta. For storing meta data, such as variable labels, scoring systems for tests, recoding information, etc.
code. This is where R scripts go (and any other code).
figs. This is where figures go.
results. This is where the outputs of data aggregation, statistical modelling, inference, etc. go.
prose. Holds key emails, internal documentation and explanations, interim reports of analyses, talks, manuscripts, final publications.
rmd. This is where R Markdown files go.
common. The common directory is for document elements I re-use from project to project, e.g., business logo, LaTeX preambles, 
bibliography files, etc. I save my styles reference files in this directory. 

Note: knitr is fairly stubborn about the working directory being that in which the file you're compiling lives. 
This can be controlled / worked around, but just don't be surprised that this is a point of some friction. 
You can expect to pay extra special attention to this if you are generating figures and/or caching.

######################################################################################################################################
# Workflow 
# https://stackoverflow.com/a/1434424
######################################################################################################################################

I generally break my projects into 4 pieces:

load.R
clean.R
func.R
do.R

load.R: Takes care of loading in all the data required. 
Typically this is a short file, reading in data from files, URLs and/or ODBC. 
Depending on the project at this point I'll either write out the workspace using save() 
or just keep things in memory for the next step.

clean.R: This is where all the ugly stuff lives - taking care of missing values, merging data frames, handling outliers.

func.R: Contains all of the functions needed to perform the actual analysis. 
source()'ing this file should have no side effects other than loading up the function definitions. 
This means that you can modify this file and reload it without having to go back an repeat steps 1 & 2 
which can take a long time to run for large data sets.

do.R: Calls the functions defined in func.R to perform the analysis and produce charts and tables.

The main motivation for this set up is for working with large data whereby you don't want to have to reload the data 
each time you make a change to a subsequent step. 
Also, keeping my code compartmentalized like this means I can come back to a long forgotten project and quickly read 
load.R and work out what data I need to update, and then look at do.R to work out what analysis was performed.


######################################################################################################################################
# renv
# https://rstudio.github.io/renv/articles/renv.html
######################################################################################################################################

The general workflow when working with renv is:

1. Call renv::init() to initialize a new project-local environment with a private R library,
2. Work in the project as normal, installing and removing new R packages as they are needed in the project,
3. Call renv::snapshot() to save the state of the project library to the lockfile (called renv.lock),
4. Continue working on your project, installing and updating R packages as needed.
5. Call renv::snapshot() again to save the state of your project library if your attempts to update R packages were successful, 
or call renv::restore() to revert to the previous state as encoded in the lockfile if your attempts to update packages 
introduced some new problems.