#!/bin/bash

#===============================================================================
#
#          FILE:  compile.sh
#        AUTHOR:  Edoardo Costantini
#       VERSION:  1.0
#       CREATED:  2022-10-13
#      REVISION:  2022-10-13
#   DESCRIPTION:  Bash script to compile any version fo the paper
#
#         USAGE:  . compile.sh $1
#
#     ARGUMENTS: $1  <file>. <file>.tex <file>.Rnw
#       OPTIONS:  ---
#         NOTES:  ---
#===============================================================================

# Store the time for easy clean up
timestamp=$(date +"%Y-%m-%d %H:%M:%S")
sleep 1 # make sure that files created differ by at least 1 sec form timestamp

# Extract file name without any extension
FILENAMEEXT=$1
FILENAME="${FILENAMEEXT%.*}"

# Pass the filename to knitting
Rscript --vanilla ./code/knit-rnw.R $FILENAME

# Store the time for easy clean up
timestamp=$(date +"%Y-%m-%d %H:%M:%S")
sleep 1 # make sure that files created differ by at least 1 sec form timestamp

# Deploy smart pdflatex
latexmk -pdf $FILENAME.tex

# Move pdf file to correct location
mv *.pdf ./pdf/

# Remove anything else created after this script started
tput bold
printf "These files were created and deleted: \n"
tput sgr0
find . -maxdepth 1 -type f -newerBt "$timestamp" # show what you are deleting
find . -maxdepth 1 -type f -newerBt "$timestamp" -delete
# -newerBt checks files that were created after the timestamp (B for birth)
# -maxdepth 1 avoids scans of subfolders
