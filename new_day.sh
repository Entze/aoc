#!/bin/zsh

set -e
set -o pipefail

YEAR=$(date +'%Y')
DAY=$(date +'%d')

DRY_RUN="false"

printf "Parsing %d arguments.\n" "$#"

while [ $# -gt 0 ]; do
    case "$1" in
	-y=*);&
	--year=*)
	    YEAR="${1#*=}"
	    ;;
	--y);&
	--year)
	    shift
	    YEAR="${1:-YEAR}"
	    ;;
	--d=*);&
	--day=*)
	    DAY="${1#*=}"
	    ;;
	--d);&
	--day)
	    shift
	    DAY="${1:-DAY}"
	    ;;
	--dry-run)
	    DRY_RUN="true"
	    ;;
    esac
    shift
done

if [ "$DRY_RUN" != "false" ]
then
    printf "Passed --dry-run, not copying anything.\n"
fi

printf "Checking if directory %d already exists.\n" "$YEAR"

if [ -d "$YEAR" ]
then
    printf "Directory %d already exists.\n" "$YEAR"
else
    printf "Directory %d does not yet exist." "$YEAR"
    if [ "$DRY_RUN" = "false" ]
    then
	printf " Creating...\n"
	mkdir -p "$YEAR"
    else
	printf " Doing nothing, dry run enabled.\n"
    fi
fi

printf "Checking if directory %d/Day%d already exists.\n" "$YEAR" "$DAY"

if [ -d "$YEAR/Day$DAY" ]
then
    printf "Directory %d/Day%d already exists.\nCopying would be destructive, exiting.\n" "$YEAR" "$DAY"
    exit 1
else
    printf "Directory %d/%d does not yet exist." "$YEAR" "$DAY"
    if [ "$DRY_RUN" = "false" ]
    then
	printf " Copying template...\n"
	cp --no-clobber --recursive template "$YEAR/Day$DAY"
    else
	printf " Doing nothing, dry run enabled.\n"
    fi
fi

printf "Renaming files containing \"aocYYYYDayDD\" -> \"aoc%dDay%d\".\n" "$YEAR" "$DAY"

FILENAME=${YEAR}/Day${DAY}/aocYYYYDayDD.cabal
NEW_FILENAME=${FILENAME:s/ocYYYYDayDD/oc${~YEAR}Day${~DAY}}
if [ "$DRY_RUN" = "false" ]
then
    printf "Renaming \"%s\" -> \"%s\".\n" "$FILENAME" "$NEW_FILENAME"
    mv "$FILENAME" "$NEW_FILENAME"
    printf "Replacing all strings YYYY to %d and all strings DD to %d in %s.\n" "$YEAR" "$DAY" "$NEW_FILENAME"
    sed -Ei "s/YYYY/${YEAR}/g" "$NEW_FILENAME"
    sed -Ei "s/DD/${DAY}/g" "$NEW_FILENAME"
else
    printf "Would rename \"%s\" -> \"%s\".\n" "$FILENAME" "$NEW_FILENAME"
    printf "Would replace all strings YYYY to %d and all strings DD to %d in %s.\n" "$YEAR" "$DAY" "$NEW_FILENAME"
fi

FILENAME=${YEAR}/Day${DAY}/src/AocYYYYDayDD.hs
NEW_FILENAME=${FILENAME:s/ocYYYYDayDD/oc${~YEAR}Day${~DAY}}
if [ "$DRY_RUN" = "false" ]
then
    printf "Renaming \"%s\" -> \"%s\".\n" "$FILENAME" "$NEW_FILENAME"
    mv "$FILENAME" "$NEW_FILENAME"
    printf "Replacing all strings YYYY to %d and all strings DD to %d in %s.\n" "$YEAR" "$DAY" "$NEW_FILENAME"
    sed -Ei "s/YYYY/${YEAR}/g" "$NEW_FILENAME"
    sed -Ei "s/DD/${DAY}/g" "$NEW_FILENAME"
else
    printf "Would rename \"%s\" -> \"%s\".\n" "$FILENAME" "$NEW_FILENAME"
    printf "Would replace all strings YYYY to %d and all strings DD to %d in %s.\n" "$YEAR" "$DAY" "$NEW_FILENAME"
fi

FILENAME=${YEAR}/Day${DAY}/test/AocYYYYDayDDSpec.hs
NEW_FILENAME=${FILENAME:s/ocYYYYDayDD/oc${~YEAR}Day${~DAY}}
if [ "$DRY_RUN" = "false" ]
then
    printf "Renaming \"%s\" -> \"%s\".\n" "$FILENAME" "$NEW_FILENAME"
    mv "$FILENAME" "$NEW_FILENAME"
    printf "Replacing all strings YYYY to %d and all strings DD to %d in %s.\n" "$YEAR" "$DAY" "$NEW_FILENAME"
    sed -Ei "s/YYYY/${YEAR}/g" "$NEW_FILENAME"
    sed -Ei "s/DD/${DAY}/g" "$NEW_FILENAME"
else
    printf "Would rename \"%s\" -> \"%s\".\n" "$FILENAME" "$NEW_FILENAME"
    printf "Would replace all strings YYYY to %d and all strings DD to %d in %s.\n" "$YEAR" "$DAY" "$NEW_FILENAME"
fi

FILENAME=${YEAR}/Day${DAY}/Main.hs
if [ "$DRY_RUN" = "false" ]
then
    printf "Replacing all strings YYYY to %d and all strings DD to %d in %s.\n" "$YEAR" "$DAY" "$FILENAME"
    sed -Ei "s/YYYY/${YEAR}/g" "$FILENAME"
    sed -Ei "s/DD/${DAY}/g" "$FILENAME"
else
    printf "Would rename \"%s\" -> \"%s\".\n" "$FILENAME" "$FILENAME"
    printf "Would replace all strings YYYY to %d and all strings DD to %d in %s.\n" "$YEAR" "$DAY" "$FILENAME"
fi

exit 0
