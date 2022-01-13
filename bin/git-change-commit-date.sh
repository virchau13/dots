#!/usr/bin/env bash

# Usage:
#     ./git-change-commit-date.sh <commit id | 'all' >
# Example:
#     ./git-change-commit-date.sh e119b94 # change commit with id e119b94
#     ./git-change-commit-date.sh all # changes all commits
#
# Warning! rebase/filter will change the history

COMMIT=$1
if [ -z "$NEWDATE" ]; then
    NEWDATE=$(date -R)
fi
FILTER="true"

# check for missing argument
[ -z "$1" ] && echo "Missing arguments" && exit

# check for changes before rebasing
(git diff-files --quiet || (echo Unstaged changes, please commit or stash with --keep-index; exit 1))

echo "Change commit date of $COMMIT to $NEWDATE"

if [ "$COMMIT" != "all" ]
  then
    COMMIT=$(git rev-parse "$COMMIT")
    FILTER="\$GIT_COMMIT = '$COMMIT'"
fi

git filter-branch -f --env-filter \
    "if [ $FILTER ]
    then
        export GIT_AUTHOR_DATE='$NEWDATE'
        export GIT_COMMITTER_DATE='$NEWDATE'
    fi"
