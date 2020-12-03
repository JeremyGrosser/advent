#!/bin/bash

for url in `cat repolist`; do
    username=$(echo "${url}" | cut -d'/' -f4)
    if [ ! -e ${username} ]; then
        git clone ${url} ${username}
    fi
    echo "--------------------------------------------------------------------------------"
    echo "${username}"
    pushd ${username} >/dev/null
    git fetch -q
    git diff HEAD..origin/$(git branch --show-current)
    git pull -q -s recursive -X theirs origin $(git branch --show-current)
    popd >/dev/null
done
