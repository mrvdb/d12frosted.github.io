if [[ is_dirty ]]; then
    git add --all
    git commit -m "snapshot $(date '+%m/%d/%y %H:%M')"
    git push origin master
else
    echo ' .. nothing to do'
fi

cd ..

function is_dirty {
    [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]] && echo "*"
}
