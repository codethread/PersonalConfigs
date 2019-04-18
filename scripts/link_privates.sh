#!/bin/bash

link_privates() {
    normal=$(tput sgr0)
    red=$(tput setaf 1)
    green=$(tput setaf 2)

    dir=$(basename "$PWD")
    fullPath=${SKY_PRIVATE_CONFIGS}/${dir}

    if [ ! -d "$fullPath" ]; then
        echo "     ${red}✘${normal}  $fullPath could not be found"
        exit 1;
    fi

    find "$fullPath" -type f -print0 | while IFS= read -r -d $'\0' file; do 
        ln -s "$file" "$PWD"
        echo "${green}----->  linked ${file}${normal}"
    done

    echo ""
    echo "     ${green}✓${normal}  🦄  Done"
}
