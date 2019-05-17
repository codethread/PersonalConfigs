#!/bin/bash

normal=$(tput sgr0)
red=$(tput setaf 1)
green=$(tput setaf 2)
yellow=$(tput setaf 3)

configs=$1
_self="${0##*/}"

if [[ ! -d $configs ]]; then
  echo "${red}missing required arg${normal}: config files directory"
  echo "e.g: ${_self} ~/PersonalConfigs"
  exit 1
fi

touch ~/.teardown_list.txt

function mapFiles() {
  array_of_dirFiles="$(ls -A $1)"

  IFS='
' # prevent splitting on spaces

  for fileOnly in ${array_of_dirFiles}
  do
    dirFile="$1/$fileOnly"

    if	[[ ! $dirFile =~ (README.md|.DS_Store|./.gitignore$|.sw.?$|^./.git$|^./_.*) ]]
    then
      if [[ -d $dirFile ]]; then
        if [[ -L "$HOME/$dirFile" ]]; then
          echo "  ${yellow}~${normal} d: $dirFile "

        elif [[ -f $dirFile ]]; then
          echo "${red}dir to be linked: $dirFile is a already file${normal} you'll need to remove this manually"

        elif [[  -d "$HOME/$dirFile" ]]; then
          mapFiles "$dirFile"

        else
          mkdir "$HOME/$dirFile"  || exit 1
          echo "$HOME/$dirFile" >> ~/.teardown_list.txt
          mapFiles "$dirFile"

        fi

      elif [[ -f $dirFile ]]; then
        if [[  -L "$HOME/$dirFile" ]]; then
          echo "  ${yellow}~${normal} f: $dirFile "

        elif [[  -f "$HOME/$dirFile" ]]; then
          echo "  ${red}✘${normal} f: $dirFile already exists, you'll need to remove this manually"

        else
          ln -s "$configs/$dirFile" "$HOME/$dirFile" || exit 1
          echo "  ${green}✓${normal} f: $dirFile "
          echo "$HOME/$dirFile" >> ~/.teardown_list.txt

        fi
      else
        echo "$dirFile is not valid"
        exit 1
      fi
    fi
  done
}

mapFiles "."

sort -ro ~/.teardown_list.txt ~/.teardown_list.txt
