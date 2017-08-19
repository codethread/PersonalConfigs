#!/bin/bash
function setup_scripts() {
  # get get location of configs
  configs=$(pwd)

  array_of_files=( .??* )

  cd ~
  if [ ! backups ]; then
    mkdir backups
  fi

  for file_name in "${array_of_files[@]}"
  do
    if	[[ $file_name =~ .sw.?$ ]] || [[ $file_name =~ ^.git$ ]]
    then
      echo '>< ' ${file_name}
    else
      cd ~

      if [ ~/backups/$file_name ]; then
        rm ~/backups/$file_name
      fi

      mv $file_name backups

      ln -s ${configs}/${file_name} ${file_name}
      echo '----->' $file_name 
    fi
  done
}
