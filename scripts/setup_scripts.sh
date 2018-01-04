#!/bin/bash
function setup_scripts() {
  # get get location of configs
  configs=$(pwd)

  array_of_files=( .??* )

  cd ~
  if [ ! .dot_env_backups ]; then
    mkdir .dot_env_backups
  fi

  for file_name in "${array_of_files[@]}"
  do
    if	[[ $file_name =~ .sw.?$ ]] || [[ $file_name =~ ^.git$ ]]
    then
      echo '>< ' ${file_name}
    else
      cd ~

      if [ ~/.dot_env_backups/$file_name ]; then
        rm ~/.dot_env_backups/$file_name
      fi

      mv $file_name .dot_env_backups

      ln -s ${configs}/${file_name} ${file_name}
      echo '----->' $file_name 
    fi
  done

  [ ! -d ~/.vim/colors ] && mkdir ~/.vim/colors
  ln -s ~/PersonalConfigs/colors/tenderAdam.vim ~/.vim/colors
  ln -s ~/PersonalConfigs/colors/airline/tenderAdam.vim ~/.vim/pack/my-packages/start/vim-airline-themes/autoload/airline/themes/

}
