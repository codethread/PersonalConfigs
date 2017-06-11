#!/bin/bash

# get get location of configs
configs=$(pwd)

declare -a array_of_files=(".vimrc" ".zshrc" "tmux.conf")

cd ~
if [ ! backups ]; then
	mkdir backups
fi

for file_name in "${array_of_files[@]}"
do
	echo 'func started with' $file_name
	cd ~

	if [ backups/$file_name ]; then
		rm ~/backups/$file_name
	fi

	mv $file_name backups
	
	if [ $file_name ]; then
		rm $file_name
	fi

	ln -s ${configs}/${file_name} ${file_name}
	echo 'finished' $file_name 
done

