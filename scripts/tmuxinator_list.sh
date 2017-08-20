#!/bin/bash
function tmuxinator_list() {
  RETURN_DIR=$(pwd)
  cd ~/.tmuxinator
  ls
  cd $RETURN_DIR
}
