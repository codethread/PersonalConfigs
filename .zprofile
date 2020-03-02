if [ ! -n "$SOURCED_PROFILE" ]; then
  # dirty hack as this is getting annoying now
  source "$HOME/.profile"
fi

#------------------------------------------
#--- Sky Stuff
#-----------------------------------------
if [[ $(whoami) =~ 'adh23' ]]; then
  if [ -d "$HOME/confluent/bin" ] ; then
    PATH="$HOME/confluent/bin:$PATH"
  fi
fi

# export TERM=xterm-256color-italic
