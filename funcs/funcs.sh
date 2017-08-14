function start_sky_pages_with_params() {
  #---------------------------------------------#
  # IMPORTANT: GRABS SKYPORT ENDPOINTS
  # - YOU MAY ALREADY HAVE THIS SO CHANGE NAME
  # -------------------------------------------#
 
  if [ -r ~/.sky_private ]
  then
    source ~/.sky_private
  fi

  CHECKS=true
  DEBUG=false
  HELP=false
  ENV='d'

  PAGES_DOT_ENV=''

  HELP_TEXT='
  Runs sky-pages and sets .env with appropriate graphql endpoint

  Usage: spages [-flag]

  Flags
  -d  sets env to dev / F02
  -s  sets env to stage / E05
  -l  sets env localhost:2000
  -h  prints out help info
  '

  #---------------------------------------------#
  # GET ARGUMENTS
  # -------------------------------------------#

  # gets the flags and args from the bash command
  # add the new graphql address from private file 
  while getopts dslhx option
  do
    case "${option}" in
      d) ENV='d';; 
      s) ENV='s';; 
      l) ENV='l';; 
      x) DEBUG=true;;
      h) HELP=true;;
    esac
  done

  if [ ! -d $SKY_SERVICE_FOLDER ]; then
    echo ' you need to set up a SKY_SERVICE_FOLDER in your zshrc file pointing at the dir with your pages instalation '
    CHECKS=false
  fi

  if [ ! -d $SKY_SERVICE_FOLDER'/sky-pages' ]; then
    echo ' you dont have a sky-pages isntalltion at location '$SKY_SERVICE_FOLDER' '
    CHECKS=false
  fi

  if [ ! -f $SKY_SERVICE_FOLDER'/sky-pages/.env' ]; then
    echo 'pages .env file missing'
    CHECKS=false
  fi

  if $HELP; then
    echo $HELP_TEXT
    CHECKS=false
  fi

  if $CHECKS; then
    PAGES_DOT_ENV=$SKY_SERVICE_FOLDER'/sky-pages/.env'

    # remove any old graphql address
    sed -i -- '/GRAPHQL_ENDPOINT.*/d' $PAGES_DOT_ENV 

    case $ENV in
        d) echo $SKYPORT_DEV >> $PAGES_DOT_ENV ;;
        s) echo $SKYPORT_STAGE >> $PAGES_DOT_ENV ;;
        l) echo $SKYPORT_LOCAL >> $PAGES_DOT_ENV ;;
    esac

   if $DEBUG; then
      echo '.env file now contains:'
      cat $PAGES_DOT_ENV
    else
      pages start dev
    fi

  fi
}
