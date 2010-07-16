#!/bin/bash
# Port is executed
root=$WORKING_DIRECTORY

function call()
{
  read REQUEST LOCATION PORT < receive
  if [[ -r ${root}/${LOCATION} ]]; then
    if [[ -d ${root}/${LOCATION} ]]; then
      cat plainheaders # it is a directory containing the file i read
      ls -l "${root}/${LOCATION}"
    else
      case "${LOCATION}" in *.txt)
        cat plainheaderfile # cat the file
      ;;
      *.html)
        cat htmlheaderfile # cat the file
      ;;
      *.png)
        cat pngheaderfile# cat the file
      ;;
      *)
        cat applicationosheaderfile # cat the file
      ;;
    esac
    cat "${root}/${LOCATION}"
  fi
else
  if [[ -e ${root}/${LOCATION} ]]; then
    cat 403forbidden # cat the file
  else # not found, send error page
    cat 404notfound # cat the file
  fi
fi
}

mkfifo receive
mkfifo send

echo listening on port $PORT

while true; do
cat send | netcat -l -p $PORT > receive &
serve > send
done

echo "cleaning up"
rm receive send