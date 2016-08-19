#!/bin/sh

if [ $# -eq 0 ]; then
  echo "Yo!"
  exit
fi

groupadd -o -g $BUILDER_GID $BUILDER_GROUP 2> /dev/null
useradd -o -m -g $BUILDER_GID -u $BUILDER_UID $BUILDER_USER 2> /dev/null
#chown -R $BUILDER_UID:$BUILDER_GID $HOME

# Run the command as the specified user/group.
exec chpst -u :$BUILDER_UID:$BUILDER_GID "$@"
