#! /usr/bin/env bash

echo -n 'root password: '
read -s ROOT_PASSWORD
echo

echo -n 'user password: '
read -s USER_PASSWORD
echo

docker run\
  -e MYSQL_ROOT_PASSWORD="$ROOT_PASSWORD"\
  -e MYSQL_DATABASE='twitter-review'\
  -e MYSQL_USER='twitter-review'\
  -e MYSQL_PASSWORD="$USER_PASSWORD"\
  --name twitter-review-db\
  mysql
