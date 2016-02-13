# Run following.
# mysql -u root -e "SET @password = '${MYSQL_PASSWORD}'; SOURCE create-user.sql;"

SET @create_user = CONCAT("CREATE USER `twitter-review`@`localhost` IDENTIFIED BY '", @password, "'");
PREPARE stmt FROM @create_user; EXECUTE stmt; DEALLOCATE PREPARE stmt;
grant all on `twitter-review`.* to 'twitter-review'@'localhost';
