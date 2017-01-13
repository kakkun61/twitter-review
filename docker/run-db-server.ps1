$root_password_secure = Read-Host -AsSecureString 'root password'
$root_password = [Runtime.InteropServices.Marshal]::PtrToStringAuto([Runtime.InteropServices.Marshal]::SecureStringToBSTR($root_password_secure))
$user_password_secure = Read-Host -AsSecureString 'user password'
$user_password = [Runtime.InteropServices.Marshal]::PtrToStringAuto([Runtime.InteropServices.Marshal]::SecureStringToBSTR($user_password_secure))

docker run `
  -e MYSQL_ROOT_PASSWORD="$root_password" `
  -e MYSQL_DATABASE='twitter-review' `
  -e MYSQL_USER='twitter-review' `
  -e MYSQL_PASSWORD="$user_password" `
  -d `
  --name twitter-review-db `
  mysql
