if [[ "$_" == "$0" ]]
then
  echo Apply '"source"' to me. >&2
  exit 1
fi

set -x

cp -rf /ssh/* $HOME/.ssh
chmod -R 600 $HOME/.ssh

eval $(ssh-agent)
set +x
echo -n 'SSH private key (id_rsa): '
read ssh_key
set -x
if [[ -z "$ssh_key" ]]
then
  ssh_key=id_rsa
fi
ssh-add "$HOME/.ssh/$ssh_key"
echo 'ssh-agent -k' >> $HOME/.bash_logout

eval "$(stack --bash-completion-script stack)"

alias appsync='rsync -a --delete --exclude=app/.stack-work /app/ /wd'

appsync

alias stack='stack --allow-different-user'

apt install -y make gcc libgmp3-dev libmysqlclient-dev pkgconf libpcre3-dev
pushd /wd/app
  stack setup --resolver=lts-5.4
  stack install --resolver=lts-5.4 yesod-bin
  stack build --only-dependencies
popd

source /app/docker/init-app-server-secret.bash

set +x
