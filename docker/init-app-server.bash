set -x

cp -rf /ssh/* $HOME/.ssh
chmod -R 600 $HOME/.ssh

eval $(ssh-agent)
ssh-add $HOME/.ssh/id_rsa
echo 'ssh-agent -k' >> $HOME/.bash_logout

eval "$(stack --bash-completion-script stack)"

alias appsync='rsync -a --delete --exclude=app/.stack-work /app/ /wd'

appsync

alias stack='stack --allow-different-user'

apt install -y make gcc libgmp3-dev libmysqlclient-dev pkgconf libpcre3-dev
pushd /wd/app
  stack setup --resolver=lts-5.4
  stack install --resolver=lts-5.4 --ghc-options -j yesod-bin
  stack build --only-dependencies --ghc-options -j
popd

source /app/docker/init-app-server-secret.bash

set +x
