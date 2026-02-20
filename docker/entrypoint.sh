#!/bin/sh -l

export HOME=/root

cd $GITHUB_WORKSPACE
git config --global user.email "bmk@inbo.be"
git config --global user.name "INBO BMK"

if [ $GITHUB_REPOSITORY = 'inbo/flandersqmd' ] ; then
  echo 'Test new version of flandersqmd'
  apt-get update
  apt-get upgrade -y
  Rscript -e 'remotes::install_local(".", force = TRUE)'
  cd /
  git clone --single-branch --branch=$EXAMPLE_BRANCH --depth=1 https://oauth2:$GITHUB_TOKEN@github.com/inbo/flandersqmd-book check
  cd /check/source
  echo '\nRendering to quarto document...\n'
  Rscript --no-save --no-restore --no-init-file -e 'options(warn = 2); install.packages(checklist:::list_missing_packages(), quiet = TRUE)'
  quarto render
  if [ $? -ne 0 ]; then
    echo "\nRendering failed. Please check the error message above.\n";
    exit 1
  else 
    echo "\New version of flandersqmd was able to render the example.\n";
    exit 0;
  fi
fi

echo 'Test changes in source repository'
echo "Cloning repository $GITHUB_REPOSITORY with commit $GITHUB_SHA for event $GITHUB_EVENT_NAME"
cd /
mkdir check
cd check
git init
git remote add origin https://oauth2:$GITHUB_TOKEN@github.com/$GITHUB_REPOSITORY
git fetch --depth 1 origin $GITHUB_SHA
git checkout FETCH_HEAD
cd $INPUT_FOLDER

echo '\nRendering to quarto document...\n'
Rscript --no-save --no-restore --no-init-file -e 'options(warn = 2); install.packages(checklist:::list_missing_packages(), quiet = TRUE)'
quarto render
if [ $? -ne 0 ]; then
  echo "\nRendering failed. Please check the error message above.\n";
  exit 1
fi

if [ "$GITHUB_REF_NAME" != "main" ] ; then
    echo "\nNot on main branch, skipping update of gh-pages.\n";
    exit 0;
fi

cd /
git clone --depth 1 -b gh-pages https://oauth2:$GITHUB_TOKEN@github.com/$GITHUB_REPOSITORY ghpages
cd /ghpages
git rm -rf --quiet .
cp -R /check/output/. /ghpages/.
git add --all
git commit --amend -m "Automated update of gh-pages from flandersqmd"
git push --force --set-upstream origin gh-pages

if [ $? -ne 0 ]; then
    echo "\nUpdating failed. Please check the error message above.\n";
    exit 1
fi
