#!/bin/bash
set -e # exit with nonzero exit code if anything fails

# Build documentation
cabal haddock

# Go to haddock output dir
cd dist/doc/html/hmacaroons

# Copy benchmark
cp benchmark.html dist/doc/html/hmacaroons

# Quiet the git init message, since it's not useful in the build log
git init > /dev/null 2>&1

# inside this git repo we'll pretend to be a new user
git config user.name "Travis CI"
git config user.email "julien.tanguy@jhome.fr"

# The first and only commit to this new Git repo contains all the
# files present with the commit message "Deploy to GitHub Pages".
git add .
# Silence the commit too
git commit -m "Deploy to GitHub Pages" > /dev/null 2>&1


# Force push from the current repo's master branch to the remote
# repo's gh-pages branch. (All previous history on the gh-pages branch
# will be lost, since we are overwriting it.) We redirect any output to
# /dev/null to hide any sensitive credential data that might otherwise be exposed.
echo "Pushing haddock to gh-pages"
git push --force --quiet "https://${GH_TOKEN}@${GH_REF}" master:gh-pages > /dev/null 2>&1

cd ~/build/jtanguy/hmacaroons
