#!/bin/sh

set -e

[ -z "${TRAVIS_DEPLOY_KEY}" ] && exit 0
[ "${TRAVIS_BRANCH}" != "master" ] && exit 0

git config --global user.email "james.hanley@mcgill.ca"
git config --global user.name "JamesHanley"

git clone -b gh-pages https://${TRAVIS_DEPLOY_KEY}@github.com/${TRAVIS_REPO_SLUG}.git book-output
cd book-output
cp -r ../_book/* ./
git add --all *
git commit -m "Update the book" || true
git push -q origin gh-pages
