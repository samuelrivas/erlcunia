#!/bin/sh

# Copyright 2006, 2007, 2010 Samuel Rivas <samuelrivas@gmail.com>
#
# This file is part of Erlcunia.
#
# Erlcunia is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# Erlcunia is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# Erlcunia.  If not, see <http://www.gnu.org/licenses/>.

set -e

if [ $# -ne 1 ]; then
    echo
    echo "Usage $0 <version>"
    echo
    exit 1
fi

# Check that we are in the right branch
BRANCH=`git branch | grep ^*  | cut -d ' ' -f 2`

if [ ${BRANCH} != "master" ]; then
    echo
    echo "You're not in a production branch. Checkout master and be sure you"
    echo "merge the changes you want to publish"
    echo
    exit 1
fi

# Check that we are in a clean repository
STATUS=`git status --porcelain`
if [ ! -z "${STATUS}" ]; then
    echo
    echo "There are local changes, commit them before tagging"
    echo
    exit 1
fi

# Put the version number in the app file and sinan config
VERSION=`echo "$1" | sed -e "s/v\(.*\)/\1/g"`
APP_FILE=ebin/erlcunia.app
SINAN_CFG=sinan.cfg

sed -i "s/{vsn,.*}/{vsn, \"${VERSION}\"}/" ${APP_FILE}
sed -i "s/vsn.*$/vsn : \"${VERSION}\"/" ${SINAN_CFG}

# Commit and tag
echo
echo " * Creating a commit for version ${VERSION}"
git commit -a -m "TAG version ${VERSION}" > /dev/null
echo " * Creating tag ${VERSION}"
git tag ${VERSION} -m "Released version ${VERSION}"

# Revert to development version
echo " * Resetting ${APP_FILE} and ${SINAN_CFG} and commiting them after ${VERSION} tag"
git checkout HEAD^ ${APP_FILE}
git checkout HEAD^ ${SINAN_CFG}
git commit -a -m "REVERT revert versioned files to development version" > /dev/null

echo
echo "Success!"
echo "Remember to push this tag to canonical"
echo
