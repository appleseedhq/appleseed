#!/bin/bash

#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2020 Kevin Masson, The appleseedhq Organization
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#


set -e

THISDIR=`pwd`


#--------------------------------------------------------------------------------------------------
# Create a report.
#--------------------------------------------------------------------------------------------------

echo "travis_fold:start:report"
echo "Creating a report..."

touch build_report.txt

echo "web url=$TRAVIS_BUILD_WEB_URL" >> build_report.txt
echo "job url=$TRAVIS_JOB_WEB_URL" >> build_report.txt
echo "commit=$TRAVIS_COMMIT" >> build_report.txt
echo "commit url=https://github.com/appleseedhq/appleseed/commit/$TRAVIS_COMMIT" >> build_report.txt
echo "commit message=$TRAVIS_COMMIT_MESSAGE" >> build_report.txt
echo "job id=$TRAVIS_JOB_ID" >> build_report.txt
echo "job name=$TRAVIS_JOB_NAME" >> build_report.txt
echo "job number=$TRAVIS_JOB_NUMBER" >> build_report.txt

echo "travis_fold:end:report"


#--------------------------------------------------------------------------------------------------
# Deploy build.
#--------------------------------------------------------------------------------------------------

echo "travis_fold:start:deploy"
echo "Deploy travis build on the server..."

# Add server public key to known hosts.
echo $DEPLOY_SSH_KEY >> $HOME/.ssh/known_hosts

# Remove previous build from server.
export SSHPASS=$DEPLOY_PASSWORD
sshpass -e ssh $DEPLOY_USER@$DEPLOY_URL rm -rf $DEPLOY_FOLDER

# Send new build to the server.
sshpass -e rsync \
    --recursive \
    --archive \
    --compress \
    --stats \
    --no-perms --no-owner --no-group \
    build_report.txt \
    prebuilt-linux-deps \
    sandbox \
    scripts \
    $DEPLOY_USER@$DEPLOY_URL:$DEPLOY_FOLDER

echo "travis_fold:end:deploy"


set +e
