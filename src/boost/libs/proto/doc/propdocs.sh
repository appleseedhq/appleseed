#!/bin/sh

rsync --delete --rsh=ssh --recursive html/* eric_niebler,boost-sandbox@web.sourceforge.net:/home/groups/b/bo/boost-sandbox/htdocs/libs/proto/doc/html
