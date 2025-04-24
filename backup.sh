#!/usr/bin/env bash

tar -zcvf "mnesia-$(date '+%Y-%m-%d').tar.gz" Mnesia.nonode@nohost && \
echo "mnesia OK" && \
tar -zcvf "images-$(date '+%Y-%m-%d').tar.gz" apps && \
echo "images OK"
