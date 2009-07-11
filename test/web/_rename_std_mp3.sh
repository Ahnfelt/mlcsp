#!/bin/bash
ls $1*mp3* | while read i; do mv -v $i ${i%\?*}; done

