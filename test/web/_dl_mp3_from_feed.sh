#!/bin/bash
curl $1 | grep -o 'http.*mp3' > links.txt && wget -nc -i links.txt -P$2 && rm links.txt

