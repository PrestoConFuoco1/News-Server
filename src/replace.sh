#!/bin/bash
sed -E 's/'$2'/'$3'/' $1 > $1'.bak1'
mv $1 $1'.bak2'
mv $1'.bak1' $1
