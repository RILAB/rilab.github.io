#! /bin/zsh

cat <(cat construction.md) <(date) > index.md

for i in *.md; 
do
	pandoc -o ../${i//md/html} $i	
done
