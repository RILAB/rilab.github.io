#! /bin/zsh

cd markdown
cat <(cat construction.md) <(date) > index.md

for i in *.md; 
do
	pandoc -S -s -c ../pandoc.css -o ../${i//md/html} $i	
done

git commit -am "update"; 
git push
