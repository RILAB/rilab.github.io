#! /bin/zsh

cd markdown
cat <(cat construction.md) <(date) > index.md

for i in *.md; 
do
	pandoc -S -s -c ../pandoc.css -H ../header.html -o ../${i//md/html} $i	
	#pandoc -S -s -c ../pygment_trac.css -c ../styles.css -o ../${i//md/html} $i	
done

git commit -am "update"; 
git push
