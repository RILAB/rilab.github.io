#! /bin/zsh

cd markdown

for i in *.md; 
do
	pandoc -S -s -c ../pandoc.css -H ../header.html -o ../${i//md/html} $i	
	#pandoc -S -s -c ../pygment_trac.css -c ../styles.css -o ../${i//md/html} $i	
done


cd ../blogs

for i in *.md; 
do
	pandoc -S -s -c ../pandoc.css -H ../header.html -o ./${i//md/html} $i	
	#pandoc -S -s -c ../pygment_trac.css -c ../styles.css -o ./${i//md/html} $i	
	git add ./${i//md/html}
done

git commit -am "update"; 
git push
