
.PHONY: html clean

html:
	agdapandoc --html --css Agda.css Simple.lagda

clean:
	find . -name "*.agdai" -exec rm -f {} \; 
	rm -f html/*.html

