index.html: pandoc.css HEPExample.md
	pandoc -s --css pandoc.css -f markdown+lhs -t html5+lhs HEPExample.md > index.html
