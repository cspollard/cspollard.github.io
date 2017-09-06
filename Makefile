index.html:
	pandoc -s --css pandoc.css -f markdown+lhs -t html5+lhs HEPExample.md > index.html
