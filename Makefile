index.html: pandoc.css HEPExample.lhs
	pandoc -s --css pandoc.css -f markdown+lhs -t html5+lhs HEPExample.lhs > index.html
