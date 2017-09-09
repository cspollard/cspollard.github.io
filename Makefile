all: context.html index.html

context.html: pandoc.css HEPExample.lhs
	pandoc -s --css pandoc.css -f markdown+lhs -t html5+lhs HEPExample.lhs > context.html


index.html: pandoc.css index.md
	pandoc -s --css pandoc.css -f markdown+lhs -t html5+lhs index.md > index.html
