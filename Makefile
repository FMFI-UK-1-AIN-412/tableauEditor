index.html: $(wildcard src/*.elm)
	elm-make --yes  src/Editor.elm  --output=$@

publish: index.html
	rsync -av  index.html danka.ii.fmph.uniba.sk:/home/webmaster/dai/courses/lpi/tbl/
