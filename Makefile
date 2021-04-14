
STATIC_FILES = index.html static/main.css static/img/favicon.ico
ELM_MAIN = src/Editor.elm
ELM_FILES = $(wildcard src/*.elm)

PUBLISH_URL = dai.fmph.uniba.sk:/home/webmaster/dai/courses/lpi/folTableauEditor


SRC_DIR = src
OUT_DIR = build
STATIC_OUT = $(foreach f, $(STATIC_FILES), build/$(f))
ELM_OUT = $(OUT_DIR)/$(basename $(notdir $(ELM_MAIN))).js



build: $(STATIC_OUT) $(ELM_OUT)
publish: build
	rsync -av $(OUT_DIR)/ $(PUBLISH_URL)
clean:
	-rm -r $(OUT_DIR)
live:
	elm-live --dir src -- $(ELM_MAIN) --output=$(basename $(ELM_MAIN)).js
.PHONY: build publish clean live


$(OUT_DIR)/index.html: $(SRC_DIR)/index.html $(OUT_DIR)/static/main.css $(OUT_DIR)/Editor.js
	mkdir -p $(OUT_DIR)
	sed -e 's,src="[^"]*",src="$(notdir $(ELM_OUT))?'"$$(sha1sum $(ELM_OUT) | grep -o '^[0-9a-f]\+')"'",; s,href="static/main\.css",href="static/main.css?'"$$(sha1sum $(OUT_DIR)/static/main.css | grep -o '^[0-9a-f]\+')"'",' $< >$@

$(ELM_OUT): elm.json $(wildcard $(SRC_DIR)/*.elm) $(wildcard $(SRC_DIR)/*/*.elm) $(wildcard $(SRC_DIR)/*/*/*.elm) $(wildcard $(SRC_DIR)/*/*/*/*.elm)
	mkdir -p $(OUT_DIR)
	elm make $(ELM_MAIN) --output=$@

$(OUT_DIR)/%: $(SRC_DIR)/%
	mkdir -p $(dir $@)
	cp -av $(SRC_DIR)/$* $@


.PHONY: ghpublish commitGhPages
ghpublish: GITR=$(shell git log -1 --oneline)
ghpublish: GITB=$(shell git symbolic-ref HEAD | sed -e "s,refs/heads/,,")
ghpublish: build
	git checkout gh-pages
	$(MAKE) GITR="$(GITR)" commitGhPages ; r=$? ; git checkout $(GITB) ; exit $r
commitGhPages:
	git --work-tree=$(OUT_DIR) add -A
	git --work-tree=$(OUT_DIR) commit -e -m "Build $(GITR)"
	git reset --hard
	git push origin gh-pages
