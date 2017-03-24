
STATIC_FILES = index.html editor.css
ELM_MAIN = src/Editor.elm
ELM_FILES = $(wildcard src/*.elm)

PUBLISH_URL = danka.ii.fmph.uniba.sk:/home/webmaster/dai/courses/lpi/tbl/

SRC_DIR = src
OUT_DIR = build
STATIC_OUT = $(foreach f, $(STATIC_FILES), build/$(f))
ELM_OUT = $(OUT_DIR)/$(basename $(notdir $(ELM_MAIN))).js



build: $(STATIC_OUT) $(ELM_OUT)
publish: build
	rsync -av $(OUT_DIR)/ $(PUBLISH_URL)
clean:
	rm -r $(OUT_DIR)
.PHONY: build publish clean


$(OUT_DIR)/index.html: $(SRC_DIR)/index.html
	mkdir -p $(OUT_DIR)
	sed -e 's,src="/_compile[^"]*",src="$(notdir $(ELM_OUT))",' $< >$@

$(ELM_OUT): $(wildcard $(SRC_DIR)/*.elm)
	mkdir -p $(OUT_DIR)
	elm-make --yes $(ELM_MAIN)  --output=$@

$(OUT_DIR)/%: $(SRC_DIR)/$*
	mkdir -p $(OUT_DIR)
	cp -av $(SRC_DIR)/$* $@
