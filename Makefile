DIST_DIR = $(shell stack path --dist-dir)
BIN_DIR = $(DIST_DIR)/build-weather-exe
SOURCES = $(shell find src -name '*.hs')
DATA_FILE = data/example.psv

.PHONY : all
all : run-prod

.PHONY : prof
prof : weather-exe.pdf
	evince $<

.PHONY : doc
doc : $(SOURCES)
	stack haddock

.PHONY : data
data : $(DATA_FILE)

.PHONY : run-prod
run-prod : $(BIN_DIR)/weather-exe $(DATA_FILE)
	time -v stack exec weather-exe -- proc $(DATA_FILE)

.PHONY : clean
clean :
	stack clean
	rm -f weather-exe.hp weather-exe.ps weather-exe.pdf

## BUILD EXECUTABLE
$(BIN_DIR)/weather-exe : $(SOURCES) app/Main.hs
	stack build $(BUILD_FLAGS)

## PROFILING
weather-exe.hp : BUILD_FLAGS = --profile
weather-exe.hp : $(BIN_DIR)/weather-exe $(DATA_FILE)
	time -v stack exec weather-exe -- proc $(DATA_FILE) +RTS -hr

weather-exe.ps : weather-exe.hp
	stack exec hp2ps -- -c $< > $@

weather-exe.pdf : weather-exe.ps
	ps2pdf $<

## GENERATE DATA
$(DATA_FILE) :
	time -v stack exec weather-exe -- gen $@
