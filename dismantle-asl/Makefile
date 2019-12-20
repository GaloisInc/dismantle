.PHONY: default spec all clean realclean deepclean
default: all

ASL_PARSER = ../deps/arm-asl-parser
PARSED = ./data/Parsed

HS_SOURCES := $(shell find ./src ./tools -name '*.hs' -not -path '*/\.*') $(shell find . -name '*.cabal')

.PRECIOUS: ${PARSED}/%.sexpr ./data/%.asl ${ASL_PARSER}/asl/%.asl ${ASL_PARSER}/asl-parsed/%.sexpr

${PARSED}/%.sexpr: ${ASL_PARSER}/asl-parsed/%.sexpr ./data/%.asl
	cp $< $@

./data/%.asl: ${ASL_PARSER}/asl/%.asl
	cp $< $@

${PARSED}/extra_defs.sexpr: ./data/extra_defs.asl
	cd ${ASL_PARSER}/asl-parser-java && \
	./gradlew -q run --args="defs $(abspath $<)" > $(abspath $@) && \
	[[ -s $(abspath $@) ]] || (rm -f $(abspath $@) && exit 1)

${ASL_PARSER}/asl/%.asl:
	$(MAKE) --directory=${ASL_PARSER} ./asl/$(@F)

${ASL_PARSER}/asl-parsed/%.sexpr:
	$(MAKE) --directory=${ASL_PARSER} ./asl-parsed/$(@F)

SPEC_FILES = arm_defs.sexpr arm_instrs.sexpr support.sexpr arm_regs.sexpr extra_defs.sexpr
SOURCE_FILES = $(SPEC_FILES:%.sexpr=${PARSED}/%.sexpr)

spec: ${SOURCE_FILES}

./output/formulas.what4: spec ${HS_SOURCES}
	cabal v2-build dismantle-asl-genarm
	cabal v2-run dismantle-asl-genarm -- --output-formulas="$@" --asl-spec="${PARSED}/" --check-serialization

all: ./output/formulas.what4

clean:
	rm -f ./output/formulas.what4

realclean: clean
	mv ./data/extra_defs.asl . ; rm -rf ./data/*.asl ; mv ./extra_defs.asl ./data/
	rm -f ./data/Parsed/*

deepclean: realclean
	$(MAKE) --directory=${ASL_PARSER} realclean
