all:
	bison -d JBisonV3.y
	flex JFlexV2.l
	cc lex.yy.c JBisonV3.tab.c -o JavaValidateV3.out

debug:
	bison -d -v JBisonV3.y --debug
	flex JFlexV2.l
	cc lex.yy.c JBisonV3.tab.c -o JavaValidateV3.out

run:
	./JavaValidate3.out < test.z
