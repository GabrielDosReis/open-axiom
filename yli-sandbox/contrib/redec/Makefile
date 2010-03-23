OA := /users/yli/software/OA-latest/oa/bin/open-axiom --compile

all: help analysis tpchk extract checkasm preproc

help: synhelp.spad
	${OA} synhelp.spad

analysis: analysis.spad
	${OA} analysis.spad

tpchk: typchker.spad
	${OA} typchker.spad

extract: extractreduce.spad
	 ${OA} extractreduce.spad

preproc: preprocess.spad
	 ${OA} preprocess.spad

checkasm: checkassumption.spad
	  ${OA} checkassumption.spad

opcats: operatorcats.spad
	${OA} operatorcats.spad

clean:
	@rm -rf *.NRLIB *.erlib libdb.text *~ *.tex