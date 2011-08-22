test-dir  = test
test-temp = ${test-dir}/temp
bootstrap = ${test-dir}/bootstrap.scm
record    = ${test-temp}/test.record
log       = ${test-temp}/test.log

test = @gosh -I. -l ${bootstrap} ${test-dir}

check:
	@echo "Cleaning log file ..."
	@rm -f ${record} ${log}
	@echo "------------------------------------------------------------------------"
	${test}/util-test.scm		 >> ${log}
	${test}/model/hcx-test.scm       >> ${log}
	${test}/model/rgb-test.scm       >> ${log}
	${test}/model/hsv-test.scm       >> ${log}
	${test}/model/hsl-test.scm       >> ${log}
	${test}/control/convert-test.scm >> ${log}
	${test}/control/harmonics-test.scm >> ${log}
	@echo "------------------------------------------------------------------------"
	@cat ${record}