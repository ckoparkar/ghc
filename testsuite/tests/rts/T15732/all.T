test('T15732_01', normal, run_command, ['$MAKE -s --no-print-directory T15732_01'])
test('T15732_02', normal, run_command, ['$MAKE -s --no-print-directory T15732_02'])
test('T15732_03a', [extra_files([ 'T15732_03.hs', 'T15732_03.response'
                                , 'T15732_03_01.response' ])],
     run_command, ['$MAKE -s --no-print-directory T15732_03a'])
test('T15732_03b', [extra_files([ 'T15732_03.hs', 'T15732_03.response'
                                , 'T15732_03_01.response' ])],
     run_command, ['$MAKE -s --no-print-directory T15732_03b'])
test('T15732_04a', [extra_files(['T15732_04.hs', 'T15732_04.response'])],
     run_command, ['$MAKE -s --no-print-directory T15732_04a'])
test('T15732_04b', [extra_files(['T15732_04.hs', 'T15732_04.response'])],
     run_command, ['$MAKE -s --no-print-directory T15732_04b'])
test('T15732_05a', [extra_files(['T15732_05.hs', 'T15732_05.response'])],
      run_command, ['$MAKE -s --no-print-directory T15732_05a'])
test('T15732_05b', [extra_files(['T15732_05.hs', 'T15732_05.response'])],
      run_command, ['$MAKE -s --no-print-directory T15732_05b'])
test('T15732_06', normal, run_command, ['$MAKE -s --no-print-directory T15732_06'])
test('T15732_07', normal, run_command, ['$MAKE -s --no-print-directory T15732_07'])
test('T15732_08a', [extra_files(['T15732_08.hs', 'T15732_08.response'])],
      run_command, ['$MAKE -s --no-print-directory T15732_08a'])
test('T15732_08b', [extra_files(['T15732_08.hs', 'T15732_08.response'])],
      run_command, ['$MAKE -s --no-print-directory T15732_08b'])
