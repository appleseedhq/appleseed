@echo off
rem
rem Copyright by The HDF Group.
rem Copyright by the Board of Trustees of the University of Illinois.
rem All rights reserved.
rem
rem This file is part of HDF5.  The full HDF5 copyright notice, including
rem terms governing use, modification, and redistribution, is contained in
rem the files COPYING and Copyright.html.  COPYING can be found at the root
rem of the source code distribution tree; Copyright.html can be found at the
rem root level of an installed copy of the electronic HDF5 document set and
rem is linked from the top-level documents page.  It can also be found at
rem http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
rem access to either file, you may request a copy from help@hdfgroup.org.
rem
rem Tests for test_error and err_compat 
rem
rem    Created:  Scott Wegner, 8/16/07
rem    Modified: 

setlocal enabledelayedexpansion
pushd %~dp0

rem Determine backwards compatibility options enabled
rem set deprecated_symbols=%deprecated_symbols%

set /a nerrors=0
set verbose=yes

if not exist .\testfiles mkdir testfiles

goto main

rem Print a line-line message left justified in a field of 70 characters
rem beginning with the word "Testing".
rem
:testing
    set test_msg=Testing
    for %%a in (%*) do (
        if %%a neq PASSED (
        if %%a neq *FAILED* (
            set test_msg=!test_msg! %%a
        ) )
    )
    set test_msg=%test_msg%                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b


rem Run a test and print PASS or *FAIL*.  If a test fails then increment
rem the `nerrors' global variable and (if $verbose is set) display the
rem difference between the actual output and the expected output. The
rem expected output is given as the first argument to this function and
rem the actual output file is calculated by replacing the `.ddl' with
rem `.out'.  The actual output is not removed if $HDF5_NOCLEANUP has a
rem non-zero value.
rem
:test
    rem The test name
    set test_err=%1%3
    rem The path of the test binary
    set test_err_bin=%cd%\%test_err%\%2\%test_err%
    
    set expect1=%cd%\testfiles\%~n1_1
    set expect2=%cd%\testfiles\%~n1_2
    set expect1_parsed=%~n1_1.parsed
    set expect2_parsed=%~n1_2.parsed
    set actual=%~n1.out
    set actual_err=%~n1.err
    set actual_ext=%~n1.ext
    
    rem Run test
    (
        echo.#############################
        echo.Expected output for %1 
        echo.#############################
        %test_err_bin%
    ) >%actual% 2>%actual_err%
    rem Extract file name, line number, version and thread IDs because they may 
    rem be different
    
    rem Also filter out lines starting with *****, because Windows treats these
    rem as wildcards, and parses as filenames.  -SJW, 8/16/07
    type nul > %actual_ext%
    for /f "delims=" %%a in (%actual_err%) do (
        set line_tmp=%%a
        if not "!line_tmp:~0,9!"=="*********" (
            set line=
            set last_token=
            set skip=
            for %%b in (%%a) do (
                if not defined skip (
                    if "!last_token!"=="thread" (
                        set line=!line! ^(IDs^):
                        
                    ) else if "!last_token!"=="some" (
                        if "%%b"=="thread:" (
                            set line=!line! thread ^(IDs^):
                            set skip=yes
                        ) else (
                            set line=!line! some %%b
                        )
                        
                    ) else if "!last_token:~0,2!"=="#0" (
                        set line=!line! ^(file name^)
                        
                    ) else if "!last_token!"=="HDF5" (
                        rem Check if we wrap parenthesis around "version (number)"
                        set version_token=%%b
                        if "!version_token:~0,1!"=="(" (
                            set line=!line! ^(version ^(number^)^)
                        ) else (
                            set line=!line! version ^(number^).
                        )
                        
                    ) else if "!last_token!"=="line" (
                        set line=!line! ^(number^)
                        
                    ) else if not "%%b"=="some" (
                        set line=!line! %%b
                    )
                    set last_token=%%b
                )
            )
            echo.!line!>>%actual_ext%
        )
    )
    type %actual_ext% >> %actual%
    
    rem We parse through our expected output file in a similar way, because
    rem Windows will parse out commas and other special characters as well.
    rem    -SJW, 8/16/07
    for %%a in (expect1 expect2) do (
        type nul > !%%a_parsed!
        for /f "delims=" %%b in (!%%a!) do (
            set line_tmp=%%b
            if not "!line_tmp:~0,9!"=="*********" (
                set line=
                for %%c in (%%b) do (
                    set line=!line! %%c
                )
                echo.!line!>>!%%a_parsed!
            )
        )
    )
        
    fc /w %expect1_parsed% %actual% > nul
    if errorlevel 0 (
            call :testing PASSED %test_err%
    ) else (
        fc /w %expect2_parsed% %actual% > nul
        if errorlevel 0 (
            call :testing PASSED %test_err%
        ) else (
            call :testing *FAILED* %test_err%
            echo.    Expected result differs from actual result
            set /a nerrors=%nerrors%+1
            if "yes"=="%verbose%" fc %expect1_parsed% %actual%
        )
    )
    
    rem Clean up output file
    if not defined HDF5_NOCLEANUP (
        for %%a in (%actual% %actual_err% %actual_ext% %expect1_parsed% %expect2_parsed%) do del /f %%a
    )
    
    exit /b
    
    
rem Print a "SKIP" message
:skip
    call :testing -SKIP- %*
    
    exit /b
    
    
rem ##############################################################################
rem ##############################################################################
rem ###                       T H E   T E S T S                                ###
rem ##############################################################################
rem ##############################################################################

:main

    rem test for err_compat
    if "%deprecated_symbols%"=="yes" (
        call :skip err_compat %1 %2
    ) else (
        call :test err_compat %1 %2
    )

    rem test for error_test
    call :test error_test %1 %2

    if "%nerrors%"=="0" (
        echo.All Error API tests passed.
    ) else (
        echo.** FAILED Error API tests
	)
    
    popd
    endlocal & exit /b %nerrors%
    
