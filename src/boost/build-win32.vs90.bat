@echo off

bjam.exe -q --prefix=..\..\build\win32.vs90\boost --build-dir=..\..\build\win32.vs90\boost toolset=msvc-9.0 variant=debug link=static threading=multi runtime-link=shared define=_CRT_SECURE_NO_DEPRECATE define=_HAS_ITERATOR_DEBUGGING=0 install

bjam.exe -q --prefix=..\..\build\win32.vs90\boost --build-dir=..\..\build\win32.vs90\boost toolset=msvc-9.0 variant=release link=static threading=multi runtime-link=shared define=_CRT_SECURE_NO_DEPRECATE define=_SECURE_SCL=0 install
