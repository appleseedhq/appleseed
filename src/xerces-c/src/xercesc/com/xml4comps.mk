
xml4comps.dll: dlldata.obj xml4com_p.obj xml4com_i.obj
	link /dll /out:xml4comps.dll /def:xml4comps.def /entry:DllMain dlldata.obj xml4com_p.obj xml4com_i.obj \
		kernel32.lib rpcndr.lib rpcns4.lib rpcrt4.lib oleaut32.lib uuid.lib \

.c.obj:
	cl /c /Ox /DWIN32 /D_WIN32_WINNT=0x0400 /DREGISTER_PROXY_DLL \
		$<

clean:
	@del xml4comps.dll
	@del xml4comps.lib
	@del xml4comps.exp
	@del dlldata.obj
	@del xml4com_p.obj
	@del xml4com_i.obj
