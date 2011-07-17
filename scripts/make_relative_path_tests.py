import os.path

def make_relative_path(file_path, base):
    npath = os.path.normpath(os.path.normcase(file_path))
    nbase = os.path.normpath(os.path.normcase(base))
    if npath.startswith(nbase):
        result = npath[len(nbase):]
        if result.startswith("/") or result.startswith("\\"):
            result = result[1:]
        return result
    else:
        return file_path

print(make_relative_path("c:\\dir\\file.txt", ""))
print(make_relative_path("c:/dir/file.txt", ""))
print(make_relative_path("c:\\dir\\file.txt", "c:\\"))
print(make_relative_path("c:/dir/file.txt", "c:\\"))
print(make_relative_path("c:\\dir\\file.txt", "c:/dir"))
print(make_relative_path("c:/dir/file.txt", "c:\\dir"))
print(make_relative_path("c:\\dir\\file.txt", "c:/dir/"))
print(make_relative_path("c:/dir/file.txt", "c:\\dir\\"))
print(make_relative_path("c:\\dir\\file.txt", "c:/rep"))
print(make_relative_path("c:/dir/file.txt", "c:\\rep"))
