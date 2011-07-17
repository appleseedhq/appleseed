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

def expect(expected, received):
    if expected != received:
        print("Unit test failed!")
        print("  Expected: {0}".format(expected))
        print("  Received: {0}".format(received))

def run_tests():
    expect("c:\\dir\\file.ext", make_relative_path("c:\\dir\\file.ext", ""))
    expect("/dir/file.ext", make_relative_path("/dir/file.ext", ""))
    expect("dir\\file.ext", make_relative_path("c:\\dir\\file.ext", "c:\\"))
    expect("dir\\file.ext", make_relative_path("/dir/file.ext", "/"))
    expect("dir\\file.ext", make_relative_path("c:\\dir\\file.ext", "c:/"))
    expect("dir\\file.ext", make_relative_path("/dir/file.ext", "\\"))
    expect("file.ext", make_relative_path("c:\\dir\\file.ext", "c:\\dir"))
    expect("file.ext", make_relative_path("/dir/file.ext", "/dir"))
    expect("file.ext", make_relative_path("c:\\dir\\file.ext", "c:\\dir\\"))
    expect("file.ext", make_relative_path("/dir/file.ext", "/dir/"))
    expect("c:\\dir\\file.ext", make_relative_path("c:\\dir\\file.ext", "c:\\rep"))
    expect("/dir/file.ext", make_relative_path("/dir/file.ext", "/rep"))

run_tests()
