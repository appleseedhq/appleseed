//Maya ASCII 2011 scene
//Name: open_me_to_install.ma
//Last modified: Tue, Jul 10, 2012 05:18:34 PM
//Codeset: UTF-8
requires maya "2011";
currentUnit -l centimeter -a degree -t film;
fileInfo "application" "maya";
fileInfo "product" "Maya 2011";
fileInfo "version" "2011 Hotfix 3 x64";
fileInfo "cutIdentifier" "201007130017-777584";
fileInfo "osv" "Mac OS X 10.7.4";

createNode script -n "install";
	setAttr ".b" -type "string" (
		"import maya.mel as mel\nimport maya.cmds as cmds\nimport os\nimport sys\nuser_setup_file = None\nscene_name = cmds.file(sn=True, q=True)\nscene_dir = os.path.split(scene_name)[0]\nsys.path.append(scene_dir)\nimport install_helper\ninstall_helper.install_mayaseed()");
	setAttr ".st" 1;
	setAttr ".stp" 1;

// End of open_me_to_install.ma
