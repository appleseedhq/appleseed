Mayaseed docs
=============


Plugin structure
----------------

You will find the following files and folders inside the Mayaseed **src** directory that make up the Mayaseed plugin.

+ graphics (directory)
 + mayaseed_graphic.png
+ INSTALL.txt
+ Mayaseed_Docs.html
+ Mayaseed_Docs.md
+ open_me_to_install.ma
+ plugins (directory)
 + mayaseed.py
+ README.txt
+ scripts
 + about.txt
 + AEms_environmentTemplate.mel
 + AEms_renderSettingsTemplate.mel
 + mayaseed.ui
 + ms_commands.py
 + ms_export.py
 + ms_menu.py
 
###graphics (directory)###

The **graphics** directory contains any graphics used in the ui. 


###INSTALL.txt##

This file contains instructions on how to install Mayaseed, the instructions are mirrored in the Mayaseed docs but are included here just in case.


###Mayaseed\_Docs.md & Mayaseed_Docs.html###

**Mayaseed\_Docs.html** is generated from the **Mayaseed\_Docs.md** file. The .md file is a plain text file formatted using the markdown language.


###open\_me\_to\_install.ma###

**open\_me\_to\_install.ma** is a regular maya file that contains a python script node that executes on opening. The python script adds the Mayaseed plugins and scripts directory to the maya PATH so that maya can find the source files.


###plugins (directory)###

This directory contains the main plugin python file meaning it contains a single file that defines the Mayaseed nodes.


###mayaseed.py###

This file contains the code that defines the ms_renderSettings node and the ms_environment node.


###README.txt###

Simple text file containing information about Mayaseed.


###scripts (directory)###

The scripts directory contain all the functions, classes and attribute editor templates plus a few other things. 


###about.txt###

**about.txt** contains the text that the **About Mayaseed** dialogue displays.


###AEms\_environmentTemplate.mel###

This file describes how the ms_environment node is displayed in the attribute editor.


###AEms\_renderSettingsTemplate.mel###

This file describes how the ms_renderSettings node is displayed in the attribute editor.


###ms\_commands.py###

**ms\_commands.py** contains commands used in the Mayaseed menu and utility variables,functions and classes for the **ms\_export** module. 


###ms\_export.py###

**ms\_export.py** contains the **export()** function that does most of the hard work in translating the maya scene to the .appleseed format. Broadly speaking a **writeXML** object is handed down through each XML entity in the appleseed scene description writing to the file as it goes.


###ms\_menu###

This python script sets up the Mayaseed menu and is called by **mayaseed.py** in the **initializePlugin()** function. The **uninitializePlugin()** function also calls this module to delete the Mayaseed menu when the plugin is unloaded.


