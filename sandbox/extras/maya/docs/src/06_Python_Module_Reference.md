Mayaseed docs
=============


Python Module reference
-----------------------

With mayaseed correctly installed you also have the following new python modules available to you.

+ ms_commands 
+ ms_export

###ms_commands module###

**ms_commands** is where all the common functions, classes and variables reside, this section describes them. 


####Variable: ms\_commands.MAYASEED_VERSION#####

This variable will return the current Mayaseed version.


####Variable: ms\_commands.MAYASEED_URL####

This variable will return the URL of the Mayaseed website.


####Variable: ms\_commands.APPLESEED_URL####

This variable will return the URL of the appleseed website.


####Variable: ms\_commands.ROOT_DIRECTORY####

This variable will return the URL of the Mayaseed install's root directory.


####Function: ms\_commands.addShadingAttribs()####

With an object or shader selected use this function to add custom shader translation attributes to a shader.


####Function: ms\_commands.removeShadingAttribs()####

With an object or shader selected use this function to remove custom shader translation attributes to a shader


####Class: ms\_commands.MsInfoDial()####

This function shows the Mayaseed info dialogue.


####Function: ms\_commands.normalizeRGB(Tuple[Float:R,Float:G,Float:B]: Color) Returns Tuple[Float:R,Float:G,Float:B,Float:M]####

Use this function normalises a 3 value [R,G,B] tuple and returns a normalised tuple with the RGB values normalised to a 0-1 range with a 4th multiplier value.


####Function: ms\_commands.convertConnectionToImage(String:shader, String:attribute, String:dest\_file, Int:resolution=1024) Returns String:dest\_file

This function will bake a given shading connection to an image file. 

#####Argument: String:shader#####

Name of the shader. e.g. "Lambert1"


#####Argument: String:attribute#####

The name of the attribute you want to bake. e.g. "Color"


#####Argument: String:dest\_file#####

The destination of the file you'd like to export


#####Argument: Int:resolution#####

The resolution of the image you would like to bake, images are always square.


####Function: convertTexToExr(String:file\_path, String:dest\_dir, Boolean: overwrite=True) Returns String:dest_file####

Use this function to convert an image to an .exr file using the **imf_copy** utility that ships with maya. The function returns a string containing the path to the destination file.


#####Argument: String:file\_path#####

File path of the image to be converted.


#####Argument: String:dest\_dir#####

The directory that you would like to save the converted image to. 

> Note: The converted image will have the same name as the source file but with the .exr extension.


#####Argument: Boolean:overwrite = True#####

By default Mayaseed will overwrite any images that have the same name as the output file, set this argument to false if you want to cancel image conversion for existing images


####Function: ms\_commands.shapeIsExportable(String:node\_name) Returns Boolean

This function will check to see if a maya shape node is exportable based on whether it is visible and is a valid shape node.


#####Argument: String:node_name#####

This string is the name of the node you wish to check


####Function: ms\_commands.hasShaderConnected(String:node\_name) Returns Boolean####

This function checks whether a shape node has a shader connected.


#####Argument: String:node\_name#####

This string is the name of the shape node you want to check.


###ms_export module###

This module contains the main bulk of the code that handles the export and essentially only has one useful function.


####Function: ms_export.export(String:render_settings_node)####

This function is the workhorse of Mayaseed and does all the work in translating, exporting and writing your scene to disk based on the settings from your render settings node.


#####Argument: String:String:render_settings_node#####

This string is the name of the render settings node that contains the settings of your export.