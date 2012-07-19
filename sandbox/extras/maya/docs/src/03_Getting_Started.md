Mayaseed docs
=============


Getting started
---------------

###The Mayaseed menu###

The Mayaseed menu will appear when the plugin is correctly installed, if you can't see it make sure the plugin installed correctly and is enabled.


###The Render Settings node###

The ms\_renderSettings (render settings) node is the workhorse of Mayaseed, it contains most of the settings to control your export and is also one of the places where you can launch the export. To export your scene you only need one ms\_renderSettings node but it is also possible to have many per scene, this can be useful for making proxy resolution renders or exporting for different render passes. 

>Note: The ms_renderSettings node's attributes are organised in a way that mirrors the internal file structure, so if an attribute seems like its in a strange place there is a good reason. By using Mayaseed you are also learning about appleseed at the same time.

###Your first export###

To export a scene you first need to create a render settings node, to do this choose **Mayaseed -> create render settings node**.

Now in the attribute editor you can Set up your export

>Note: if you deselect a render settings node then you can easily re select it by choosing Mayaseed -> Select render setting node

To set up your first export you will only really need to do two things; set up your output destination and choose your render camera. 

First you will need to set your output directory, and then choose a name for your output file, both attributes are in the export settings section of the renderSettings node. 

Next you need to choose your render camera in the output settings.

That should be all you need for your first export, not all you need to do is click the blue **Export** button at the top of your Render Settings node's attribute editor.

