Mayaseed docs
=============


Installation
-------------------

To install Mayaseed just open 'open_me_to_install.ma' and it should guide you through the steps to install. If this doesnt work or you'd prefer to install in manually you just need to edit your userSetup.mel file which should be here:

    Mac: /Users/<username>/Library/Preferences/Autodesk/maya/<maya version>/scripts

    Windows Vista and higher: C:\Users\<username>\Documents\maya\<maya version>\scripts (you may have My Documents instead of Documents)

    Windows XP and lower: C:\Documents and Settings\username\My Documents\maya\<maya version>\scripts

    Linux: /usr/aw/userconfig/maya/<maya version>/scripts

If you don't have one of these files thats ok, just create a plain text file with a .mel extension and edit that.

So copy the following lines to your useSetup.mel file


    // mayaseed  --------------
    
    $env_script_path = `getenv MAYA_SCRIPT_PATH`;
    $env_plugin_path = `getenv MAYA_PLUG_IN_PATH`;
    putenv MAYA_SCRIPT_PATH ($env_script_path + "<mayaseed_root>/scripts");
    $env_script_path = `getenv MAYA_SCRIPT_PATH`;
    putenv MAYA_SCRIPT_PATH ($env_script_path + "<mayaseed_root>/graphics");
    putenv MAYA_PLUG_IN_PATH ($env_plugin_path + "<mayaseed root>/plugins");


    // mayaseed  --------------


This just tells maya to search in your install directory for the Mayaseed plugin when it starts up. You will also want to replace any occurences of <mayaseed_root> with the path to your Mayaseed install directory, so for example I would replace the following line:

    putenv MAYA_SCRIPT_PATH ($env_script_path + ":<mayaseed root>/plugins");

with this:

    putenv MAYA_PLUG_IN_PATH ($env_plugin_path + ":/projects/mayaseed/plugins");


Finally you will want to start up Maya and enable the plugin, to do this choose **Window -> Settings Preferences -> Plugin manager** and load 'mayaseed.py', you will also want to click autoload so you don't have to do this step every time Maya loads.

That should be everything, if everything has gone to plan you should have a new menu called Mayaseed where you can create new ms_renderSettings nodes. This exporter is very much a work in progress and many improvements/features are planned for the future. Also please submit any bugs/feature requests as this is meant to be practical and usable software and I'd love to hear how it is being used.


