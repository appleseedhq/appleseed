Installation
============

To install this plugin, copy the `basicenumerator` directory into the `studio/plugins/` directory of your appleseed installation.

It should look like this:

    <root appleseed directory>
        bin/
        ...
        studio/
            plugins/
                basicenumerator/
                    __init__.py


Usage
=====

To use this plugin, run the following commands in appleseed.studio's Python console:

    import basicenumerator
    basicenumerator.list_objects()
