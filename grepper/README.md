grepper - Search multiple words in files
---------------------------

Syntax: `grepper -i <first> <second> <third> ...`

*   -i switch is for case insensitive search

Search recursively for multiple words found in files using grep, so for example I need to find a file with the words 'researched' 'found' 'belonging', I would call this like:

`./grepper researched found belonging`

This will list all the files recursively in the current directory with the three words researched, found and belonging.
