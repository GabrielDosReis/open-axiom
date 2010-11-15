To try this out first change to the panserver directory and 
build the shared socklib library:

gcc -O2 -fpic -c socklib.c

and

gcc -shared -o socklib.so socklib.o


Then start OpenAxiom from the panserver directory and
issue the command

)read panserver.input

From Firefox enter the url

127.0.0.1:8085/your/path/to/openAxiom.xml

To quit enter )quit from the Firefox interface.

I've tried to make it so that panserver exits by typing in the
url '127.0.0.1:8085/quit' but although the spad panServer
function apparently ends the OpenAxiom commandline interface
hangs.  For the time being I just kill the process.

To enter commands in the Firefox interface use the key combination
'Shift-Enter'.  Left clicking in a cell will get a context menu.
