To try this out start OpenAxiom from the panserver directory and
then issue the command

)read panserver.input

From Firefox enter the url

127.0.0.1:8085/your/path/to/openAxiom.xml

I've included the object and shared object files for
socklib.c but if you need to compile it yourself use
the commands

gcc -O2 -fpic -c testgcl.c

and

gcc -shared -Wl,-soname,testgcl,testgcl.so -o testgcl.so testgcl.o

I've tried to make it so that panserver exits by typing in the
url '127.0.0.1:8085/quit' but although the spad panServer
function apparently ends the OpenAxiom commandline interface
hangs.  For the time being I just kill the process.

To enter commands in the Firefox interface use the key combination
'Shift-Enter'.  Right clicking in a cell will get a context menu.
