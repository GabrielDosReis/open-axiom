tm_openaxiom - TeXmacs OpenAxiom Interface Program

  Version 0.0.1
  3 July 2009

Original Version by: Bill Page
Modified by: Alfredo Portes

Compile this program within the MSYS environment on Windows using
the command:

  gcc tm_openaxiom.c texbreaker.c -o tm_openaxiom

Copy the executable file 'tm_openaxiom.exe' to the OPENAXIOM bin directory:

  ... /installation-directory/bin

Default options can be specified by the environment variable:

  export TM_OPENAXIOM='break 1, over 1, cdot 1, space 0, big( 1, width 4.500'

These can be overridden by on the command line:

  tm_openaxiom 'break 1, over 1, cdot 1, space 0, big( 1, width 4.500'

and can be modified dynamically via the simulated OpenAxiom command:

  )set output texmacs break 1, over 1, cdot 1, space 0, big( 1, width 4.500

You may use 1 yes or on and 0 no or off.

  break <on>|<off>      line-break algorithm
  over <on>|<off>       convert 2-d \over to 1-d /
  cdot <on>|<off>       convert \cdot to \ (space)
  space <on>|<off>      convert \ (space) to \,
  big( <on>|<off>       convert \left( \right to ( )
  width <9.99>          line width in inches

I have also included an updated style file for OPENAXIOM

  openaxiom.ts

which corrects the positioning of the OpenAxiom Type: clause.

After installing TeXmacs for Windows, copy this file into the
TeXmacs directory:

  c:\Program Files\wintexmacs\texmacs\plugins\openaxiom\packages\session

or the equivalent location for the Cygwin version of TeXmacs.

Questions? bill.page1@sympatico.ca
	     doyenatccy@gmail.com