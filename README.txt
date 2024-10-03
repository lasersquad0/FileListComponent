TFileList and TFileListView components for Delphi
-------------------------------------------------

CONTACTS
--------

 Andrey Romanchenko <lasersquad@gmail.com>

Components are based on Amedeo Lanza di Casalanza <amldc@tin.it> TFileList component for Delphi2


DESCRIPTION
-----------

FileList is a set of components based on TListView control and Explorer’s File Window features.
TFileView is a TListView that shows files and directories, links system image lists on creation 
and can accept files dropped from Windows' Explorer.

TFileList is a TFileView descendant which automatically populates files and directories from the 
specified directory. In addition to that it supports filtering by Mask. Thus specifying a path name 
and a file mask, the control will be automatically filled with the list of matching files, showing 
proper icons and a customizable set of file information.

Files in the original release:
FileList.pas	Delphi unit containing TFileList and TFileListView sources
FileList.dcr	Resource file with TFileList and TFileListView icons
FileList.rtf	Docs in RTF format
FListxxx.txt	This Readme file
FListxxx.inf	inf file for upload to DSP
MaskSearch.pas	Utility unit from the MstGrep.zip archive (by Marcus
		Stephany). This is included since i modified it, please check
		on DSP for original release of MstGrep.

USAGE
-----

Just drop a TFileList on a form, setup FileTypes, ViewColumns to required values.
In addition you can set SortColumn, RowSelect, DisplayDirectories, DisplayParentDir properties,
Also you may wan to to add a FilterComboBox (or/and a PathComboBox by Angus Johnson) and 
write a line of code to update the TFileList's Mask upon changing of 
FilterComboBox's one.

You may customize columns' captions and width programmatically using the SectColCaption and SetColWidth 
public procedures.

TFileList reacts to dbl mouse clicks when you navigating directories. Only browsing through directories 
is supported. To launch files or open documents from TListView you need to write a bit of your code. 

You can then add file masks using the AddMask procedure. Be warned that 
TFileList defaults to '*.*', so you have to set Mask to an empty string 
before adding masks or use Mask property to set a mask.

EXAMPLES
--------

For an exmple of use please look at the demo application in Demo folder.
It shows how to use both TFileList and TFileView components. 

Please see FileList.rtf for further information and license condition.


Suggestions, bug reports & comments to the author:

   Andrey Romanchenko
   <lasersquad@gmail.com>
