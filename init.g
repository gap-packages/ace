#############################################################################
##
#W  init.g                ACE Share Package                       Greg Gamble
##
##  The original init,g was written by Alexander Hulpke and modified  to read
##  example.gd by Greg Gamble.
##    
#H  @(#)$Id$
##

##  Announce the package version and test for the existence of the binary
DeclarePackage("ace","3.0",
  function()
  local path,file,line,a;
    # test for existence of the compiled binary
    path:=DirectoriesPackagePrograms("ace");
    file:=Filename(path,"ace");
    if file=fail then
      Info(InfoWarning,1,
        "Package ``ace'': The program `ace' is not compiled");
    fi;
    return file<>fail;
  end);

##  Install the documentation
DeclarePackageAutoDocumentation( "ace", "doc" );

#############################################################################
##
#R  Read the actual code.
##
ReadPkg( "ace", "gap/ace.g" );
ReadPkg( "ace", "gap/example.gd" );

#E init.g . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
