#############################################################################
##
#W  init.g                ACE Share Package                  Alexander Hulpke
#W                                                                Greg Gamble
##
#H  @(#)$Id$
##

##  Announce the package version and test for the existence of the binary
DeclarePackage( "ace", "3.001",
  function()
  local file;
    # Check that the version no. of GAP is ok.
    if not(IsBound( CompareVersionNumbers ) and 
           CompareVersionNumbers( VERSION, "4.2" )) then
      Info(InfoWarning, 1,
           "Package ``ace'': Sorry! ACE needs at least GAP 4.2");
      return false;
    fi;
    # Test for existence of the compiled binary
    file := Filename(DirectoriesPackagePrograms("ace"), "ace");
    if file = fail then
      Info(InfoWarning, 1,
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
ReadPkg( "ace", "gap/options.gd" );
ReadPkg( "ace", "gap/streams.gd" );
ReadPkg( "ace", "gap/general.gd" );
ReadPkg( "ace", "gap/interact.gd" );
ReadPkg( "ace", "gap/ace.g" );

#E init.g . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
