#############################################################################
##
#W  init.g                   ACE Package                     Alexander Hulpke
#W                                                                Greg Gamble
##
#H  @(#)$Id$
##

##  Announce the package version and test for the existence of the binary

if not IsBound(GAPInfo) then
  BindGlobal("GAPInfo", 
    rec(DirectoriesTemporary := DIRECTORIES_TEMPORARY,
        PackagesInfo := rec(ace := [rec(Version :=
          Chomp(StringFile(Filename( DirectoriesPackageLibrary("ace", ""),
                                     "VERSION" ))))])
       ));
fi;
DeclarePackage( "ace", GAPInfo.PackagesInfo.ace[1].Version,
  function()
    # Check that the version no. of GAP is ok.
    if not(IsBound( CompareVersionNumbers ) and 
           CompareVersionNumbers( VERSION, "4.3" )) then
      Info(InfoWarning, 1,
           "Package ``ACE'' ACEPackageVersion(): requires at least GAP 4.3");
      return false;
    fi;
    # Test for existence of the compiled binary
    if Filename(DirectoriesPackagePrograms("ace"), "ace") = fail then
      Info(InfoWarning, 1,
           "Package ``ace'': The program `ace' is not compiled");
      return fail;
    fi;
    return true;
  end
);

##  Install the documentation
DeclarePackageAutoDocumentation( "ACE", "doc", "ACE",
                                 "Advanced Coset Enumerator" );

#############################################################################
##
#R  Read the actual code.
##
ReadPkg( "ace", "gap/general.gd" );
ReadPkg( "ace", "gap/interact.gd" );
ReadPkg( "ace", "gap/streams.gd" );
ReadPkg( "ace", "gap/options.gd" );
ReadPkg( "ace", "gap/ace.g" );

#E init.g . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
