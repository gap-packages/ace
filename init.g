#############################################################################
##
#W    init.g               share package 'ace'               Alexander Hulpke
##
##    @(#)$Id$
##

# announce the package version and test for the existence of the binary
DeclarePackage("ace","1.0",
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

# install the documentation
DeclarePackageAutoDocumentation( "ace", "doc" );

# read the actual code.
ReadPkg( "ace", "gap/ace.g");
