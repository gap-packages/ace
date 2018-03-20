LoadPackage( "ace" );
dirs := DirectoriesPackageLibrary( "ace", "tst" );
TestDirectory(dirs, rec(exitGAP := true));
