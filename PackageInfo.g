SetPackageInfo( rec(

PackageName := "ACE",
Subtitle    := "Advanced Coset Enumerator",
Version     := "5.7.0",
Date        := "10/04/2025", # dd/mm/yyyy format
License     := "MIT",

Persons := [
  rec(
    LastName      := "Gamble",
    FirstNames    := "Greg",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "Greg.Gamble@uwa.edu.au",
    WWWHome       := "http://staffhome.ecm.uwa.edu.au/~00021149",
    PostalAddress := Concatenation( [
                       "Greg Gamble\n",
                       "Department of Mathematics and Statistics\n",
                       "Curtin University\n",
                       "GPO Box U 1987\n",
                       "Perth WA 6845\n",
                       "Australia" ] ),
    Place         := "Perth",
    Institution   := "Curtin University"
  ),
  rec(
    LastName      := "Hulpke",
    FirstNames    := "Alexander",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "hulpke@math.colostate.edu",
    WWWHome       := "https://www.math.colostate.edu/~hulpke",
    PostalAddress := Concatenation( [
                       "Alexander Hulpke\n",
                       "Department of Mathematics\n",
                       "Colorado State University\n",
                       "Weber Building\n",
                       "Fort Collins, CO 80523\n",
                       "USA" ] ),
    Place         := "Fort Collins",
    Institution   := "Colorado State University"
  ),
  rec(
    LastName      := "Havas",
    FirstNames    := "George",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "havas@itee.uq.edu.au",
    WWWHome       := "http://staff.itee.uq.edu.au/havas",
    PostalAddress := Concatenation( [
                       "George Havas\n",
                       "Centre for Discrete Mathematics and Computing\n",
                       "Department of Information Technology ",
                       "and Electrical Engineering\n",
                       "The University of Queensland\n",
                       "St. Lucia 4072\n",
                       "Australia" ] ),
    Place         := "Brisbane",
    Institution   := "The University of Queensland"
  ),
  rec( 
    LastName      := "Ramsay",
    FirstNames    := "Colin",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "cram@itee.uq.edu.au",
    PostalAddress := Concatenation( [
                       "Colin Ramsay\n",
                       "Centre for Discrete Mathematics and Computing\n",
                       "Department of Information Technology ",
                       "and Electrical Engineering\n",
                       "The University of Queensland\n",
                       "St. Lucia 4072\n",
                       "Australia" ] ),
    Place         := "Brisbane",
    Institution   := "The University of Queensland"
  ),
  rec(
    LastName      := "Horn",
    FirstNames    := "Max",
    IsAuthor      := false,
    IsMaintainer  := true,
    Email         := "mhorn@rptu.de",
    WWWHome       := "https://www.quendi.de/math",
    PostalAddress := Concatenation(
                       "Fachbereich Mathematik\n",
                       "RPTU Kaiserslautern-Landau\n",
                       "Gottlieb-Daimler-Straße 48\n",
                       "67663 Kaiserslautern\n",
                       "Germany" ),
    Place         := "Kaiserslautern, Germany",
    Institution   := "RPTU Kaiserslautern-Landau"
  )
],

Status      := "accepted",
CommunicatedBy
            := "Joachim Neubüser (Aachen)",
AcceptDate  := "04/2001",

SourceRepository := rec(
  Type := "git",
  URL := "https://github.com/gap-packages/ace",
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome  := "https://gap-packages.github.io/ace",
README_URL      := Concatenation( ~.PackageWWWHome, "/README.md" ),
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "/PackageInfo.g" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                  "/releases/download/v", ~.Version,
                                  "/ace-", ~.Version ),
ArchiveFormats  := ".tar.gz",

AbstractHTML :=
  "The <span class=\"pkgname\">ACE</span> package provides both an \
   interactive and non-interactive interface with the Todd-Coxeter coset\
   enumeration functions of the ACE (Advanced Coset Enumerator) C program.",

PackageDoc  := rec(
  BookName  := "ACE",
  ArchiveURLSubset := ["doc", "htm"],
  HTMLStart := "htm/chapters.htm",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Advanced Coset Enumerator",
),

Dependencies := rec(
  GAP := ">= 4.7",
  NeededOtherPackages := [],
  SuggestedOtherPackages := [],
  ExternalConditions := []
),

AvailabilityTest :=
  function()
    # Test for existence of the compiled binary
    if Filename(DirectoriesPackagePrograms("ace"), "ace") = fail then
      LogPackageLoadingMessage( PACKAGE_WARNING,
          [ "The program `ace' is not compiled." ] );
      return false;
    fi;
    return true;
  end,

BannerString := Concatenation(
"---------------------------------------------------------------------------",
"\n",
"Loading    ", ~.PackageName, " (", ~.Subtitle, ") ", ~.Version, "\n",
"GAP code by ", ~.Persons[1].FirstNames, " ", ~.Persons[1].LastName,
      " <", ~.Persons[1].Email, "> (address for correspondence)\n",
"       ", ~.Persons[2].FirstNames, " ", ~.Persons[2].LastName,
      " (", ~.Persons[2].WWWHome, ")\n",
"           [uses ACE binary (C code program) version: 3.001]\n",
"C code by  ", ~.Persons[3].FirstNames, " ", ~.Persons[3].LastName,
      " (", ~.Persons[3].WWWHome, ")\n",
"           ", ~.Persons[4].FirstNames, " ", ~.Persons[4].LastName,
       " <", ~.Persons[4].Email, ">\n",
"Co-maintainer: ", ~.Persons[5].FirstNames, " ", ~.Persons[5].LastName,
       " <", ~.Persons[5].Email, ">\n\n",
"                 For help, type: ?ACE\n",
"---------------------------------------------------------------------------",
"\n" ),

TestFile := "tst/aceds.tst",

Keywords := [ "coset enumeration", "Felsch strategy", "HLT strategy",
              "coset table", "index", "maxcosets", "activecosets" ]

));
