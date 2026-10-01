#############################################################################
##
##  makedoc.g
##
##  Builds the package documentation with AutoDoc/GAPDoc.
##
#############################################################################

LoadPackage("AutoDoc");

# Run this from the package's root directory: gap makedoc.g
AutoDoc(rec(
    autodoc := true,
    gapdoc := true,
    extract_examples := true,
    scaffold := rec(
        includes := [
            "ace.xml",
            "install.xml",
            "basics.xml",
            "options.xml",
            "strategies.xml",
            "interact.xml",
            "messages.xml",
            "examples.xml",
            "moreexamples.xml",
            "otheroptions.xml"
        ],
        entities := rec(
            ACE := "<Package>ACE</Package>",
        ),
        bib := "acebib.xml",
    ),
));

QuitGap();
