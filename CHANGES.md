In this file we record the changes since the 3.xxx versions of the ACE
package. Also look at gap/CHANGES  for  a  history  of  the  pre-3.xxx
development.

## Version 5.7.0 (2025-04-10)
  * Deprecate `InfoACELevel` and `SetInfoACELevel` (instead one
    may use `InfoACE` together with `SetInfoLevel`)
  * Various janitorial changes

## Version 5.6.2 (2023-01-03)
  * Enhance the `configure` script to accept `--with-gaproot=PATH`
  * Drop the `-ansi` option from the flags passed to the C compiler

## Version 5.6.1 (2022-09-26)
  * Re-release of 5.6 to avoid an issue with the GAP release process

## Version 5.6 (2022-09-15)
  * Various minor improvements to the C code

## Version 5.5 (2022-08-01)
  * Work around a bug in the GAP help system that prevents it from finding
    the HTML version of the ACE manual appendices. This affects searches in
    these appendices when GAP is configured to open help in a web browser
    instead of the terminal.

## Version 5.4 (2022-03-09)
  * Adapt test suite to changes to the library of perfect groups
  * Allow overriding compiler by setting the `CC` environment variable
  * Various janitorial changes

## Version 5.3 (2020-02-12)
  * Increase default workspace from 10^6 to 10^8 words
  * Fix compatibility with Cygwin
  * Drop build date from binary
  * Allow overriding CC and use CFLAGS and LDFLAGS
  * Various janitorial changes

## Version 5.2 (2016-03-11)
  Mainly cosmetic changes to improve compatibility with GAP 4.7.
  * LICENSE (new file)
    - All parts of ACE (including the GAP package, and the original C
      standalone executable) are now licensed under the so-called MIT 
      license. For details, refer to the LICENSE file.
  * README,doc/{ace,options,moreexamples}.tex
    - Notes about bug placed in above files.
      In documentation these are in sections 1.11, 4.4, C.1 (ahead of
      example dated Wed Oct 31 09:41:14 2001)
  * PackageInfo.g,doc/install.tex
    - Max Horn listed as co-maintainer.
    - Now list dependence as requiring GAP version >= 4.7.
  * PackageInfo.g,doc/{{ace,install}.tex,manual.bib},src/al[012].h,
    src/{ace,coinc,control,enum,enum0[012],parser,postproc,util[012]}.c
    - updated email address for Colin Ramsay, and web address for 
      George Havas and ACE 3.001.
  * doc/manual.bib
    - some references now in the main .bib file
  * doc/manual.tex
    - two \def s added to ensure GAP's manualbib.xml.bib (prev. manual.bib)
      is compatible with gapmacro compilation.
  * doc/install.tex
    - changed Reference manual reference to `gap.ini` and `gaprc` files,
      from `.gaprc` file.
    - removed reference to `make_doc`, which is not included in
      downloadable archives.
  * doc/{ace,install,options}.tex:
    - added blank line before each \endexample line, to account for
      changed behaviour which was not tabbing the last line of an example
  * PackageInfo.g,README,VERSION,doc/{manual,ace,install}.tex:
    - routine changes
  * tst/aceds.tst
    - Info lines in example only appear when `InfoACE` is set to 3.
      This has been wrong for a while. It was wrong in Version 5.0.
      Line `SetInfoACELevel(3)` added. Some additional lines also appear
      at this `InfoACE` level. These and some further tidy-ups done.

## Version 5.1 (2012-01-22)
  Last GAP 4.4 version.
  * make_zoo,pack_pkg:
    - make_zoo is deprecated
    - (newly created) pack_pkg now creates a tar.gz archive
  * configure,Makefile.in:
    - made more robust, to ensure that they work well with both 
      GAP 4.4 and 4.5
  * doc/manual.bib:
    - some typos. fixed
  * doc/make_doc:
    - the current HTML converter etc/convert.pl fails to include /htm
      in the main GAP manual links, since it is attuned for GAP 4.5; 
      a temporary perl command was added to fix this
  * PackageInfo.g,README,VERSION,doc/{manual,ace,install}.tex:
    - routine changes including an update to address and email address

## Version 5.0 (2006-01-26)
  * pre-GAP 4.4 compatibility features removed:
    - gap/ace.g:
      . banner removed ... this role is performed by PackageInfo.g
        since GAP 4.4; thus `pkgbanner` option no longer exists
      . file split into lib/ace.gd and lib/ace.gi
    - gap/compat4r2.g[di]:
        (GAP 4.2 compatibility) deleted from distribution
    - gap/options.gi:
        `ACE_COSET_TABLE_STANDARD` simplified by removing GAP 4.2
        compatibility
    - read.g,init.g:
      . removed all but the `ReadPkg` commands 
      . changed `ReadPkg` to `ReadPackage`
      . made adjustments for gap/ace.g split
  * gap/*.g[di],doc/*.tex:
    - changed Department name in header
  * PackageInfo.g,README,VERSION,doc/{ace,install,examples}.tex:
    - routine changes as per above

## Version 4.1
  GAP 4.4 Release version.
  * PkgInfo.g replaced by PackageInfo.g
    - `Pkg...` fields replaced by `Package...` fields
    - `AutoLoad` fields replaced by `Autoload`
    - now defines `Subtitle`, `BannerString` and `TestFile` fields
  * gap/ace.g:
    - banner no longer generated by this file for GAP 4.4
  * make_zoo:
    - now includes doc/manual.{lab,toc}
  * doc/make_doc:
    - now generates PDF format of manual also
  * res-examples/pgrelfind.g:
    - added M_12 presentation
  * README,VERSION,doc/{ace,install,examples}.tex:
    - routine changes as per above

## Version 4.0
  Revised for GAP 4.4. Not given general release.
  * init.g 
    - for GAP 4.3 compatibility, if GAP 4.3 then 
      `GAPInfo` record defined with entries:
        . `DirectoriesTemporary` which is defined to be `DIRECTORIES_TEMPORARY`
        . `Version` which is defined to be the contents VERSION
  * `ACEPackageVersion` 
    - no longer temporarily defined in init.g
    - defined to return the value of `GAPInfo.Version` (in GAP 4.3 and GAP 4.4)
  * gap/*.g[di]:
    - Revision names now of form "ace/gap..."
  * gap/general.gi:
    - `ACEPackageVersion` redefined as above
    - `GAPInfo.DirectoriesTemporary` replaces `DIRECTORIES_TEMPORARY`
  * GAP 4.2 compatibility no longer supported 
    - ace/compat4r2.g[di] no longer distributed

## Version 3.003
  * Initial value of `ACEIgnoreUnknownDefault` is now  `true`  (previously
    it was `false`).

  * `ACECosetTableFromGensAndRels` and the non-interactive
    `ACECosetTable` (except when producing an input script via  option 
    `ACEinfile`),  and the non-interactive `ACEStats` which previously 
    used  files  all  now invoke iostreams.

  * Option `pkgbanner` used to control the printing of the banner on
    loading.

## Version 3.002
  * `ACEIgnoreUnknownDefault` added.

  * Option `continue` changed to `continu`, as `continue` is a keyword in
    GAP 4.3.

## Version 3.001
  * First release.
