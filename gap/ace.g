#############################################################################
##
#W    ace.g               share package 'ace'               Alexander Hulpke
##
##    This file contains the interface to the ace binary by George Havas.
##    @(#)$Id$
##

# set up paths.
ACEinfo:=rec(
  binary:=Filename(DirectoriesPackagePrograms("ace"),"ace"),
  tmpdir:=DirectoryTemporary()
  ); # to store file and directory names
ACEinfo.infile:=Filename(ACEinfo.tmpdir,"ein");
ACEinfo.outfile:=Filename(ACEinfo.tmpdir,"aus");

PrintTo(ACEinfo.infile,"\n");
Exec(Concatenation("cd ",Filename(ACEinfo.tmpdir,""),"; ",
      ACEinfo.binary," <ein >aus"));
ACEinfo.file:=InputTextFile(ACEinfo.outfile);
ACEinfo.version:=ReadLine(ACEinfo.file);
CloseStream(ACEinfo.file);
ACEinfo.file:=PositionSublist(ACEinfo.version,"ACE")+3;
ACEinfo.version:=ACEinfo.version{
  [ACEinfo.file+1..Position(ACEinfo.version,' ',ACEinfo.file)-1]};

Print("  Loading the `ACE' share package version ",PACKAGES_VERSIONS.ace,"\n");
Print("  Using ACE binary version ",ACEinfo.version,"\n");

BindGlobal("CallACE",function(fgens,grels,fsgens)
local n,nums,i,DoWords,infile,l,ind,p,gens,w,tab,a,redir,flag;
  n:=Length(fgens);
  if ForAny(fgens,i->NumberSyllables(i)<>1 or ExponentSyllable(i,1)<>1) then
    Error("wrong fgens");
  fi;
  nums:=List(fgens,i->GeneratorSyllable(i,1));
  IsSSortedList(nums); # force sort flag
  PrintTo(ACEinfo.infile,"ENUM:G\nGR:");
  gens:=[];
  for i in [1..n] do
    w:=WordAlp(CHARS_LALPHA,i);
    Add(gens,w);
    AppendTo(ACEinfo.infile,w);
    if i<n then
      AppendTo(ACEinfo.infile,",");
    fi;
  od;
  AppendTo(ACEinfo.infile,";\nREL:");

  DoWords:=function(w)
  local i,j,m,e,n;
    for i in [1..Length(w)] do
      m:=NumberSyllables(w[i]);
      for j in [1..m] do
	n:=Position(nums,GeneratorSyllable(w[i],j));
	e:=ExponentSyllable(w[i],j);
	AppendTo(ACEinfo.infile,WordAlp(CHARS_LALPHA,n));
	if e<>1 then
	  AppendTo(ACEinfo.infile,"^",e);
	fi;
	if j<m then
	  AppendTo(ACEinfo.infile,"*");
	fi;
      od;
      if i<Length(w) then
	AppendTo(ACEinfo.infile,",");
      fi;
    od;
  end;

  DoWords(grels);

  AppendTo(ACEinfo.infile,";\nSUBG:H\nGEN:");
  DoWords(fsgens);
  AppendTo(ACEinfo.infile,";\n");

  # now do the options
  a:=ValueOption("com");
  if IsInt(a) and a>=0 then
    AppendTo(ACEinfo.infile,"COM:",a,";\n");
  fi;

  a:=ValueOption("workspace");
  if IsInt(a) and a>0 then
    AppendTo(ACEinfo.infile,"WO:",a,";\n");
  fi;

  a:=ValueOption("max");
  if a<>fail then
    if IsList(a) then
      l:="";
      for i in [1..Length(a)] do
        Append(l,String(a[i]));
	if i<Length(a) then
	  Add(l,',');
	fi;
      od;
      AppendTo(ACEinfo.infile,"MAX:",l,";\n");
    elif IsInt(a) then
      AppendTo(ACEinfo.infile,"MAX:",a,";\n");
    fi;
  fi;

  a:=ValueOption("ct");
  if a<>fail then
    if IsList(a) then
      l:="";
      for i in [1..Length(a)] do
        Append(l,String(a[i]));
	if i<Length(a) then
	  Add(l,',');
	fi;
      od;
      AppendTo(ACEinfo.infile,"CT:",l,";\n");
    elif IsInt(a) then
      AppendTo(ACEinfo.infile,"CT:",a,";\n");
    fi;
  fi;

  a:=ValueOption("rt");
  if a<>fail then
    if IsList(a) then
      l:="";
      for i in [1..Length(a)] do
        Append(l,String(a[i]));
	if i<Length(a) then
	  Add(l,',');
	fi;
      od;
      AppendTo(ACEinfo.infile,"RT:",l,";\n");
    elif IsInt(a) then
      AppendTo(ACEinfo.infile,"RT:",a,";\n");
    fi;
  fi;

  a:=ValueOption("time");
  if IsInt(a) and a>0 then
    AppendTo(ACEinfo.infile,"TI:",a,";\n");
  fi;

  a:=ValueOption("fill");
  if a<>fail then
    if IsList(a) then
      l:="";
      for i in [1..Length(a)] do
        Append(l,String(a[i]));
	if i<Length(a) then
	  Add(l,',');
	fi;
      od;
      AppendTo(ACEinfo.infile,"FI:",l,";\n");
    elif IsInt(a) then
      AppendTo(ACEinfo.infile,"FI:",a,";\n");
    fi;
  fi;

  a:=ValueOption("mendelsohn");
  if a<>fail then
    AppendTo(ACEinfo.infile,"MEND:1;\n");
  fi;
  a:=ValueOption("asis");
  if a<>fail then
    AppendTo(ACEinfo.infile,"ASIS:0;\n");
  fi;
  a:=ValueOption("mess");
  if a<>fail then
    if IsInt(a) and (a<-1 or a>1) then
      AppendTo(ACEinfo.infile,"MESS:",a,";\n");
    fi;
    redir:="";
  else
    redir:="> temp";
  fi;


  AppendTo(ACEinfo.infile,"END\nAO:");
  AppendTo(ACEinfo.infile,ACEinfo.outfile);
  AppendTo(ACEinfo.infile,";\nPR: 0;\n");
  Exec(Concatenation("cd ",Filename(ACEinfo.tmpdir,""),"; ",
	  ACEinfo.binary," <ein ",redir));

  infile:=InputTextFile(ACEinfo.outfile);
  l:=ReadLine(infile);

  # skip some header
  while l<>fail and l[1]<>' ' do
    l:=ReadLine(infile);
  od;

  if l=fail then
    Error("no output from ACE?");
  fi;

  # get the indices
  ind:=[];
  p:=0;
  i:=1;
  while i<Length(l) do
    while i<Length(l) and l[i] in " \n" do
      i:=i+1;
    od;
    p:=p+1; # another column
    w:="";
    while i<Length(l) and not l[i] in " \n" do
      Add(w,l[i]);
      i:=i+1;
    od;
    if Length(w)>0 and w[1] in CHARS_LALPHA then
      w:=Position(gens,w);
      if w<>fail then
	ind[w]:=p;
      fi;
    fi;
  od;
  # now read the table
  tab:=List([1..n],i->[]);
  p:=0;
  l:=ReadLine(infile);
  flag:=true;
  while flag and l<>fail and Length(l)>1 do
    p:=p+1;
    l:=SplitString(l,""," :\n"); # remove colon, space and newline
    # syntax check
    if Int(l[1])=fail or Int(l[1])<>p then
      Error("syntax error in file");
    fi;
    for i in [1..n] do
      a:=Int(l[ind[i]+1]);
      if a=0 then
        flag:=false;
      fi;
      Add(tab[i],a);
    od;

    l:=ReadLine(infile);
  od;
  CloseStream(infile);
  if flag=false then
    return fail;
  fi;
  l:=[];
  n:=Length(tab[1]);
  for i in tab do
    Add(l,i);
    i:=PermList(i)^-1;
    Add(l,List([1..n],j->j^i));
  od;
  return l;
end);

# the ACE version of the coset enumerator
BindGlobal("ACETCENUM",rec(name:="ACE-enumerator",
 CosetTableFromGensAndRels:=
  function(fgens,grels,fsgens)
  local l;
    repeat
      l:=CallACE(fgens,grels,fsgens);
      if l=fail then
	Error("the coset enumeration using the `ACE' enumerator did not ",
		"complete.\n If you called the command with restrictive",
		"options relax these. Otherwise increase the memory available",
		"to `ACE'.\nType `quit;' to exit to the main loop.");
      fi;
    until l<>fail;
    return l;
  end));
