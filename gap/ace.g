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

Print("  Loading the `ACE' share package\n");
Print("  Using ACE binary version ",ACEinfo.version,"\n");

BindGlobal("CALL_ACE",function(arg)
local fgens,grels,fsgens,j,k,n,nums,i,DoWords,ParseValueOption,
      ParseBooleanOption,infile,l,ind,p,gens,w,tab,a,redir,flag,stats,statval;

  fgens:=arg[1];
  grels:=arg[2];
  fsgens:=arg[3];
  # only do stats
  stats:=Length(arg)>3 and arg[4]=1;

  n:=Length(fgens);
  if ForAny(fgens,i->NumberSyllables(i)<>1 or ExponentSyllable(i,1)<>1) then
    Error("wrong fgens");
  fi;
  nums:=List(fgens,i->GeneratorSyllable(i,1));
  IsSSortedList(nums); # force sort flag


  # do we want to produce only an input file
  infile:=ACEinfo.infile;
  a:=ValueOption("outfile");
  if a<>fail and IsString(a) then
    infile:=a;
  fi;

  PrintTo(infile,"ENUM:G\nGR:");
  gens:=[];
  for i in [1..n] do
    w:=WordAlp(CHARS_LALPHA,i);
    Add(gens,w);
    AppendTo(infile,w);
    if i<n then
      AppendTo(infile,",");
    fi;
  od;
  AppendTo(infile,";\nREL:");

  DoWords:=function(w)
    local i,j,m,e,n;
    for i in [1..Length(w)] do
      m:=NumberSyllables(w[i]);
      for j in [1..m] do
        n:=Position(nums,GeneratorSyllable(w[i],j));
        e:=ExponentSyllable(w[i],j);
        AppendTo(infile,WordAlp(CHARS_LALPHA,n));
        if e<>1 then
          AppendTo(infile,"^",e);
        fi;
        if j<m then
          AppendTo(infile,"*");
        fi;
      od;
      if i<Length(w) then
        AppendTo(infile,",");
      fi;
    od;
  end;

  DoWords(grels);

  AppendTo(infile,";\nSUBG:H\nGEN:");
  DoWords(fsgens);
  AppendTo(infile,";\n");

  # now do the options

  ParseBooleanOption:=function(arg)
    a:=ValueOption(arg[1]);
    if a=true then
      if Length(arg)>1 then
	AppendTo(infile,arg[2],";\n");
      else
	AppendTo(infile,arg[1],";\n");
      fi;
    elif a=false then
      if Length(arg)>2 then
	AppendTo(infile,arg[3],";\n");
      fi;
    fi;
  end;

  ParseValueOption:=function(arg)
    a:=ValueOption(arg[1]);
    if a<>fail then
      if IsBool(a) then
	PopOptions();
        Error("Option ",arg[1]," needs a value");
      fi;
      AppendTo(infile,arg[Length(arg)],":",String(a),";\n");
    fi;
  end;

  # first the strategies
  ParseBooleanOption("default");
  ParseBooleanOption("easy");
  ParseBooleanOption("hard");
  ParseBooleanOption("hlt");
  ParseBooleanOption("purec");
  ParseBooleanOption("purer");
  ParseBooleanOption("felsch0","felsch:0");
  ParseBooleanOption("felsch1","felsch:1");
  ParseBooleanOption("sims1","sims:1");
  ParseBooleanOption("sims3","sims:3");
  ParseBooleanOption("sims5","sims:5");
  ParseBooleanOption("sims7","sims:7");
  ParseBooleanOption("sims9","sims:9");

  # now the modifying options
  ParseBooleanOption("asis","asis:1","asis:0");
  ParseValueOption("ct");
  ParseValueOption("rt");
  ParseValueOption("number");
  ParseBooleanOption("mendelsohn","mend:1","mend:0");
  ParseBooleanOption("mend","mend:1","mend:0");
  ParseValueOption("fill");
  ParseValueOption("pmode");
  ParseValueOption("psize");
  ParseBooleanOption("norow","row:0","row:1");
  ParseValueOption("look");
  ParseValueOption("dmode");
  ParseValueOption("dsize");

  # Technical options
  ParseValueOption("workspace");
  ParseValueOption("rtime","time");
  ParseValueOption("max");

  # Info Stuff
  ParseValueOption("aep");
  a:=ValueOption("rep");
  if a<>fail then
    if IsInt(a) then
      AppendTo(infile,"rep:",a,";\n");
    elif IsList(a) then
      AppendTo(infile,"rep:",a[1],",",a[2],";\n");
    fi;
  fi;

  a:=ValueOption("mess");
  if a<>fail and not stats then
    if IsInt(a) and (a<-1 or a>1) then
      AppendTo(infile,"MESS:",a,";\n");
    fi;
    redir:="";
  else
    redir:="> temp";
  fi;

  a:=ValueOption("messfile");
  if a<>fail then
    AppendTo(infile,"AO:",a,";\n");
    redir:="> temp";
  fi;

  AppendTo(ACEinfo.infile,"AO:",ACEinfo.outfile,";\n");
  AppendTo(infile,"END\n");

  if infile<>ACEinfo.infile then
    # we only wanted to write an input file
    return;
  fi;

  if stats=false then
    AppendTo(ACEinfo.infile,"PR;\n");
  fi;

  Exec(Concatenation("cd ",Filename(ACEinfo.tmpdir,""),"; ",
          ACEinfo.binary," <ein ",redir));

  infile:=InputTextFile(ACEinfo.outfile);
  l:=ReadLine(infile);

  # parse the line for statistics
  a:=Filtered(l,i->i in ". " or i in CHARS_DIGITS);
  a:=SplitString(a,""," .");
  if stats then 
    if l{[1..5]}="INDEX" then
      statval:=[Int(a[1]),Int(a[7])+Int(a[8])/10^Length(a[8]),
		Int(a[9]),Int(a[10])];
    else
      statval:=[0,Int(a[6])+Int(a[7])/10^Length(a[7]),Int(a[8]),Int(a[9])];
    fi;

    CloseStream(infile);
    return statval;
  fi;

  # skip some header until the ` coset ' line
  while l<>fail and l{[1..6]}<>" coset" do
    l:=ReadLine(infile);
  od;

  if l=fail then
    PopOptions();
    Error("no output from ACE?");
  fi;

  # get the indices
  ind:=[];
  p:=0;
  i:=9;
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
  # read the `---' line
  l:=ReadLine(infile);

  # now read the table
  tab:=List([1..n],i->[]);
  p:=0;
  l:=ReadLine(infile);
  flag:=true;
  while flag and l<>fail and Length(l)>1 and l[1]<>'=' do
    p:=p+1;
    l:=SplitString(l,""," :|\n"); # remove colon, space, | and newline
    # syntax check
    if Int(l[1])=fail or Int(l[1])<>p then
      PopOptions();
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
    if ForAll([1..n], j->i[i[j]] = j) then
      Add(l,i);
    else
      j := [];
      for k in [1..n] do
        j[i[k]] := k;
      od;
      Add(l,j);
    fi;
  od;
  return l;
end);

BindGlobal("CallACE",function(fgens,grels,fsgens)
  local l,silent;
  silent:=ValueOption("silent")=true;
  repeat
    l:=CALL_ACE(fgens,grels,fsgens);
    if l=fail then
      if silent then return fail;fi;
      Error("the coset enumeration using the `ACE' enumerator did not ",
            "complete.\n If you called the command with restrictive",
            "options relax these. Otherwise increase the memory available",
            "to `ACE'.\nType `quit;' to exit to the main loop.");
    fi;
  until l<>fail;
  StandardizeTable(l);
  return l;
end);

# the ACE version of the coset enumerator
BindGlobal("ACETCENUM",rec(name:="ACE-enumerator",
        CosetTableFromGensAndRels:=CallACE));

BindGlobal("ACEStats",function(fgens,grels,fsgens)
  return CALL_ACE(fgens,grels,fsgens,1);
end);

