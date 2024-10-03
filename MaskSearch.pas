unit MaskSearch;

interface
uses Classes, SysUtils, Windows, ShellAPI;

// String routines
procedure GetShellFileInfo(FileName :TFileName; var ShFileInfo :TShFileInfo);
//function SizeToStr(Size:int64; Typ :Integer): string;
//function AttrStr(Attr:Integer): string;
//function GetTimeModified(a:tfiletime):string;

// File Search utility
procedure SetFilters(a:string; GrepList:TStringList; FindFile:boolean; MatchCase :boolean);
function cmpmask(a:string; GrepList:TStringList;findfile : boolean; MatchCase :boolean):boolean;
function cmpfile(FileName:string; GrepList:TStringList; MatchCase :boolean):boolean;

implementation

//
// STRING ROUTINES
//
// Get Shell Info for the specified file
procedure GetShellFileInfo (FileName:TFileName; var ShFileInfo:TShFileInfo);
begin
  ShGetFileInfo(PChar(FileName), 0, ShFileInfo, SizeOf(ShFileInfo),
                 SHGFI_SYSICONINDEX or SHGFI_ICON or
                 SHGFI_DISPLAYNAME or SHGFI_TYPENAME or
                 SHGFI_SMALLICON);
end;

// Convert Size for Sort
{function SizeToStr(Size:int64; Typ :Integer): string;
begin
  if (Typ and faDirectory) = faDirectory then
    Result := ''
  else
    Result := Format ('%20u', [Size]);
end;
 }

// returns a string with file attributes (DRSH)
{function AttrStr(Attr:Integer):string;
begin
  Result := '';
  if (Attr and FILE_ATTRIBUTE_DIRECTORY)  > 0 then Result := Result + 'D';
  if (Attr and FILE_ATTRIBUTE_ARCHIVE)    > 0 then Result := Result + 'A';
  if (Attr and FILE_ATTRIBUTE_READONLY)   > 0 then Result := Result + 'R';
  if (Attr and FILE_ATTRIBUTE_SYSTEM)     > 0 then Result := Result + 'S';
  if (Attr and FILE_ATTRIBUTE_HIDDEN)     > 0 then Result := Result + 'H';
  if (Attr and FILE_ATTRIBUTE_COMPRESSED) > 0 then Result := Result + 'C';
  if (Attr and FILE_ATTRIBUTE_TEMPORARY)  > 0 then Result := Result + 'T';
end;
 }

{
function GetTimeModified(a:tfiletime):string;
// This function retrieves the last time, the given file was written to disk
var
  mtm :TSystemTime;
  at  :TFileTime;
  ds,ts:string;
  //ds,ts:ShortString;
const
  MAX_DATETIME_STR = 255;
begin
  // Time must get converted, else there is an error of one hour
  // Does anybody know what this function does ?
  // Maybe something like summertime/wintertime (or what you call it out of Germany) ?
  FileTimeToLocalFileTime(a,at);
  FileTimeToSystemTime(at,mtm);
  SetLength(ds, GetDateFormat(LOCALE_USER_DEFAULT, 0, @mtm, NIL, @ds[1], 255) - 1);
  SetLength(ts, GetTimeFormat(LOCALE_USER_DEFAULT, TIME_NOSECONDS, @mtm, NIL, @ts[1], 255)  - 1);
  Result:=ds+'  '+ts;
end; // End getmod
 }

function CaseAware(S :string; Match :Boolean) :string;
begin
  if Match
  then Result := S
  else Result := AnsiLowerCase(S);
end;

//
// Original File Search Routine by Marcus Stephany
//
procedure SetFilters(a: string; GrepList: TStringList; FindFile: Boolean; MatchCase: Boolean);
// fills the grep_list with the parts of 'a' (divided by ',' or ';')
// findfile describes whether to use for find files or text in files
// + aml modified : Match Case
var
	ct: integer;
begin
     GrepList.clear;
     GrepList.sorted := False;
     if a = '' then begin
        GrepList.add('*');
        exit;
     end;

     // replace all ',' by ';'
     ct := Pos(',', a);
     while ct > 0 do begin
       a[ct] := ';';
       ct := Pos(',', a);
     end;

     if a[length(a)] <> ';' then a := a + ';';

     // divide the string
     ct := Pos(';', a);
     while ct > 0 do begin
       GrepList.Add(CaseAware(Trim(Copy(a, 1, ct - 1)), MatchCase));
       a := Copy(a, ct + 1, MaxInt);
       ct := Pos(';', a);
     end;

     // replace a 'xxx' term (without a '.') with '*xxx*' (for compatibility
     // with win95's file-search-dialog)
     // only if findfile
     if FindFile then begin
       if GrepList.Count > 0 then for ct := 0 to Pred(GrepList.Count) do begin
         a := GrepList[ct];
         if (Pos('*', a) = 0) and (Pos('?', a) = 0) and (Pos('.', a) = 0) then
           GrepList[ct] := '*' + a + '*'
         else if Pos('.', a) = 0 then if a[length(a)] <> '*' then
           GrepList[ct] := a + '*';
       end;
     end;

     GrepList.Sorted := True;
     GrepList.Duplicates := dupIgnore;
end;

function cmpmask1(a, b:string; FindFile:boolean):boolean;
// tests whether the string 'a' fits to the search mask in 'b'
var sr             : string;
    ps1,ps2,ps3    : integer;
    dontcare       : boolean;
    onechar        : char;
    tmp_list       : tstrings;
begin
     Result := true;
     if b = '*' then exit; // fits always
     if b = '*.*' then if Pos('.',a) > 0 then exit; // fits, too
     if (Pos('*',b) = 0) and (Pos('?',b)=0) then
        if not FindFile then begin
           if Pos(b,a) > 0
              then exit;
           // searched text was found (searchstring IN text)
        end else
           if a = b then exit;
           // searched file was found (searchstring IS text)


     Result := false;
     if b = '' then exit;
     try
        tmp_list := TStringList.create;
        // divide partial strings ('?','*' or text) to tmp_list
        repeat
              onechar := b[1];
              if (onechar='*') or (onechar='?') then begin
                 tmp_list.Add(onechar);
                 Delete(b, 1, 1);
              end else begin
                  ps1 := Pos('?',b);
                  if ps1 = 0 then ps1 := MaxInt;
                  ps2 := Pos('*',b);
                  if ps2 = 0 then ps2 := MaxInt;
                  if ps2 > ps1 then ps2 := ps1;
                  tmp_list.Add(Copy(b, 1, ps2 - 1));
                  b := Copy(b, ps2, MaxInt);
              end;
        until b = '';
        // now compare the string with the partial search masks
        dontcare := false;
        ps2      := 1;
        if tmp_list.Count > 0 then for ps1 := 0 to Pred(tmp_list.Count) do begin
           sr := tmp_list[ps1];
           if sr = '?' then begin
              Inc(ps2,1);
              if ps2 > Length(a) then exit;
           end else
           if sr = '*' then
              dontcare := true
           else begin
                if not dontcare then begin
                   if Copy(a, ps2, Length(sr)) <> sr then exit;
                   dontcare := false;
                   ps2 := ps2 + Length(sr);
                end else begin
                   ps3:= Pos(sr, Copy(a, ps2, MaxInt));
                   if ps3 = 0 then exit;
                   ps2 := ps3+length(sr);
                   dontcare := false;
                end;
           end;
        end;
        if not dontcare then if ps2 <> Length(a)+1 then exit;
        Result := true;
     finally
       tmp_list.free;
     end;
end;

function cmpmask(a:string; GrepList:TStringList; FindFile:boolean; MatchCase :Boolean):Boolean;
// tests whether the string 'a' fits to the search masks in grep_list
var ct : integer;
begin
     Result := True;
     if a = '' then exit; // if no search string, the always return TRUE
     a := CaseAware(a, MatchCase);
     Result := False;
     if (GrepList = nil) or (GrepList.Count < 1) then exit;
     Result := True;
     for ct := 0 to Pred(GrepList.Count) do
         if cmpmask1(a, GrepList[ct], FindFile) then exit; // compare with the whole
                                                          // grep_list until one fits
     Result := false;
end;

function cmpfile(FileName:string; GrepList:TStringList; MatchCase :Boolean):Boolean;
// tests whether a file's contents fit to the specified mask;
var
   fl:string;
   ts:TFileStream;
   //ct:Integer;
begin
     Result := True;
     // different handling between filefind an textfind
     // true if no or each text is wanted
     if (GrepList.Count < 1) or (GrepList[0] = '*') then exit;

     Result := False;
     try
       ts := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
     except
       exit;
     end;
     try
       SetLength(fl, ts.Size + 1);
       ts.Position := 0;
       ts.Read(fl[1], ts.Size);
       ts.Free;
       Result := cmpmask(CaseAware(fl, MatchCase), GrepList, false, MatchCase);
     finally
       SetLength(fl, 0);
     end;
end;


end.
