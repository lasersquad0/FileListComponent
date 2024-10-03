{
  FileList - File ListView components for Delphi
  based on component written by Amedeo Lanza <amldc@tin.it>

Description:
 TFileView is a ListView which links system image lists on creation and can
 accept files dropped from Windows' Explorer.
 TFileList is a FileView descendant wich encapsulates some system functions to
 mix ancestor's capability with the FileListBox's Directory and Mask properties.
 Thus specifying a path name and a file mask, the control will be filled with
 the list of matching files, showing proper icons and a customizable set of file
 information.

 Copyright & license
 This source is Copyright © of Amedeo Lanza di Casalanza and Andrey Romanchenko.
 You may freely use, modify and distribute this software for non commercial
 purposes and for writing freeware and/or shareware software.
 You MAY NOT SELL this software although you may include it in software collection
 you distribute, provided there is no charge for the software itself.
 Please leave the copyright information and any additional information (readme.* etc.)
 provided with the original author's copy.
 Use of this software for building of commercial programs should be expressely
 authorized by the author.
 Any use of this software for illegal activities is prohibited.

 If you enhance this software or fix any bug, please send the new source to
 the authors.

Usage:
  Drop a TFileList and some controls to handle Directory and File Mask
  selections, then add some lines of code to set the Directory and Mask
  properties of TFileList upon selection from other controls ...just as you
  where using a TFileListBox control :-)

Dependencies:
  Marcus Stephany's MASKSEARCH utility unit is needed. It is included into FileList package.

Comments:
  Uses FindFirstFile, FindNextFile and FindClose API calls from Windows unit to
  retrieve file list with better performances and ShGetFileInfo to retrieve
  extended information.

Current known limitations :
  Tested with Delphi 11.3 and 12.X on Windows 10 and Windows 11.
  Handles only File Items, System Resources cannot be displayed as in an
  Explorer's window.
  Columns order is mainly fixed, however it is possible to move columns by mouse if
  FullDrag property is set to True. Setting of columns' width and caption is quite
  trivial.


DISCLAIMER:
 I ASK NO FEE and I GIVE NO WARRANTY for this software, either expressed or
 implied. Use it AT YOUR OWN RISK.

Suggestions, bug reports & comments to the author:
 Andrey Romanchenko
 lasersquad@gmail.com
 https://github.com/lasersquad0

Aknowledgement:
 Portions of this software come from freeware examples by
  Markus Stephany
  MirBir.St@T-Online.de

History:

 v2.5 28-AUG-2014 - lasersquad@gmail.com
   + Many fixes and changes to make it work on latest Delphi version

}

unit FileList;

interface

uses
  Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.ComCtrls, WinAPI.ShellApi, Hash;

type
  // callback for Drop file
  TFileDropEvent = procedure(Files: TStrings; X, Y: Integer) of object;

  // file informations (display columns)
  TFileInfo = (fiName, fiSize, fiType, fiModified, fiCreated, fiLastAccess,
               fiAttributes, fiDosName, fiDosExt, fiPath, fiAll);

  TFileInfos = set of TFileInfo;

  TFileInfoToColumnID = array [TFileInfo] of Integer;

  {TTypeIconList = class (TStringList)
  public
    procedure AddType(WinType :string; IconIndex :integer);
    function IconOf(Wintype :string) :integer;
  end;}

  TFileListView = class(TCustomListView)
  private
    FAcceptFiles: Boolean;
    FOnFileDrop: TFileDropEvent;
    FSortColumnID: Integer;
    FInvertSort: Boolean;  // used to invert sorting when column clicked twice
    FColumnIDtoInfo: THash<Integer, TFileInfo>; // makes it easier to find column type (fiName, fiSize, etc.) by Column.ID
    FFileInfoToColumnID: TFileInfoToColumnID;
//    FLastUserDefinedColumn: Integer;  // Index of last column defined by user (in column editor in design time). First column has index 0. Equals -1 is there is no user defined columns

    procedure GetSystemImageList; // load LargeImages and SmallImages
    procedure WMDROPFILES(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure SetAcceptFiles(Accept: Boolean);
    function ItemIndexByInfo(Col: TFileInfo): Integer;
    function ItemIndexByID(ColID: Integer): Integer;
    procedure SyncColumnData;
    function GetFileShellInfo(FileName: TFIleName; var Item: TListItem; var FindData: TWin32FindData): Boolean;  // Get Windows file name, system file type and icon
    class function SortProcFileListView(Item1, Item2: TListItem; Data: Integer): Integer; stdcall; static;

  protected
    procedure Loaded; override;
    procedure ColClick(Column: TListColumn); override;
    procedure ResetFileInfoToColumnID;
    procedure ClearSortingMark(ColID: Integer);
    procedure SetSortingMark(ColID: Integer; Ascending: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddFile(FullPath: string; FileData: TWin32FindData): TListItem; overload;
    function AddFile(FileName: TFileName; LoadFileData: Boolean = True): TListItem; overload; virtual;
    procedure SetupFileColumns(FileColumns: TFileInfos);
    procedure SetSortColumn(Column: TListColumn); virtual;
    function GetSelItemValue(Col: TFileInfo): string;
  published
    { Published inherited declarations }
    property Align;
    property BorderStyle;
    property Color;
    property ColumnClick;
    property OnClick;
    property OnDblClick;
    property Ctl3D;
    property DragMode;
    property ReadOnly;
    property Font;
    property HideSelection;
    property IconOptions;
    property Items;
    property AllocBy;
    property MultiSelect;
    property RowSelect;
    property OnChange;
    property OnChanging;
    property OnColumnClick;
    property OnColumnDragged;
    property OnCompare;
    property OnDeletion;
    property OnEdited;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnInsert;
    property OnDragDrop;
    property OnDragOver;
    property DragCursor;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property ParentShowHint;
    property ShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    //property SortType;
    property TabOrder;
    property TabStop default True;
    property ViewStyle;
    property Visible;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {class specific published properties}
    property AcceptFiles: Boolean read FAcceptFiles write SetAcceptFiles;
    property OnFileDrop: TFileDropEvent read FOnFileDrop write FOnFileDrop;
  end;

  // Just exports the needed inherited properties
  TFileView = class(TFileListView)
  published
    { Published inherited declarations }
    property Columns;
  end;

  // specialized list for patterns handling
 { TPatternList = class (TStringList)
  public
    procedure AddPatterns(S: string);
  end;}

  // file attributes
  TFileType = ({ftDirectory,}ftArchive, ftReadonly, ftSystem, ftHidden, ftCompressed, ftTemporary, ftAll);
  TFileTypes = Set of TFileType;

  // Column settings structure
  TColumnInfo = record
    Caption   :TCaption;   // Column caption
    Width     :Integer;    // Column Width
    Alignment :TAlignment; // Column Alignement
   // ColumnID  :Integer;    // Column Number
  end;

  TColumnInfos = array [TFileInfo] of TColumnInfo;

  // callback for items insertion
  TFlInsertEvent = procedure (Sender: TObject; Item: TListItem; Path: string; FindData: TWin32FindData) of object;
  TFlReadDirectoryEvent = procedure (Sender: TObject; Path: string) of object;

  // TFileList is a TListView clone wich behaves like Windows Explorer's file window
  TFileList = class(TFileListView)
  private
    FFileTypes       : TFileTypes;      // kind of filter by file attributes
    //FDirectory       :TPatternList;    // current directory
    FDirectory       : string;          // current directory
    FFileMask        : TStringList;     // file specifications list
    FDisplayDirs     : Boolean;         // include directories in file scan
    FDisplayParentDir: Boolean;         // whether to show '..' directory
    FViewColumns     : TFileInfos;      // visible columns
    FSortColumnType  : TFileInfo;       // current sort column
    FData            : THash<TListItem, TWin32FindData>;
    FOnFileAdd       : TFLInsertEvent;  // chance to add custom info
    FOnReadDirectory : TFlReadDirectoryEvent; // signal that reading directory is done

    procedure UpdateFileList;       // scan the directory and load file list
    class function SortProcFileList(Item1, Item2: TListItem; Data: Integer): Integer; stdcall; static;

  protected
    procedure ColClick(Column: TListColumn); override;
    procedure DblClick; override;
    procedure Loaded; override;
    function  GetMask: string;  // Get file specifications
    //function  GetPath :string;  // Get search path list
    procedure SetDisplayDirs(Display: Boolean);  // enable/disable list of dirs
    procedure SetDisplayParentDir(Display: Boolean);  // enable/disable list of dirs
    procedure SetFileTypes(FT: TFileTypes);
    procedure SetMask(Mask: string); // Set file specifications
    //procedure SetPath(Path: string); // Set search path specifications
    procedure SetDirectory(NewDir: string); // Set directory and show its content
    procedure SetSortColumn(ColType: TListColumn); overload; override;
    procedure SetViewColumns(Columns: TFileInfos);
    procedure AddFileData(NewItem: TListItem; Path: string; FindData: TWin32FindData); // add a file
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  AddFile(FileName: TFileName): TListItem; // override; // add a file
    procedure AddMask(Mask: string); // Add file specifications
    //procedure AddPath(Path :string); // Add search path specifications
   // function  IsDirectory(Item :TListItem) :boolean;
    procedure SetColCaption(Col: TFileInfo; Value: string); // set column caption
    procedure SetColWidth(Col: TFileInfo; Value: Integer); // set column width
    procedure SetupFileColumns; overload;
    procedure SetupFileColumns(FileColumns: TFileInfos); overload;
    procedure SetSortColumn(ColType: TFileInfo); overload;

  published
    {class specific published properties}
    // The Directory property lets you specify a directory or a list of directories.
    // Upon change the specified paths will be used to retrieve and load matching files.
    // Use the Mask property to specify a file mask or a list of file masks.
    // List of paths and/or file masks may be separated by commas or semicolumns.
    // Setting the Directory property to a null string will prevent directory scan and
    // allow manual file loading.
    property Directory: string read FDirectory write SetDirectory;
    property FileTypes: TFileTypes read FFileTypes write SetFileTypes;
    property DisplayDirectories: Boolean read FDisplayDirs write SetDisplayDirs default True;
    property DisplayParentDir: Boolean read FDisplayParentDir write SetDisplayParentDir; // default true;
    property Mask: string read GetMask write SetMask;
    property OnFileAdd: TFLInsertEvent read FOnFileAdd write FOnFileAdd;
    property OnReadDirectory: TFLReadDirectoryEvent read FOnReadDirectory write FOnReadDirectory;
    property SortColumn: TFileInfo read FSortColumnType write SetSortColumn default fiName;
    property ViewColumns: TFileInfos read FViewColumns write SetViewColumns;
  end;

function ThousandSep(Num: UInt64): string; // convet UInt64 into string separated by thouthands separator
function SizeToStr(Size: uint64; Typ: Integer): string; // Convert Size for Sort
function GetLocalTime(ftm: TFileTime):string;


procedure Register;

implementation

uses WinAPI.CommCtrl, System.StrUtils, System.Math, Vcl.Dialogs, MaskSearch;

{$R *.dcr}

const
  DOTD = '.';
  DDOTD = '..';

  DefColumnInfos: TColumnInfos = ( (Caption:'Name'; Width:150; Alignment:taLeftJustify),
                                  (Caption:'Size'; Width:100; Alignment:taRightJustify),
                                  (Caption:'Type'; Width:130; Alignment:taLeftJustify),
                                  (Caption:'Modified'; Width:100; Alignment:taLeftJustify),
                                  (Caption:'Created'; Width:100; Alignment:taLeftJustify),
                                  (Caption:'Last Access'; Width:85; Alignment:taLeftJustify),
                                  (Caption:'Attributes'; Width:60; Alignment:taLeftJustify),
                                  (Caption:'Dos Name'; Width:130; Alignment:taLeftJustify),
                                  (Caption:'Dos Ext'; Width:60; Alignment:taLeftJustify),
                                  (Caption:'Path'; Width:200; Alignment:taLeftJustify),
                                  (Caption:'All'; Width:0; Alignment:taLeftJustify)
                                  );


var
  DefDirShellInfo: TShFileInfo;
  WinDir: string;


////////////////////////////////////////////////////////////////////////////////
// UTILITIES
////////////////////////////////////////////////////////////////////////////////

// compute 64bit file size
function MakeFileSize (hi, lo: Cardinal): uint64;
begin
  Result := (uint64(hi) shl 32) + uint64(lo);
end;

// check result of WINAPI call and raise exception in case of error
// many WINAPI functions return zero value in case of error, and non-zero in case of success
procedure B_RES(APIResult: NativeInt);
begin
  if APIResult = 0 then raise Exception.Create('Windows API Call failed.');
end;

// for directories SIZE column always contains empty value in TFileListView and TFileList
function IsDirectory(Item: TListItem): Boolean;
begin
  if (Item <> nil) AND (Item.SubItems.Count > 0)
    then Result := Item.SubItems[0] = ''
    else Result := False;
end;

function CheckAttributes(Att: DWord; Typ: TFileTypes): Boolean;
begin
  if (ftAll in Typ) then begin
    Result := True;
  end else begin
    Result := True;
    if (Att AND FILE_ATTRIBUTE_ARCHIVE)    = FILE_ATTRIBUTE_ARCHIVE    then Result := Result AND (ftArchive in Typ);
    if (Att AND FILE_ATTRIBUTE_READONLY)   = FILE_ATTRIBUTE_READONLY   then Result := Result AND (ftReadonly in Typ);
    if (Att AND FILE_ATTRIBUTE_HIDDEN)     = FILE_ATTRIBUTE_HIDDEN     then Result := Result AND (ftHidden in Typ);
    if (Att AND FILE_ATTRIBUTE_SYSTEM)     = FILE_ATTRIBUTE_SYSTEM     then Result := Result AND (ftSystem in Typ);
    if (Att AND FILE_ATTRIBUTE_TEMPORARY)  = FILE_ATTRIBUTE_TEMPORARY  then Result := Result AND (ftTemporary in Typ);
    if (Att AND FILE_ATTRIBUTE_COMPRESSED) = FILE_ATTRIBUTE_COMPRESSED then Result := Result AND (ftCompressed in Typ);
  end
end;

// returns a string with file attributes (DRSH)
function AttrStr(Attr: Integer): string;
begin
  Result := '';
  if (Attr AND FILE_ATTRIBUTE_DIRECTORY)  > 0 then Result := Result + 'D';
  if (Attr AND FILE_ATTRIBUTE_ARCHIVE)    > 0 then Result := Result + 'A';
  if (Attr AND FILE_ATTRIBUTE_READONLY)   > 0 then Result := Result + 'R';
  if (Attr AND FILE_ATTRIBUTE_SYSTEM)     > 0 then Result := Result + 'S';
  if (Attr AND FILE_ATTRIBUTE_HIDDEN)     > 0 then Result := Result + 'H';
  if (Attr AND FILE_ATTRIBUTE_COMPRESSED) > 0 then Result := Result + 'C';
  if (Attr AND FILE_ATTRIBUTE_TEMPORARY)  > 0 then Result := Result + 'T';
  if (Attr AND FILE_ATTRIBUTE_ENCRYPTED)  > 0 then Result := Result + 'E';
end;

function ThousandSep(Num: UInt64): string;
const
  MaxChar = 30; // Probably, 26 is enough: 19 digits + 7 separators
var
  Count: Integer;
  Rem: UInt64;
  Res: array[0..MaxChar] of Char;
  WritePtr: PChar;
begin
  WritePtr := @Res[MaxChar];
  WritePtr^ := #0;
  Count := 0;
  while Num > 0 do
  begin
    DivMod(Num, 10, Num, Rem);
    Dec(WritePtr);
    WritePtr^ := Char(Byte(Rem) + Ord('0'));
    Inc(Count);
    if Count = 3 then
    begin
      Dec(WritePtr);
      WritePtr^ :=  FormatSettings.ThousandSeparator; //'.';
      Count := 0;
    end;
  end;
  if WritePtr^ = FormatSettings.ThousandSeparator {'.'} then Inc(WritePtr);
  Count := MaxChar - ((NativeInt(WritePtr) - NativeInt(@Res)) shr 1);
  SetLength(Result, Count);
  Move(WritePtr^, PByte(Result)^, Count * SizeOf(Char));
end;

// Convert Size for Sort
// For directories Size value is empty string
// For zero size files Size value is '0'
function SizeToStr(Size: uint64; Typ: Integer): string;
begin
  if (Typ AND FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY
    then Result := ''
    else if Size = 0
      then Result := '0'
      else Result := Format('%s', [ThousandSep(Size)]);
end;


// This function retrieves the last time, the given file was written to disk
function GetLocalTime(ftm: TFileTime):string;
var
  mtm: TSystemTime;
  at : TFileTime;
  //ds, ts:ShortString;
  ds, ts: string;
const
  MAX_DATETIME_STR = 255;
begin
  SetLength(ds, MAX_DATETIME_STR);
  SetLength(ts, MAX_DATETIME_STR);
  // Time must get converted, else there is an error of one hour
  // Does anybody know what this function does ?
  // Maybe something like summertime/wintertime (or what you call it out of Germany) ?
  FileTimeToLocalFileTime(ftm, at);
  FileTimeToSystemTime(at, mtm);

  SetLength(ds, GetDateFormat(LOCALE_USER_DEFAULT, 0, @mtm, NIL, @ds[1], MAX_DATETIME_STR) - 1);
  SetLength(ts, GetTimeFormat(LOCALE_USER_DEFAULT, TIME_NOSECONDS, @mtm, NIL, @ts[1], MAX_DATETIME_STR) - 1);
  Result := ds + '  ' + ts;
end;


////////////////////////////////////////////////////////////////////////////////
// Utility TFileList subclasses
////////////////////////////////////////////////////////////////////////////////

{
// TPATTERNLIST
// add patterns from the specified string
// in format aaa;bbb;ccc etc.
procedure TPatternList.AddPatterns(S: string);
var
  Pattern :string;
  Rest    :string;
  P: Integer;
begin
  if S = '' then Exit; // avoid empty patterns
  Rest := S;
  Pattern := '';
  P := Pos(';',Rest);
  while (Rest <> '') AND (P > 0) do begin
    Pattern := Copy(Rest, 1, P - 1);
    Rest    := Copy(Rest, P + 1, Length(Rest));
    // ensure all paths have an ending backslash
    if Pattern[Length(Pattern)] <> '\' then Pattern := Pattern + '\';
    Add(Pattern);
    P := Pos(';', Rest);
  end;

  if Rest[Length(Rest)] <> '\' then Rest := Rest + '\';
  Add(Rest);
end;
 }

// TTYPEICONLIST
{procedure TTypeIconList.AddType(WinType :string; IconIndex :Integer);
begin
  Add(Format('%s=%d', [WinType, IconIndex]));
end;

function TTypeIconList.IconOf(WinType :string) :Integer;
begin
  var S := Values[WinType];
  if S = '' then S := '-1';
  Result := StrToInt(S);
end;
 }

////////////////////////////////////////////////////////////////////////////////
// CLASS METHODS FOR TSystemFileList
////////////////////////////////////////////////////////////////////////////////

// TFileListView Sorting Routine
class function TFileListView.SortProcFileListView(Item1, Item2: TListItem; Data: Integer): Integer;
var
  Caption1, Caption2: string;
  String1, String2: string;
  ColID, ItemIndex: Integer;
  InvertSort: Boolean;
begin
  Result := 0; // Defaults to equal

  if NOT (TObject(Data) is TFileListView) then raise Exception.Create('Incorrect data in SortProc!');

  var FileListView := TFileListView(Data);
  ColID := FileListView.FSortColumnID;
  ItemIndex := FileListView.ItemIndexByID(ColID);    // Note! for first column/item it returns -1, second - 0
  InvertSort := FileListView.FInvertSort;

  // if ItemIndex is -1, just a sort by NAME is required
  if ItemIndex < 0 then begin // Index=-1 means first column
    // Converts NAME to uppercase to ignore case
    Caption1 := AnsiUpperCase(Item1.Caption);
    Caption2 := AnsiUpperCase(Item2.Caption);
    // Compare NAMES
    if Caption1 > Caption2 then Result := 1
    else if Caption1 < Caption2 then Result := -1;
  end
  else
  begin
    // checks for invalid column specified (1st item)
    if Item1.SubItems.Count < ItemIndex
       then String1 := ''
       else String1 := AnsiUpperCase(Item1.SubItems[ItemIndex]);
    // checks for invalid column specified (2nd item)
    if Item2.SubItems.Count < ItemIndex
       then String2 := ''
       else String2 := AnsiUpperCase(Item2.SubItems[ItemIndex]);

    // compare the selected values
    if String1 > String2 then Result := 1
    else if String1 < String2 then Result := -1
    else begin
    { if String1 = String2 then } // stings are equal, try to sort on Caption
      // Converts NAME to uppercase to ignore case
      Caption1 := AnsiUpperCase(Item1.Caption);
      Caption2 := AnsiUpperCase(Item2.Caption);
      // Compare NAMES
      if Caption1 > Caption2 then Result := 1
      else if Caption1 < Caption2 then Result := -1
    end;
  end;

  // invert Sort if requested
  if InvertSort then Result := -Result; // Result * -1; // is sort reverted ?
end;

constructor TFileListView.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  FAcceptFiles := False;
  FSortColumnID := -1; // no sorting by default
  FInvertSort := False;
  FColumnIDtoInfo := THash<Integer, TFileInfo>.Create;
 // FLastUserDefinedColumn := -1; // no user defined columns
  ViewStyle := vsReport;
  FullDrag := True;

  ResetFileInfoToColumnID;
end;

destructor TFileListView.Destroy;
begin
  // SmallImages.Free; // needed ?
  // LargeImages.Free; // needed ?
  FColumnIDtoInfo.Free;
  inherited Destroy;
end;

procedure TFileListView.Loaded;
begin
  inherited Loaded;
  if NOT (csDesigning IN ComponentState) then begin
    GetSystemImageList;  // get system icon lists (small and large)
    DragAcceptFiles(Handle, FAcceptFiles);
  end;

//  FLastUserDefinedColumn := Columns.Count; // all user defined columns are loaded already, get its count
end;

procedure TFileListView.ResetFileInfoToColumnID;
begin
  for var Inf := Low(FFileInfoToColumnID) to High(FFileInfoToColumnID) do
      FFileInfoToColumnID[Inf] := -1;
end;

procedure TFileListView.WMDROPFILES(var Msg: TWMDropFiles);
var
  i, DropCount, BufSize: integer;
  FileName: pChar;
  FileList: TStrings;
  Point: TPoint;
begin
  BufSize := 0;
  DropCount := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, BufSize);
  FileList := TStringList.Create;
  try
  for i := 0 to DropCount - 1 do begin
    BufSize := DragQueryFile(Msg.Drop, i, nil, BufSize) + 1;
    FileName := StrAlloc(BufSize + 1);
    try
      DragQueryFile(Msg.Drop, i, FileName, BufSize);
      FileList.Add(FileName);
      DragQueryPoint(Msg.Drop, Point);
    finally
      StrDispose(FileName);
    end;
  end;
  DragFinish(Msg.Drop);
  if Assigned(FOnFileDrop) then
    FOnFileDrop(FileList, Point.X, Point.Y)
  finally
    FileList.Free;
  end;
end;

procedure TFileListView.SetAcceptFiles(Accept: Boolean);
begin
  if not (csDesigning in ComponentState) then DragAcceptFiles(Handle, Accept);
  FAcceptFiles := Accept;
end;

function TFileListView.GetSelItemValue(Col: TFileInfo): string;
begin
  Result := '';
  if Selected = nil then exit;
  if Col = fiAll then exit;   // fiAll does not have corresponding column

  if Col = fiName
    then Result := Selected.Caption
    else Result := Selected.SubItems[ItemIndexByInfo(Col)];
end;

procedure TFileListView.GetSystemImageList;
var
  SysImageList: UINT;      // temporary handle for System ImageLists
  ShFileInfo :TShFileInfo; // Shell File Info structure
begin
  LargeImages := TImageList.Create(self);
  SysImageList := ShGetFileInfo('', 0, ShFileInfo, SizeOf(ShFileInfo), SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  if SysImageList <> 0 then begin
    LargeImages.Handle := SysImageList;
    LargeImages.ShareImages := true; // Avoid freeing of System Image List !
  end;
  SmallImages := TImageList.Create(self);
  SysImageList := ShGetFileInfo('', 0, ShFileInfo, SizeOf(ShFileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if SysImageList <> 0 then begin
    SmallImages.Handle := SysImageList;
    SmallImages.ShareImages := true; // Avoid freeing of System Image List !
  end;
end;

procedure TFileListView.SyncColumnData;
var
  Inf: TFileInfo;
  ColID: Integer;
begin
  for Inf := Low(TFileInfo) to Pred(High(TFileInfo)) do begin
    ColID := FFileInfoToColumnID[Inf];
    if (ColID <> -1) AND (Columns.FindItemID(ColID) = nil) then begin
      FFileInfoToColumnID[Inf] := -1;
      FColumnIDtoInfo.Delete(ColID);
    end;
  end;

end;

// Builds columns for ListView based on file fields
// Columns are created as requested by FileColumns parameter
procedure TFileListView.SetupFileColumns(FileColumns: TFileInfos);
var
  Inf :TFileInfo;
  Col: TListColumn;
begin
  try
    SyncColumnData;

    Columns.BeginUpdate;
    for Inf := Low(TFileInfo) to Pred(High(TFileInfo)) do
      if (FFileInfoToColumnID[Inf] = -1) then begin  // if column from FileColumns is not added yet then add it
         if (Inf IN FileColumns) OR (fiAll IN FileColumns) then begin
           Col := Columns.Add;
           Col.Caption   := DefColumnInfos[Inf].Caption;  // fill columns with default values
           Col.Alignment := DefColumnInfos[Inf].Alignment;
           Col.Width     := DefColumnInfos[Inf].Width;
           FFileInfoToColumnID[Inf] := Col.ID;
           FColumnIDtoInfo[Col.ID] := Inf;
         end;
      end else begin // if column is found - check if we should remove it
        if NOT ((Inf IN FileColumns) OR (fiAll IN FileColumns)) then begin
          Columns.Delete(Columns.FindItemID(FFileInfoToColumnID[Inf]).Index);
          FFileInfoToColumnID[Inf] := -1;
          FColumnIDtoInfo.Delete(FFileInfoToColumnID[Inf]);
        end;
      end;

  finally
    Columns.EndUpdate;
  end;
end;

function TFileListView.ItemIndexByInfo(Col: TFileInfo): Integer;
begin
  Result := Columns.FindItemID(FFileInfoToColumnID[Col]).Index - 1;
end;

function TFileListView.ItemIndexByID(ColID: Integer): Integer;
begin
  Result := Columns.FindItemID(ColID).Index - 1;
end;

 // Get Windows file name, system file type and icon
function TFileListView.GetFileShellInfo(FileName: TFileName; var Item: TListItem; var FindData: TWin32FindData): Boolean;
var
  ShFileInfo: TShFileInfo;
  fiNameIndex: Integer;
begin
  ZeroMemory(@ShFileInfo, SizeOf(ShFileInfo));
  var Res := ShGetFileInfo(PChar(FileName), FindData.dwFileAttributes, ShFileInfo, SizeOf(ShFileInfo),
                           SHGFI_USEFILEATTRIBUTES OR SHGFI_TYPENAME OR SHGFI_DISPLAYNAME OR SHGFI_SYSICONINDEX OR SHGFI_SMALLICON { OR SHGFI_ICON } );

  fiNameIndex := ItemIndexByInfo(fiName);
  if Res = 0 then begin // looks like file not found or some other error occurred
    if fiNameIndex = -1
      then Item.Caption := ExtractFileName(FileName) // if fiName is first column then fill its value as Caption
      else Item.SubItems[fiNameIndex] := ExtractFileName(FileName);
    Item.ImageIndex := ShFileInfo.IIcon; // Set file icon index
    if FFileInfoToColumnID[fiType] <> -1 then Item.SubItems[ItemIndexByInfo(fiType)] := 'Unknown file type';
  end
  else
  begin
    if fiNameIndex = -1
      then Item.Caption := ShFileInfo.szDisplayName // if fiName is first column then fill its value as Caption
      else Item.SubItems[fiNameIndex] := ShFileInfo.szDisplayName;
    Item.ImageIndex := ShFileInfo.IIcon;      // Set file icon index
    if FFileInfoToColumnID[fiType] <> -1 then Item.SubItems[ItemIndexByInfo(fiType)] := ShFileInfo.szTypeName;
  end;

  Result := Res <> 0;
end;

// adds a file and returns the newly added item
function TFileListView.AddFile(FileName: TFileName; LoadFileData: Boolean = True): TListItem;
var
  I: Integer;
 // ShFileInfo :TShFileInfo;
  FindData: TWin32FindData;
  FindHandle: THandle;
begin
  Items.BeginUpdate;
  try
    Result := Items.Add;

    // add as many empty subitems as number of columns (excluding first one)
    for I := 1 to Columns.Count - 1 do Result.Subitems.Add('');

    if LoadFileData then begin

      ZeroMemory(@FindData, sizeof(FindData));
      FindHandle := Windows.FindFirstFile(PChar(FileName), FindData);

      try
        if FindHandle <> INVALID_HANDLE_VALUE then begin
          GetFileShellInfo(FileName, Result, FindData);

          if FFileInfoToColumnID[fiSize]       <> -1 then
            Result.SubItems[ItemIndexByInfo(fiSize)] := SizeToStr(MakeFileSize(FindData.nFileSizeHigh, FindData.nFileSizeLow), FindData.dwFileAttributes);
          if FFileInfoToColumnID[fiDosName]    <> -1 then
            Result.SubItems[ItemIndexByInfo(fiDosName)] := IfThen(FindData.cAlternateFileName <> '', FindData.cAlternateFileName, FindData.cFileName);
          if FFileInfoToColumnID[fiDosExt]     <> -1 then
            Result.SubItems[ItemIndexByInfo(fiDosExt)] := ExtractFileExt(IfThen(FindData.cAlternateFileName <> '', FindData.cAlternateFileName, FindData.cFileName));
          if FFileInfoToColumnID[fiModified]   <> -1 then
            Result.SubItems[ItemIndexByInfo(fiModified)] := GetLocalTime(FindData.ftLastWriteTime);
          if FFileInfoToColumnID[fiCreated]    <> -1 then
            Result.SubItems[ItemIndexByInfo(fiCreated)] := GetLocalTime(FindData.ftCreationTime);
          if FFileInfoToColumnID[fiLastAccess] <> -1 then
            Result.SubItems[ItemIndexByInfo(fiLastAccess)] := GetLocalTime(FindData.ftLastAccessTime);
          if FFileInfoToColumnID[fiAttributes] <> -1 then
            Result.SubItems[ItemIndexByInfo(fiAttributes)] := AttrStr(FindData.dwFileAttributes);
          if FFileInfoToColumnID[fiPath] <> -1 then
            Result.SubItems[ItemIndexByInfo(fiPath)] := FileName;
        end;
      finally
        Windows.FindClose(FindHandle);
      end;
    end
    else
    begin
      var fiNameIndex := ItemIndexByInfo(fiName);
      if fiNameIndex = -1
        then Result.Caption := ExtractFileName(FileName) // if fiName is first column then fill its value as Caption
        else Result.SubItems[fiNameIndex] := ExtractFileName(FileName);

      //Result.Caption := ExtractFileName(FileName);
      Result.ImageIndex := DefDirShellInfo.IIcon; // Set default item icon index
      if FFileInfoToColumnID[fiType]      <> -1 then Result.SubItems[ItemIndexByInfo(fiType)]      := '-';
      if FFileInfoToColumnID[fiSize]      <> -1 then Result.SubItems[ItemIndexByInfo(fiSize)]      := '-';
      if FFileInfoToColumnID[fiDosName]   <> -1 then Result.SubItems[ItemIndexByInfo(fiDosName)]   := '-';
      if FFileInfoToColumnID[fiDosExt]    <> -1 then Result.SubItems[ItemIndexByInfo(fiDosExt)]    := '-';
      if FFileInfoToColumnID[fiModified]  <> -1 then Result.SubItems[ItemIndexByInfo(fiModified)]  := '-';
      if FFileInfoToColumnID[fiCreated]   <> -1 then Result.SubItems[ItemIndexByInfo(fiCreated)]   := '-';
      if FFileInfoToColumnID[fiLastAccess]<> -1 then Result.SubItems[ItemIndexByInfo(fiLastAccess)]:= '-';
      if FFileInfoToColumnID[fiAttributes]<> -1 then Result.SubItems[ItemIndexByInfo(fiAttributes)]:= '-';
      if FFileInfoToColumnID[fiPath]      <> -1 then Result.SubItems[ItemIndexByInfo(fiPath)]      := '-';
    end;

  finally
    Items.EndUpdate;
  end;
end;

function TFileListView.AddFile(FullPath: string; FileData: TWin32FindData): TListItem;
var
  i: Integer;
begin
  Result := Items.Add;
  // add as many empty subitems as number of columns (excluding first one)
  for i := 1 to Columns.Count - 1 do Result.SubItems.Add('');

  GetFileShellInfo(FullPath, Result, FileData); // this call fills items fiName and fiType, so we dont need to fill then in below

  if FFileInfoToColumnID[fiSize]       <> -1 then Result.SubItems[ItemIndexByInfo(fiSize)]      := SizeToStr(MakeFileSize(FileData.nFileSizeHigh, FileData.nFileSizeLow), FileData.dwFileAttributes);
  if FFileInfoToColumnID[fiDosName]    <> -1 then Result.SubItems[ItemIndexByInfo(fiDosName)]   := IfThen(FileData.cAlternateFileName <> '', FileData.cAlternateFileName, FileData.cFileName);
  if FFileInfoToColumnID[fiDosExt]     <> -1 then Result.SubItems[ItemIndexByInfo(fiDosExt)]    := ExtractFileExt(IfThen(FileData.cAlternateFileName <> '', FileData.cAlternateFileName, FileData.cFileName));
  if FFileInfoToColumnID[fiModified]   <> -1 then Result.SubItems[ItemIndexByInfo(fiModified)]  := GetLocalTime(FileData.ftLastWriteTime);
  if FFileInfoToColumnID[fiCreated]    <> -1 then Result.SubItems[ItemIndexByInfo(fiCreated)]   := GetLocalTime(FileData.ftCreationTime);
  if FFileInfoToColumnID[fiLastAccess] <> -1 then Result.SubItems[ItemIndexByInfo(fiLastAccess)]:= GetLocalTime(FileData.ftLastAccessTime);
  if FFileInfoToColumnID[fiPath]       <> -1 then Result.SubItems[ItemIndexByInfo(fiPath)]      := FullPath;
  if FFileInfoToColumnID[fiAttributes] <> -1 then Result.SubItems[ItemIndexByInfo(fiAttributes)] := AttrStr(FileData.dwFileAttributes);
end;

procedure TFileListView.ColClick(Column: TListColumn);
begin
  SetSortColumn(Column);
  inherited ColClick(Column);
end;

// Defines sort column and makes actual sorting
procedure TFileListView.SetSortColumn(Column: TListColumn); //TODO: add column sorting marks here???
begin
  ClearSortingMark(FSortColumnID);

  // invert sorting if the specified column was already selected
  if Column.ID = FSortColumnID then
    FInvertSort := not FInvertSort
  else begin
    FSortColumnID := Column.ID;
    FInvertSort := False;
  end;

  if ViewStyle = vsReport then begin
    CustomSort(@SortProcFileListView, Integer(self));
  end;

  SetSortingMark(FSortColumnID, NOT FInvertSort);
end;

procedure TFileListView.ClearSortingMark(ColID: Integer);
var
 hdr: HWND;
 Item: THDItem;
 res: LongBool;
begin
  if FSortColumnID = -1 then exit;

  hdr := ListView_GetHeader(Handle);

  // clear sorting mark of current column
  FillChar(Item, sizeof(Item), 0);
  Item.Mask := HDI_FORMAT;
  res := Header_GetItem(hdr, ColID, Item);
  Item.Mask := Item.Mask OR HDI_FORMAT;
  Item.fmt  := Item.fmt AND (NOT (HDF_SORTDOWN OR HDF_SORTUP));  // clear HDF_SORTDOWN and HDF_SORTUP bits in Item.fmt
  res := Header_SetItem(hdr, ColID, Item);
end;

procedure TFileListView.SetSortingMark(ColID: Integer; Ascending: Boolean);
var
 hdr: HWND;
 Item: THDItem;
 res: LongBool;
begin
  hdr := ListView_GetHeader(Handle);

  // set sorting mark to the new sorted column
  FillChar(Item, sizeof(Item), 0);
  Item.Mask := HDI_FORMAT;
  res := Header_GetItem(hdr, ColID, Item);
  Item.Mask := Item.Mask OR HDI_FORMAT;
  Item.fmt  := Item.fmt AND (NOT (HDF_SORTDOWN OR HDF_SORTUP));  // clear HDF_SORTDOWN and HDF_SORTUP bits in Item.fmt
  Item.fmt  := Item.fmt  OR IfThen(Ascending, HDF_SORTUP, HDF_SORTDOWN); // set required bit
  res := Header_SetItem(hdr, ColID, Item);
end;

////////////////////////////////////////////////////////////////////////////////
// TFileList
////////////////////////////////////////////////////////////////////////////////

// TFileList Sorting Routine
class function TFileList.SortProcFileList(Item1, Item2: TListItem; Data: Integer): Integer;
var
  Caption1, Caption2: string;
  String1, String2: string;
  //ColType: TFileInfo;
  FileDataP1, FileDataP2: ^TWin32FindData;
begin
  if NOT (TObject(Data) is TFileList) then raise Exception.Create('Incorrect data in SortProc!');

  Result := 0; // Defaults to equal
  var FL := TFileList(Data);

  if Item1.Caption = DDOTD then Result := -1  // '..' directory is always on top
  else if Item2.Caption = DDOTD then Result := 1
  else begin
    // Tests Column[1] (Item1.SubItems[0], Size) to force Directories before Files
    if IsDirectory(Item1) AND (NOT IsDirectory(Item2)) then Result := -1
    else if NOT IsDirectory(Item1) AND IsDirectory(Item2) then Result := 1 // file is always "greater" than directory
    else begin
      // both items are directory or file
      //ColType := FL.FSortColumnType;
      //ItemIndex := FL.ItemIndexByID(FL.FSortColumnID); // Note! for first column/item it returns -1, second - 0
//      InvertSort := FL.FInvertSort;


      case FL.FSortColumnType of
        fiName: begin  // Converts NAME to uppercase to ignore case
          Caption1 := AnsiUpperCase(Item1.Caption);
          Caption2 := AnsiUpperCase(Item2.Caption);

          if Caption1 > Caption2 then Result := 1
          else if Caption1 < Caption2 then Result := -1;
        end;

        fiSize: begin
          FileDataP1 := FL.FData.GetValuePointer(Item1);
          FileDataP2 := FL.FData.GetValuePointer(Item2);

          var size1 := MakeFileSize(FileDataP1.nFileSizeHigh, FileDataP1.nFileSizeLow);
          var size2 := MakeFileSize(FileDataP2^.nFileSizeHigh, FileDataP2^.nFileSizeLow);

          if size1 > size2 then Result := 1
          else if size1 < size2 then Result := -1;
        end;

        fiType, fiAttributes, fiDosName, fiDosExt, fiPath: begin
          if Item1.SubItems.Count < Ord(FL.FSortColumnType) //ItemIndex
            then String1 := ''
            else String1 := AnsiUpperCase(Item1.SubItems[Ord(FL.FSortColumnType) - 1]); //ItemIndex]);

          // checks for invalid column specified (2nd item)
          if Item2.SubItems.Count < Ord(FL.FSortColumnType) //ItemIndex
            then String2 := ''
            else String2 := AnsiUpperCase(Item2.SubItems[Ord(FL.FSortColumnType) - 1]); //ItemIndex]);

          // compare the selected values
          if String1 > String2 then Result := 1
          else if String1 < String2 then Result := -1
          else begin
            //if String1 = String2 then stings are equal, try to sort on Caption
            Caption1 := AnsiUpperCase(Item1.Caption);
            Caption2 := AnsiUpperCase(Item2.Caption);
            // Compare NAMES
            if Caption1 > Caption2 then Result := 1
            else if Caption1 < Caption2 then Result := -1
          end;
        end;

        fiModified: begin
          FileDataP1 := FL.FData.GetValuePointer(Item1);
          FileDataP2 := FL.FData.GetValuePointer(Item2);
          // Date-Time field sorted in reverse order by default
          Result := -CompareFileTime(FileDataP1^.ftLastWriteTime, FileDataP2^.ftLastWriteTime);
        end;

        fiCreated: begin
          FileDataP1 := FL.FData.GetValuePointer(Item1);
          FileDataP2 := FL.FData.GetValuePointer(Item2);
          // Date-Time field sorted in reverse order by default
          Result := -CompareFileTime(FileDataP1^.ftCreationTime, FileDataP2^.ftCreationTime);
        end;

        fiLastAccess: begin
          FileDataP1 := FL.FData.GetValuePointer(Item1);
          FileDataP2 := FL.FData.GetValuePointer(Item2);
          // Date-Time field sorted in reverse order by default
          Result := -CompareFileTime(FileDataP1^.ftLastAccessTime, FileDataP2^.ftLastAccessTime);
        end;

      //  fiAll: ;
      end;
     {
      // if ColIndex < 0, just a sort by NAME is required
      //if ItemIndex < 0 then begin // Index=-1 means first column
      if ColType = fiName then begin
        // Converts NAME to uppercase to ignore case
        Caption1 := AnsiUpperCase(Item1.Caption);
        Caption2 := AnsiUpperCase(Item2.Caption);

        if Caption1 > Caption2 then Result := 1
        else if Caption1 < Caption2 then Result := -1;
      end
      else
      begin
        // checks for invalid column specified (1st item)
        if Item1.SubItems.Count < Ord(ColType) //ItemIndex
        	then String1 := ''
        	else String1 := AnsiUpperCase(Item1.SubItems[Ord(ColType) - 1]); //ItemIndex]);

        // checks for invalid column specified (2nd item)
        if Item2.SubItems.Count < Ord(ColType) //ItemIndex
        	then String2 := ''
        	else String2 := AnsiUpperCase(Item2.SubItems[Ord(ColType) - 1]); //ItemIndex]);

        // compare the selected values
        if String1 > String2 then Result := 1
        	else if String1 < String2 then Result := -1
          else begin
	          //if String1 = String2 then stings are equal, try to sort on Caption
            Caption1 := AnsiUpperCase(Item1.Caption);
            Caption2 := AnsiUpperCase(Item2.Caption);
            // Compare NAMES
            if Caption1 > Caption2 then Result := 1
            	else if Caption1 < Caption2 then Result := -1
          end;
      end;
      }

      // invert Sort if requested
      if FL.FInvertSort then Result := -Result; // is sort reverted ?

      // Date-Time field sorted in reverse order by default
      //if ColType = fiCreated then	Result := -Result
      //else if ColType = fiModified then	Result := -Result
      //else if ColType = fiLastAccess then	Result := -Result;
    end;
  end;
end;

constructor TFileList.Create(AOwner :TComponent);
begin
  inherited Create(AOwner);
  FDisplayDirs := True;   // include directory in display
  FFileTypes  := [ftArchive, ftReadonly]; // default searched file type
  FSortColumnType := fiName; // set default sort to Name
  FSortColumnID := Ord(fiName); // TODO: that's might be not true, need to check
  FViewColumns := [fiName, fiSize, fiType, fiModified]; // columns useb by Explorer

  FData := THash<TListItem, TWin32FindData>.Create;

  //FDirectory := TPatternList.Create;   // Search Path list
  //FDirectory.Sorted := true;          // sorted and
  //FDirectory.Duplicates := dupIgnore; // ignore duplicates

  FFileMask := TStringList.Create;     // File Specifications list
  FFileMask.Sorted := True;            // sorted and
  FFileMask.Duplicates := dupIgnore;   // ignore duplicates
  FFileMask.Add('*');
end;

procedure TFileList.Loaded;
begin
  inherited Loaded;
 // if NOT (csLoading in ComponentState) then showmessage('TFileList.Loaded');
  SetupFileColumns;  // build listview columns
  UpdateFileList;
end;

destructor TFileList.Destroy;
begin
  FreeAndNil(FFileMask);  // releases child component
  FreeAndNil(FData);
 // FDirectory.Free; // releases child component
  inherited Destroy;
end;

procedure TFileList.SetColCaption(Col: TFileInfo; Value: string);
begin
  var Item := Columns.FindItemID(FFileInfoToColumnID[Col]);
  if Assigned(Item) then (Item as TListColumn).Caption := Value;
end;

procedure TFileList.SetColWidth(Col: TFileInfo; Value :Integer);
begin
  var Item := Columns.FindItemID(FFileInfoToColumnID[Col]);
  if Assigned(Item) then (Item as TListColumn).Width := Value;
  //if not (csReading in ComponentState) then SetupFileColumns; // V1.1: needed if changing widths when component visible
end;

// Builds columns for ListView
// NAME and SIZE column always created and just hidden if not
// required. Other columns are created only if requested
procedure TFileList.SetupFileColumns;
var
  Inf: TFileInfo;
begin
 if csReading in ComponentState then exit; // Do not update FileList while reading properties from .dfm file.
  //ShowMessage('TFileList.SetupFileColumns');
  try
    Columns.BeginUpdate;

    with Columns do begin
      Clear;
      //SyncColumnData;
      ResetFileInfoToColumnID;
      //FColumnIDtoInfo.Clear;

      // NAME
      with Add do begin // adds and sets up NAME column
        Caption := DefColumnInfos[fiName].Caption;
        Alignment := DefColumnInfos[fiName].Alignment;
        FFileInfoToColumnID[fiName] := ID;
        FColumnIDtoInfo.SetValue(ID, fiName);
        if (fiName in FViewColumns) or (fiAll in FViewColumns)
           then Width := DefColumnInfos[fiName].Width
          else Width := 0; // Hide column if not required
      end;

      // SIZE - we need SIZE column always added to detemine if item is directory or not
      with Add do begin // adds and sets up SIZE column
        Caption := DefColumnInfos[fiSize].Caption;
        Alignment := DefColumnInfos[fiSize].Alignment;
        FFileInfoToColumnID[fiSize] := ID;
        FColumnIDtoInfo.SetValue(ID, fiSize);
        if (fiSize in FViewColumns) or (fiAll in FViewColumns)
           then Width := DefColumnInfos[fiSize].Width
           else Width := 0; // Hide column if not required
      end;

      // all remaining columns
      for Inf := fiType to Pred(fiAll) do
        if (Inf in FViewColumns) or (fiAll in FViewColumns) then begin
          with Add do begin // adds and sets up other columns
            Caption := DefColumnInfos[Inf].Caption;
            Alignment := DefColumnInfos[Inf].Alignment;
            Width := DefColumnInfos[Inf].Width;
            FFileInfoToColumnID[Inf] := ID;
            FColumnIDtoInfo.SetValue(ID, Inf);
          end;
        end
        else
        begin
          FFileInfoToColumnID[Inf] := -1;
        end;
    end; { with columns do }
  finally
    Columns.EndUpdate;
  end;
end;

procedure TFileList.SetupFileColumns(FileColumns: TFileInfos);
begin
  FViewColumns := FileColumns;
  SetupFileColumns;
end;

////////////////////////////////////////////////////////////////////////////////
// EVENT HANDLERS
////////////////////////////////////////////////////////////////////////////////

// overrides ancestor's handler for column headers click:
// calls first the Set Sort Column procedure and then
// the inherited ColClick method.
procedure TFileList.ColClick(Column: TListColumn);
begin
  SetSortColumn(FColumnIDtoInfo[Column.ID]);
  inherited ColClick(Column); // ancestor's SetSortColumn method is "turned off' (see below)
end;

procedure TFileList.SetSortColumn(ColType: TListColumn);
begin
// "turn off" this method, make it do nothing
// avoid calling it from inherited ColClick(Column);
end;

// Set current sort column
procedure TFileList.SetSortColumn(ColType: TFileInfo);
var
 hdr: HWND;
 Item: THDItem;
 //ColIndex: Integer;
 begin
  if not ((ColType in FViewColumns) or (fiAll in FViewColumns)) then Exit; // prevent unused columns to be set for sorting
  if ColType = fiAll then Exit; // fiAll is not a valid sort column

  ClearSortingMark(FSortColumnID);

  {hdr := ListView_GetHeader(Handle);

  // clear sorting mark of current column
  FillChar(Item, sizeof(Item), 0);
  Item.Mask := HDI_FORMAT;
  //ColIndex := Columns.FindItemID(FSortColumnID).Index;
  Header_GetItem(hdr, FSortColumnID, Item);
  Item.Mask := Item.Mask OR HDI_FORMAT;
  Item.fmt  := Item.fmt AND (NOT (HDF_SORTDOWN OR HDF_SORTUP));  // clear HDF_SORTDOWN and HDF_SORTUP bits in Item.fmt
  Header_SetItem(hdr, FSortColumnID, Item);
   }
  // invert sorting if the specified column was already selected
  if ColType = FSortColumnType then begin
    FInvertSort := not FInvertSort;
  end else begin
    FSortColumnType := ColType;
    FSortColumnID := FFileInfoToColumnID[ColType];
    FInvertSort := False;
  end;

  if ViewStyle = vsReport then begin
    CustomSort(@SortProcFileList, Integer(self));
  end;

  SetSortingMark(FSortColumnID, NOT FInvertSort);
  {
  // set sorting mark to the new sorted column
  FillChar(Item, sizeof(Item), 0);
  Item.Mask := HDI_FORMAT;
  //ColIndex := Columns.FindItemID(FSortColumnID).Index;
  Header_GetItem(hdr, FSortColumnID, Item);
  Item.Mask := Item.Mask OR HDI_FORMAT;
  Item.fmt  := Item.fmt AND (NOT (HDF_SORTDOWN OR HDF_SORTUP));  // clear HDF_SORTDOWN and HDF_SORTUP bits in Item.fmt
  Item.fmt  := Item.fmt  OR IfThen(FInvertSort, HDF_SORTDOWN, HDF_SORTUP); // set required bit
  Header_SetItem(hdr, FSortColumnID, Item);
  }
end;


procedure TFileList.DblClick;
begin
  var item := Selected;
  if (item <> nil) AND IsDirectory(item) then begin
    if item.Caption = DDOTD then begin
      var pos := FDirectory.Length - 1;
      while (pos > 0) AND (FDirectory[pos] <> '\') do Dec(pos);
      SetDirectory(Copy(FDirectory, 1, pos));
    end else begin
      SetDirectory(Directory + item.Caption + '\');
    end;
  end;

  inherited DblClick;
end;

////////////////////////////////////////////////////////////////////////////////
// PROPERTIES INTERFACE
////////////////////////////////////////////////////////////////////////////////

// Set File Attributes
procedure TFileList.SetFileTypes(FT: TFileTypes);
begin
  if FT = FFileTypes then Exit;
  FFileTypes := FT;
  UpdateFileList;
end;

// Set File Specification
procedure TFileList.SetMask(Mask: string);
begin
  SetFilters(Mask, FFileMask, True, False);
  UpdateFileList;
end;

// Add specifications to File Specifications list
procedure TFileList.AddMask(Mask: string);
begin
  SetFilters(GetMask + ';' + Mask, FFileMask, True, False);
  UpdateFileList; // Update File List View, apply new mask
end;


// Get File Specifications list
function TFileList.GetMask :string;
var
  I :integer;
begin
  Result := ''; // Default result to ''
  for I := 0 to Pred(FFileMask.Count) do
    Result := Result + FFileMask[I]  + ';';
  if Result[Length(Result)] = ';' then // remove last ';'
    Result := Copy(Result, 1, Length(Result) - 1);
end;

procedure TFileList.SetDirectory(NewDir: string);
begin
  if NewDir.IsEmpty OR (NewDir[NewDir.Length] = '\')
    then FDirectory := NewDir
    else FDirectory := NewDir + '\';

  UpdateFileList;
end;

// Set Search Path List
// several dirs can be separated by ';' in Path string
{procedure TFileList.SetPath(Path: string);
begin
  FDirectory.Clear; // Clear search path list
  FDirectory.AddPatterns(Path);
  if NOT (csReading in ComponentState) then UpdateFileList; // Do not update FileList while reading properties from .dfm file.
end;

// Add specifications to Search Path List
procedure TFileList.AddPath(Path: string);
begin
  FDirectory.AddPatterns(Path);
  UpdateFileList; // Update File List View
  // modificare per caricre solo da Path
end;

// Get Search Path List
function TFileList.GetPath: string;
var
  I :integer;
begin
  Result := ''; // Default result to ''
  if FDirectory.Count < 1 then Exit;
  for I := 0 to Pred(FDirectory.Count) do
    Result := Result + FDirectory[I]  + ';';
  if Result[Length(Result)] = ';' then // remove last ';'
    Result := Copy(Result, 1, Length(Result) - 1);
end;
}

// Set current column set
procedure TFileList.SetViewColumns(Columns: TFileInfos);
begin
  if Columns = FViewColumns then Exit;

  if fiALL in Columns
    then FViewColumns := [fiName, fiSize, fiType, fiModified, fiCreated, fiLastAccess, fiAttributes, fiDosName, fiDosExt, fiPath, fiAll]
    else FViewColumns := Columns;

  if ViewStyle = vsReport then begin
    SetupFileColumns; // rebuild listview columns
    UpdateFileList;
  end;
end;

// Allows/Prevent loading of Directory Items in the file list
procedure TFileList.SetDisplayDirs(Display: Boolean);
begin
  if Display = FDisplayDirs then Exit;
  FDisplayDirs := Display;
  UpdateFileList;
end;

// Allows/Prevent loading of Parent Directory Item in the file list
procedure TFileList.SetDisplayParentDir (Display: Boolean);
begin
  if Display = FDisplayParentDir then Exit;
  FDisplayParentDir := Display;
  if FDisplayDirs then UpdateFileList; // if currently showing directories then update FileList
end;

function TFileList.AddFile(FileName: TFileName): TListItem;
var
  FindHandle :THandle;
  FindData :TWin32FindData;
  path :string;
begin
  Result := nil;
  FindHandle := Windows.FindFirstFile(PChar(FileName), FindData);

  try
    if (FindHandle <> INVALID_HANDLE_VALUE) then begin
      path := ExtractFilePath(FileName);
      if path[Length(path)] <> '\' then path := path + '\';

      Result := Items.Add;
      Result.Caption := FindData.cFileName;
      AddFileData(Result, path, FindData);
    end;
  finally
    Windows.FindClose(FindHandle);
 end;
end;

// Add a file entry to the list view. The FindData structure should be
// returned from a FindFirstFile/FindNextFile call
procedure TFileList.AddFileData(NewItem: TListItem; Path: string; FindData: TWin32FindData);
var
  ShFileInfo: TShFileInfo;
  FileSize: uint64;
 // IsDir: Boolean;
begin
  //IsDir := (FindData.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY) > 0;
  // DIRECTORY IMAGE INDEX
  //if IsDir then NewItem.ImageIndex := DirShellInfo.iIcon;

  // SIZE
  FileSize := MakeFileSize(FindData.nFileSizeHigh, FindData.nFileSizeLow);
  NewItem.SubItems.Add(SizeToStr(FileSize, FindData.dwFileAttributes));

  // TYPE
  if (fiType in FViewColumns) OR (fiAll in FViewColumns) then begin
    //if IsDir then begin
    // NewItem.SubItems.Add(DirShellInfo.szTypeName) // type
    //end else begin
    ShGetFileInfo(PChar(Path + FindData.cFileName), 0, ShFileInfo, SizeOf(ShFileInfo),
                  SHGFI_DISPLAYNAME or SHGFI_TYPENAME OR SHGFI_SYSICONINDEX OR SHGFI_SMALLICON);
    NewItem.SubItems.Add(ShFileInfo.szTypeName); // type
    NewItem.ImageIndex := ShFileInfo.iIcon;
    //end;
  end;

  // MODIFIED DATE, add subitem only if requested
  if (fiModified in FViewColumns) or (fiAll in FViewColumns) then
    NewItem.SubItems.Add(GetLocalTime(FindData.ftLastWriteTime));
  // CREATION DATE, add subitem only if requested
  if (fiCreated in FViewColumns) or (fiAll in FViewColumns) then
    NewItem.SubItems.Add(GetLocalTime(FindData.ftCreationTime));
  // LAST ACCESS, add subitem only if requested
  if (fiLastAccess in FViewColumns) or (fiAll in FViewColumns) then
    NewItem.SubItems.Add(GetLocalTime(FindData.ftLastAccessTime));
  // ATTRIBUTES, add subitem only if requested
  if (fiAttributes in FViewColumns) or (fiAll in FViewColumns) then
    NewItem.SubItems.Add(AttrStr(FindData.dwFileAttributes));
  // DOS NAME, add subitem only if requested
  if (fiDosName in FViewColumns) or (fiAll in FViewColumns) then
    NewItem.SubItems.Add(IfThen(FindData.cAlternateFileName <> '', FindData.cAlternateFileName, FindData.cFileName)); // avoid empty DOS name
  // DOS EXTENSION, add subitem only if requested
  if (fiDosExt in FViewColumns) or (fiAll in FViewColumns) then
    NewItem.SubItems.Add(ExtractFileExt(IfThen(FindData.cAlternateFileName <> '', FindData.cAlternateFileName, FindData.cFileName))); // avoid empty DOS ext
  // PATH, add subitem only if requested
  if (fiPath in FViewColumns) or (fiAll in FViewColumns) then
    NewItem.SubItems.Add(Path);

  FData.SetValue(NewItem, FindData); // store original data to be able to sort it properly

	if Assigned(FOnFileAdd) then FOnFileAdd(self, NewItem, Path, FindData); // user callback for OnAddFile
end;

// Update File List with contents of the directory specified in Directory property
procedure TFileList.UpdateFileList;

  // search single directory
  procedure GetDirList(Dir: string);
  var
    Ret: Boolean;
    FindHandle: THandle;
    FindData: TWin32FindData;
    fn: string;
    ErrMode: Integer;
    NewItem: TListItem;
  begin
    ErrMode := SetErrorMode(SEM_FAILCRITICALERRORS);// set error handler

    FindHandle := Windows.FindFirstFile(PChar(Dir + '*'), FindData);
    Ret := (FindHandle <> INVALID_HANDLE_VALUE);
    try
      while Ret do begin
      	fn := FindData.cFileName;
        if fn <> DOTD then begin	// bypass "." directory
          if fn = DDOTD then begin // add 'parent directory' only if required by DisplayParentDir property
            if FDisplayDirs AND FDisplayParentDir then begin
              NewItem := Items.Add;
              NewItem.Caption := fn; // name '..'
              AddFileData(NewItem, Dir, FindData);
            end;
          end else begin
            if CheckAttributes(FindData.dwFileAttributes, FFileTypes) then begin // first filter is by attributes
              if (FindData.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY) > 0 then begin
                if FDisplayDirs then	 // add directory entries only if required by DisplayDirectory property.
                  if CmpMask(AnsiUpperCase(FindData.cFileName), FFileMask, True, False) then begin // match directory with mask
                    NewItem := Items.Add;
                    NewItem.Caption := fn;
                    AddFileData(NewItem, Dir, FindData);
                  end;
              end else begin // item is filename, test if file name matches mask
                if CmpMask(AnsiUpperCase(FindData.cFileName), FFileMask, True, False) then begin
                  NewItem := Items.Add;
                  NewItem.Caption := fn;
                  AddFileData(NewItem, Dir, FindData); // adds matching file entry
                end;
              end;
            end;
          end;
        end;

        Ret := Windows.FindNextFile(FindHandle, FindData) // get next entry
      end;
    finally
      Windows.FindClose(FindHandle); // Close FindNext context
      SetErrorMode(ErrMode); // Reset error handler
    end;
  end;

begin
  if (csReading in ComponentState) OR (csDesigning in ComponentState) then exit; // Do not update FileList while reading properties from .dfm file or while designing component in IDE.

  Items.BeginUpdate;  // prevents video refresh until end of list loading
  try
    Screen.Cursor := crHourGlass; // set cursor shape
    Items.Clear;  // clear list view
    FData.Clear;
    if FDirectory.IsEmpty() then exit; // Clear items and do nothing if Directory is empty

    GetDirList(FDirectory);
    CustomSort(@SortProcFileList, Integer(self));	// sort items by current sort item, default by fiName

    if Assigned(FOnReadDirectory) then FOnReadDirectory(self, FDirectory); // user callback for OnReadDirectory
  finally
    Items.EndUpdate; // finally updates visual control
    Screen.Cursor := crDefault; // reset cursor
  end;
end;


////////////////////////////////////////////////////////////////////////////////
// class registration
////////////////////////////////////////////////////////////////////////////////
procedure Register;
begin
  RegisterComponents('Win32', [TFileView, TFileList]);
end;

initialization
  SetLength(WinDir, MAX_PATH);
  GetWindowsDirectory(@(WinDir[1]), MAX_PATH);
  ShGetFileInfo(@(WinDir[1]), 0, DefDirShellInfo, SizeOf(DefDirShellInfo), SHGFI_SYSICONINDEX OR SHGFI_TYPENAME OR SHGFI_SMALLICON);
end.
