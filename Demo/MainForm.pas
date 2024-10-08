unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, FileList, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    FileView1: TFileView;
    Splitter1: TSplitter;
    Label1: TLabel;
    Panel1: TPanel;
    FileList1: TFileList;
    Label2: TLabel;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FileList1ReadDirectory(Sender: TObject; Path: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FileList1ReadDirectory(Sender: TObject; Path: string);
begin
  StatusBar1.Panels[1].Text := 'TFileList items count - ' + IntToStr(FileList1.GetCount);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FileView1.SetupFileColumns([fiName, fiSize, fiType, fiModified, fiAttributes, fiPath]);
  FileView1.AddFile('c:\Users', True);
  FileView1.AddFile('c:\Windows', True);
  FileView1.AddFile('c:\Program Files', True);
  FileView1.AddFile('c:\ProgramData', True);
  FileView1.AddFile('c:\Users\Public', True);
  FileView1.AddFile('c:\Users\Public\Desktop', True);
  FileView1.AddFile('c:\Recovery', True);
  FileView1.AddFile('c:\pagefile.sys', True);
  FileView1.AddFile('c:\hiberfil.sys', True);
  FileView1.AddFile('c:\swapfile.sys', True);
  StatusBar1.Panels[0].Text := 'TFileView items count - ' + IntToStr(FileView1.GetCount);

  FileView1.Items[0].SubItems[0] := '1Size1Size1Size';
  FileView1.Items[0].Caption := '1Name1Name1Name';
  FileView1.Items[1].SubItems[0] := '2Size2Size2Size';
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  FileList1.SetColCaption(fiName, 'Another Name');
  FileList1.SetColCaption(fiCreated, 'AgainCrtd');
  FileList1.SetColWidth(fiName, 150);
  FileList1.SetColWidth(fiCreated, 150);
end;

end.
