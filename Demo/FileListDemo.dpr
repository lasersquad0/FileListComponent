program FileListDemo;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  MaskSearch in '..\MaskSearch.pas',
  DynamicArray in '..\DynamicArray.pas',
  Hash in '..\Hash.pas',
  FileList in '..\FileList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
