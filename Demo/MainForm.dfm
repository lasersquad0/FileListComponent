object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'FileList Components Demo Program'
  ClientHeight = 482
  ClientWidth = 905
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 0
    Top = 226
    Width = 905
    Height = 6
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
    ExplicitTop = 236
    ExplicitWidth = 868
  end
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 5
    Width = 899
    Height = 17
    Margins.Top = 5
    Align = alTop
    Caption = 'TFileView'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitWidth = 60
  end
  object FileView1: TFileView
    Left = 0
    Top = 25
    Width = 905
    Height = 201
    Align = alClient
    BorderStyle = bsNone
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    AcceptFiles = False
    Columns = <
      item
        Caption = 'NName'
      end
      item
        Caption = 'SSize'
      end
      item
        Caption = 'TType'
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 232
    Width = 905
    Height = 250
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    object Label2: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 5
      Width = 899
      Height = 17
      Margins.Top = 5
      Align = alTop
      Caption = 'TFileList'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 52
    end
    object FileList1: TFileList
      Left = 0
      Top = 25
      Width = 905
      Height = 206
      Align = alClient
      BorderStyle = bsNone
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      AcceptFiles = False
      Directory = 'c:\'
      FileTypes = [ftArchive, ftReadonly, ftSystem, ftHidden, ftCompressed, ftTemporary]
      DisplayParentDir = True
      Mask = '*'
      OnReadDirectory = FileList1ReadDirectory
      SortColumn = fiSize
      ViewColumns = [fiName, fiSize, fiType, fiModified, fiCreated, fiAttributes, fiPath]
    end
    object StatusBar1: TStatusBar
      Left = 0
      Top = 231
      Width = 905
      Height = 19
      Panels = <
        item
          Text = '   TFileView items count - 000'
          Width = 200
        end
        item
          Text = 'TFileList items count - 000'
          Width = 150
        end>
    end
  end
end
