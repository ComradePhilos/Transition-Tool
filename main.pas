unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Buttons, ComCtrls, INIFiles;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnStart: TBitBtn;
    CheckBox1: TCheckBox;
    DateEdit1: TDateEdit;
    DateEdit2: TDateEdit;
    EdtSource: TDirectoryEdit;
    EdtDestination: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MemoPrefix: TMemo;
    ProgressBar: TProgressBar;
    procedure BtnStartClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FPrefixes: TStringList;
    procedure LoadIni;
    procedure SaveIni;
    procedure Start(CheckDates: Boolean);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

const
  ProgrammVersion = 1.1;
  ProgrammTitle = 'Transition Tool';
  ProgrammAuthor = 'Philip Märksch';

  txtNoPrefix = 'Kann nicht starten: Keine Präfixe angegeben!';

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FPrefixes := TStringList.Create;
  self.Constraints.MinHeight := self.Height;
  self.Constraints.MinWidth := self.Width;
  LoadIni;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FPrefixes.Clear;
  FPrefixes.AddStrings(MemoPrefix.Lines);
  SaveIni;
  FPrefixes.Free;
end;

procedure TMainForm.BtnStartClick(Sender: TObject);
begin
  FPrefixes.Clear;
  FPrefixes.AddStrings(MemoPrefix.Lines);
  if (FPrefixes.Count > 0) then
  begin

  end
  else
  begin
    Application.MessageBox(txtNoPrefix, 'Achtung!', 0);
  end;
end;

procedure TMainForm.CheckBox1Change(Sender: TObject);
begin
  DateEdit1.Enabled := CheckBox1.Checked;
  DateEdit2.Enabled := CheckBox1.Checked;
end;

procedure TMainForm.LoadIni;
var
  ini: TIniFile;
  count: Integer; // number of prefixes mentioned in ini-file
  I: Integer;
begin
  ini := TIniFile.Create('config.ini');
  try
    // Paths
    EdtSource.Text := ini.ReadString('Paths', 'Source', '');
    EdtDestination.Text := ini.ReadString('Paths', 'Destination', '');
    // Prefixes
    FPrefixes.Clear;
    count := ini.ReadInteger('Prefixes', 'Count', FPrefixes.Count);
    for I := 0 to count - 1 do
    begin
      FPrefixes.Add(ini.ReadString('Prefixes', IntToStr(I) , ''));
      MemoPrefix.Lines.Add(FPrefixes[I]);
    end;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.SaveIni;
var
  ini: TIniFile;
  I: Integer;
begin
  ini := TIniFile.Create('config.ini');
  try
    // Paths
    ini.WriteString('Paths', 'Source', EdtSource.Text);
    ini.WriteString('Paths', 'Destination', EdtDestination.Text);
    // Prefixes
    ini.EraseSection('Prefixes');
    ini.WriteInteger('Prefixes', 'Count', FPrefixes.Count);
    for I := 0 to FPrefixes.Count - 1 do
    begin
      ini.WriteString('Prefixes', IntToStr(I), FPrefixes[I]);
    end;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.Start(CheckDates: Boolean);
begin

end;

end.

