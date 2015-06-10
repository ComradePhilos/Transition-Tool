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
    CheckBoxDate: TCheckBox;
    DateEdit1: TDateEdit;
    DateEdit2: TDateEdit;
    EdtSource: TDirectoryEdit;
    EdtDestination: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblProgress: TLabel;
    MemoPrefix: TMemo;
    ProgressBar: TProgressBar;
    procedure BtnStartClick(Sender: TObject);
    procedure CheckBoxDateChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FPrefixes: TStringList;
    Mainfolder: string;
    function CountFiles: integer;
    function FileDateValid(ADate: TDateTime): boolean;
    function HasPrefix(FileName: string): String;
    procedure LoadIni;
    procedure SaveIni;
    procedure Start;
    procedure UpdateProgressBar(Progress, Max: integer);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

const
  ProgrammVersion = 1.0;
  ProgrammTitle = 'Transition Tool';
  ProgrammAuthor = 'Philip Märksch';

  txtNoPrefix = 'Kann nicht starten: Keine Präfixe angegeben!';
  txtNoFiles = 'Keine Dateien im Quellverzeichnis gefunden!';
  txtFinished = 'Vorgang abegeschlossen! %d Dateien wurden verschoben';

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
    Start;
  end
  else
  begin
    Application.MessageBox(txtNoPrefix, 'Achtung!', 0);
  end;
end;

procedure TMainForm.CheckBoxDateChange(Sender: TObject);
begin
  DateEdit1.Enabled := CheckBoxDate.Checked;
  DateEdit2.Enabled := CheckBoxDate.Checked;
end;

function TMainForm.CountFiles: integer;
var
  I: integer;
  SearchRec: TSearchRec;
begin
  I := 0;
  Result := 0;
  FindFirst(EdtSource.Directory + '\*.*', faAnyFile, SearchRec);
  repeat
    Inc(I);
  until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
  Result := I - 2;
end;

function TMainForm.FileDateValid(ADate: TDateTime): boolean;
begin
  if CheckBoxDate.Checked then
  begin
    Result := (ADate >= StrToDate(DateEdit1.Text)) and (ADate <= StrToDate(DateEdit2.Text));
  end
  else
  begin
    Result := True;
  end;
end;

procedure TMainForm.LoadIni;
var
  ini: TIniFile;
  Count: integer; // number of prefixes mentioned in ini-file
  I: integer;
begin
  ini := TIniFile.Create('config.ini');
  try
    // Paths
    EdtSource.Text := ini.ReadString('Paths', 'Source', '');
    EdtDestination.Text := ini.ReadString('Paths', 'Destination', '');
    // Prefixes
    FPrefixes.Clear;
    Count := ini.ReadInteger('Prefixes', 'Count', FPrefixes.Count);
    for I := 0 to Count - 1 do
    begin
      FPrefixes.Add(ini.ReadString('Prefixes', IntToStr(I), ''));
      MemoPrefix.Lines.Add(FPrefixes[I]);
    end;
  finally
    ini.Free;
  end;
end;

function TMainForm.HasPrefix(FileName: string): String;
var
  I: integer;
begin
  Result := '';
  for I := 0 to FPrefixes.Count - 1 do
  begin
    if SameText(Copy(FileName, 0, Length(FPrefixes[I])), FPrefixes[I]) then
    begin
      Result := FPrefixes[I];
      CreateDir(EdtDestination.Text + Mainfolder + FPrefixes[I]);
    end;
  end;
end;

procedure TMainForm.SaveIni;
var
  ini: TIniFile;
  I: integer;
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

procedure TMainForm.Start;
var
  SearchRec: TSearchRec;
  OtherFolder: string;
  FileCount: integer;
  FilesProcessed: integer;
  FilesMoved: integer;
  pref: String;
begin
  FilesProcessed := 0;
  FilesMoved := 0;
  if CheckBoxDate.Checked then
    MainFolder := '\Belege (' + DateEdit1.Text + ' bis ' + DateEdit2.Text + ')\'
  else
    MainFolder := '\Belege\';

  if CheckBoxDate.Checked then
    OtherFolder := '\Andere (' + DateEdit1.Text + ' bis ' + DateEdit2.Text + ')\'
  else
    OtherFolder := '\Andere\';

  // Count Files
  FileCount := CountFiles;
  UpdateProgressBar(0, FileCount);

  // Move Files
  if (FileCount > 0) then
  begin
    CreateDir(EdtDestination.Text + OtherFolder);
    CreateDir(EdtDestination.Text + MainFolder);
    FindFirst(edtSource.Text + '\*.*', faAnyFile, SearchRec);
    FindNext(SearchRec);
    FindNext(SearchRec);

    repeat
      // If Date is not valid, then ignore file completely
      if FileDateValid(FileDateToDateTime(SearchRec.Time)) then
      begin
        pref := HasPrefix(SearchRec.Name);
        if (pref <> '') then
        begin
          RenameFile(EdtSource.Text + '\' + SearchRec.Name,
            EdtDestination.Text + MainFolder + pref +'\' + SearchRec.Name);
        end
        else
        begin
          RenameFile(EdtSource.Text + '\' + SearchRec.Name,
            EdtDestination.Text + OtherFolder + SearchRec.Name);
        end;
        Inc(FilesMoved);
      end;
      Inc(FilesProcessed);
      UpdateProgressBar(FilesProcessed, FileCount);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
    Application.MessageBox(PChar(Format(txtFinished, [FilesMoved])), 'Fertig', 0);
  end
  else
  begin
    Application.MessageBox(txtNoFiles, 'Hinweis!', 0);
  end;
end;

procedure TMainForm.UpdateProgressBar(Progress, Max: integer);
begin
  ProgressBar.Position := Progress;
  ProgressBar.Max := Max;
  lblProgress.Caption := IntToStr(Progress) + '/' + IntToStr(Max);
  Application.ProcessMessages;
end;

end.
