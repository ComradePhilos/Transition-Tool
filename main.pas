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
    EdtSource: TDirectoryEdit;
    EdtDestination: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MemoPrefix: TMemo;
    ProgressBar: TProgressBar;
    procedure BtnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    procedure LoadIni;
    procedure SaveIni;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

const
  ProgrammVersion = 1.1;
  ProgrammTitle = 'Transition Tool';
  ProgrammAuthor = 'Philip Märksch';

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  self.Constraints.MinHeight := self.Height;
  self.Constraints.MinWidth := self.Width;
  LoadIni;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SaveIni;
end;

procedure TMainForm.BtnStartClick(Sender: TObject);
begin

end;

procedure TMainForm.LoadIni;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create('config.ini');
  try
    EdtSource.Text := ini.ReadString('Paths', 'Source', '');
    EdtDestination.Text := ini.ReadString('Paths', 'Destination', '');
  finally
    ini.Free;
  end;
end;

procedure TMainForm.SaveIni;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create('config.ini');
  try
    ini.WriteString('Paths', 'Source', EdtSource.Text);
    ini.WriteString('Paths', 'Destination', EdtDestination.Text);
  finally
    ini.Free;
  end;
end;

end.

