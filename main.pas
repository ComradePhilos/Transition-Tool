unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Buttons, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    DirectoryEdit1: TDirectoryEdit;
    DirectoryEdit2: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

const
  ProgrammVersion = 1.1;
  ProgrammTitle = 'Transition Tool';
  ProgrammAuthor = 'Philip Märksch';

implementation

{$R *.lfm}

end.

