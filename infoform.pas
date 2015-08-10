unit infoform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    { private declarations }
  public
    { public declarations }
    procedure SetInfo(Version, Author: String);
  end;

var
  Form1: TForm1;

implementation

procedure TForm1.SetInfo(Version, Author: String);
begin
  Label2.Caption := 'Version: ' + Version;
  Label3.Caption := 'Author: ' + Author;
end;

{$R *.lfm}

end.

