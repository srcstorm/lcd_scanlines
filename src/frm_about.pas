unit frm_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnClose: TBitBtn;
    lblBeta: TLabel;
    memoDesc: TMemo;
    pnlHeader: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  frmAbout.ActiveControl := btnClose;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Visible := false;
end;

end.

