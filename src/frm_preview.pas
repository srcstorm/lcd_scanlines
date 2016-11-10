unit frm_preview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLType;

type

  { TfrmPreview }

  TfrmPreview = class(TForm)
    imgPreview: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    { public declarations }
    DoClosePreview: TNotifyEvent;
    procedure UILoadImg(title: string);
    procedure UIUnloadImg;
  end;

var
  frmPreview: TfrmPreview;

implementation

{$R *.lfm}

{ TfrmPreview }

procedure TfrmPreview.FormCreate(Sender: TObject);
begin
  DoClosePreview := nil;
  Visible := false;
end;

procedure TfrmPreview.FormKeyPress(Sender: TObject; var Key: char);
begin
  if key = char(VK_ESCAPE) then
    if assigned(DoClosePreview) then
      DoClosePreview(self);
end;

procedure TfrmPreview.UILoadImg(title: string);
begin
  Caption := 'Preview: ' + title;
  Top := 0;
  Left := 100;
  Width := imgPreview.Picture.Bitmap.Width;
  Height := imgPreview.Picture.Bitmap.Height;
//  Refresh;
end;

procedure TfrmPreview.UIUnloadImg;
begin
  with imgPreview do
  begin
    Picture.Clear;
    Width := 128;
    Height := 128;
  end;

  with frmPreview do
  begin
    Width := 150;
    Height := 150;
  end;
end;

end.

