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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    viewing: boolean;
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
  viewing := false;
  Visible := false;
end;

procedure TfrmPreview.FormHide(Sender: TObject);
begin
  if viewing then
    if assigned(DoClosePreview) then
    begin
      viewing := false;
      DoClosePreview(self);
    end;
end;

procedure TfrmPreview.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
  if viewing then
    if assigned(DoClosePreview) then
    begin
      viewing := false;
      DoClosePreview(self);
    end;
end;

procedure TfrmPreview.FormKeyPress(Sender: TObject; var Key: char);
begin
  if viewing then
    if key = char(VK_ESCAPE) then
      if assigned(DoClosePreview) then
      begin
        viewing := false;
        DoClosePreview(self);
      end;
end;

procedure TfrmPreview.UILoadImg(title: string);
begin
  viewing := true;
  Caption := 'Preview: ' + title;
  Top := 0;
  Left := 100;
  Width := imgPreview.Picture.Bitmap.Width;
  Height := imgPreview.Picture.Bitmap.Height;
  Show;
end;

procedure TfrmPreview.UIUnloadImg;
begin
  viewing := false;
  Hide;
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

