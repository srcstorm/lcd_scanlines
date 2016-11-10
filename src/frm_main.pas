unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls,
  // clipboard
  Clipbrd,
  // various graphics functions & types
  LCLIntf, LCLType, StdCtrls, ComCtrls, Spin, ActnList,
  // project units
  imgeffect, dynscanlines,
  frm_preview, frm_about;

const
  IMGDEBUG = 0;

type

  TLogMessageType = (lmtDebug, lmtHint, lmtInfo, lmtWarning, lmtError);

  { TfrmMain }

  TfrmMain = class(TForm)
    actionAbout: TAction;
    actionPreview: TAction;
    actionClosePreview: TAction;
    actionCopy: TAction;
    actionPaste: TAction;
    actionOpenFile: TAction;
    alistMain: TActionList;
    btnCopy: TBitBtn;
    btnExit: TBitBtn;
    btnHelp: TBitBtn;
    btnClosePreview: TBitBtn;
    btnOpen: TBitBtn;
    btnPaste: TBitBtn;
    btnPreview: TBitBtn;
    cbPreset: TComboBox;
    cbSFactor: TComboBox;
    fseBrightAbove: TFloatSpinEdit;
    fseBrightBelow: TFloatSpinEdit;
    gboxBrightness: TGroupBox;
    gboxOutputImg: TGroupBox;
    grpMessages: TGroupBox;
    imgSrc: TImage;
    lblSFactor: TLabel;
    lblBrightAbove: TLabel;
    lblBrightBelow: TLabel;
    lblBrightnessInfo: TLabel;
    lblCurrentSetting: TLabel;
    lblCurrentSettingA: TLabel;
    lblCurrentSettingB: TLabel;
    lblHeader: TLabel;
    lblPreset: TLabel;
    memoMessages: TMemo;
    openDlgSrcImage: TOpenDialog;
    pagecMain: TPageControl;
    sboxSourceImg: TScrollBox;
    tabsSettings: TTabSheet;
    tabsSourceImg: TTabSheet;
    sbarMain: TStatusBar;
    txtSettingAbove: TStaticText;
    txtSettingBelow: TStaticText;
    procedure actionAboutExecute(Sender: TObject);
    procedure actionClosePreviewExecute(Sender: TObject);
    procedure actionCopyExecute(Sender: TObject);
    procedure actionOpenFileExecute(Sender: TObject);
    procedure actionPasteExecute(Sender: TObject);
    procedure actionPreviewExecute(Sender: TObject);
    procedure cbPresetChange(Sender: TObject);
    procedure cbSFactorChange(Sender: TObject);
    procedure fseBrightAboveEditingDone(Sender: TObject);
    procedure fseBrightBelowEditingDone(Sender: TObject);
    procedure fseEnter(Sender: TObject);
    procedure fseExit(Sender: TObject);
    procedure pagecMainChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    effect: TImgEffect;
    dsDefaults: TDSSettings;
    preset, editing: boolean;
    procedure UIStartCommand;
    procedure UIEndCommand;
    procedure UISourceImageLoaded;
    procedure UIPreviewCreated;
    procedure UIPreviewClosed;
    procedure UIChangeSettings;
    procedure LogMessage(msgType: TLogMessageType; msg: string);
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;


implementation

{$R *.lfm}


{ TfrmMain }

procedure TfrmMain.actionOpenFileExecute(Sender: TObject);
begin
  if openDlgSrcImage.Execute then
  begin

    // If a file is selected...
    if (openDlgSrcImage.Files.Count = 1) and (FileExists(openDlgSrcImage.FileName)) then
    begin

      UIStartCommand;
      LogMessage(lmtInfo, 'Loading source image from file: ' + openDlgSrcImage.FileName);
      imgSrc.Picture.LoadFromFile(openDlgSrcImage.FileName);
      UISourceImageLoaded;
      UIEndCommand;

    end;
  end;

end;

procedure TfrmMain.actionPasteExecute(Sender: TObject);
var
  tempBitmap: TBitmap;
  PictureAvailable: boolean = False;
begin

  // we determine if any image is on clipboard
  if (Clipboard.HasFormat(PredefinedClipboardFormat(pcfDelphiBitmap))) or
     (Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap))) then
    PictureAvailable := True;


  if PictureAvailable then
  begin

    UIStartCommand;
    LogMessage(lmtInfo, 'Getting source image from clipboard.');
    tempBitmap := TBitmap.Create;

    if Clipboard.HasFormat(PredefinedClipboardFormat(pcfDelphiBitmap)) then
      tempBitmap.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfDelphiBitmap));
    if Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) then
      tempBitmap.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));

    imgSrc.Width := tempBitmap.Width;
    imgSrc.Height := tempBitmap.Height;

    // this line does not resize the canvas
    // accordingly...
    //Image2.Canvas.Draw(0, 0, tempBitmap);

    // so we use assign, it works perfectly
    imgSrc.Picture.Bitmap.Assign(tempBitmap);

    tempBitmap.Free;
    UISourceImageLoaded;
    UIEndCommand;

  end
  else
  begin

    ShowMessage('No image is found on clipboard.');

  end;

end;

procedure TfrmMain.actionPreviewExecute(Sender: TObject);
var
  inImg, outImg: TBitmap;
  preview: TImage;
begin
  if editing then
    exit;
  UIStartCommand;
  LogMessage(lmtInfo, 'Applying dynamic scanline filter with ' + cbSFactor.Text + 'x upscaling.');

  inImg := imgSrc.Picture.Bitmap;
  with effect.GetSourceMaxSize do
    if (inImg.Width > x) or (inImg.Height > y) then
    begin
      LogMessage(lmtWarning, 'Maximum allowed input resolution is: ' + inttostr(x) + 'x' + inttostr(y));
      LogMessage(lmtInfo, 'Source image will be cropped.');
    end;

  try
    outImg := effect.ApplyEffect(inImg);
  except
    on e: Exception do
    begin
      LogMessage(lmtError, e.Message);
      UIEndCommand;
      exit;
    end;
  end;

  with effect.GetEffectSize do
    LogMessage(lmtInfo, 'Resolution of output image: ' + inttostr(x) + 'x' + inttostr(y));

  preview := frmPreview.imgPreview;
  preview.Width := outImg.Width;
  preview.Height := outImg.Height;
  preview.Picture.Bitmap.Assign(outImg);

  frmPreview.UILoadImg(cbSFactor.Text + 'x ' + effect.GetEffectName);
  frmPreview.Show;
  frmPreview.DoClosePreview := @actionClosePreviewExecute;
  Show;
  UIPreviewCreated;
  UIEndCommand;
end;

procedure TfrmMain.cbPresetChange(Sender: TObject);
var
  dsEffect: TDynScanlines;
begin
  dsEffect := TDynScanlines(effect);
  preset := true;
  if cbPreset.Text = 'Default' then
  begin
    dsEffect.SetBrightnessFromAboveLine(dsDefaults.brightnessAbove);
    dsEffect.SetBrightnessFromBelowLine(dsDefaults.brightnessBelow);
  end
  else if cbPreset.Text = 'Bright' then
  begin
    dsEffect.SetBrightnessFromAboveLine(63.75);
    dsEffect.SetBrightnessFromBelowLine(63.75);
  end
  else if cbPreset.Text = 'Dark' then
  begin
    dsEffect.SetBrightnessFromAboveLine(43.75);
    dsEffect.SetBrightnessFromBelowLine(43.75);
  end
  else
  begin
    preset := false;
    dsEffect.SetBrightnessFromAboveLine(fseBrightAbove.Value);
    dsEffect.SetBrightnessFromBelowLine(fseBrightBelow.Value);
  end;
  UIChangeSettings;
end;

procedure TfrmMain.cbSFactorChange(Sender: TObject);
begin
  TDynScanlines(effect).SetScalingFactor(strtoint(cbSFactor.Text));
end;

{
procedure TfrmMain.actionCopyExecute(Sender: TObject);
var
  bmpFrom: TImage;
  bmpTo: TBitmap;
begin

  UIStartCommand;
  LogMessage(lmtInfo, 'Copying preview image to clipboard.');
  bmpFrom := frmPreview.imgPreview;
  bmpTo := TBitmap.Create;

  bmpTo.Width := bmpFrom.Width;
  bmpTo.Height := bmpFrom.Height;
  bmpTo.Canvas.Draw(0,0,bmpFrom.Picture.Graphic);

  //Clipboard.Clear;
  Clipboard.Assign(bmpTo);

  bmpTo.Free;
  ShowMessage('Preview image successfully copied to clipboard.');
  UIEndCommand;

end;
}

procedure TfrmMain.actionCopyExecute(Sender: TObject);
begin
  UIStartCommand;
  LogMessage(lmtInfo, 'Copying preview image to clipboard.');
  Clipboard.Assign(effect.GetEffect);
  ShowMessage('Preview image successfully copied to clipboard.');
  UIEndCommand;
end;

procedure TfrmMain.actionClosePreviewExecute(Sender: TObject);
begin
  UIStartCommand;
  LogMessage(lmtInfo, 'Closing preview window.');
  frmPreview.Hide;
  frmPreview.UIUnloadImg;
  UIPreviewClosed;
  UIEndCommand;
end;

procedure TfrmMain.actionAboutExecute(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  effect := TDynScanlines.Create;
  with TDynScanlines(effect) do
  begin
    dsDefaults.scalingFactor := GetScalingFactor;
    dsDefaults.brightnessAbove := GetBrightnessFromAboveLine;
    dsDefaults.brightnessBelow := GetBrightnessFromBelowLine;
  end;
  preset := true;
  editing := false;
  UIChangeSettings;

  if IMGDEBUG = 1 then
  begin
    with effect.GetSourceMinSize do
      LogMessage(lmtDebug, 'Minimum allowed source image resolution: ' + inttostr(x) + 'x' + inttostr(y));
    with effect.GetSourceMaxSize do
      LogMessage(lmtDebug, 'Maximum allowed source image resolution: ' + inttostr(x) + 'x' + inttostr(y));
    with effect.GetWindow do
      LogMessage(lmtDebug, 'Window of output image: ' + inttostr(x) + 'x' + inttostr(y));
  end;

  pagecMain.ActivePage := tabsSourceImg;
  LogMessage(lmtHint, 'In order to create a scanline effect, first you need to load a source image.');
  ActiveControl := btnOpen;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  effect.Free;
end;

procedure TfrmMain.fseBrightAboveEditingDone(Sender: TObject);
begin
  TDynScanlines(effect).SetBrightnessFromAboveLine(fseBrightAbove.Value);
  UIChangeSettings;
end;

procedure TfrmMain.fseBrightBelowEditingDone(Sender: TObject);
begin
  TDynScanlines(effect).SetBrightnessFromBelowLine(fseBrightBelow.Value);
  UIChangeSettings;
end;

procedure TfrmMain.fseEnter(Sender: TObject);
begin
  editing := true;
end;

procedure TfrmMain.fseExit(Sender: TObject);
begin
  editing := false;
end;

procedure TfrmMain.pagecMainChange(Sender: TObject);
begin
  //
end;

procedure TfrmMain.UIStartCommand;
begin
  sbarMain.SimpleText := 'Working...';
  Refresh;
end;

procedure TfrmMain.UIEndCommand;
begin
  sbarMain.SimpleText := 'Ready.';
end;

procedure TfrmMain.UISourceImageLoaded;
var
  w, h: string;
begin
  w := inttostr(imgSrc.Picture.Bitmap.Width);
  h := inttostr(imgSrc.Picture.Bitmap.Height);
  LogMessage(lmtInfo, 'Width: ' + w + ', Height: ' + h);
  gboxOutputImg.Enabled := true;
  actionOpenFile.Enabled := false;
  actionPaste.Enabled := false;
  actionClosePreview.Enabled := false;
  actionCopy.Enabled := false;
  actionPreview.Enabled := true;
end;

procedure TfrmMain.UIPreviewCreated;
begin
  actionPreview.Enabled := false;
  cbSFactor.Enabled := false;
  tabsSettings.Enabled := false;
  actionCopy.Enabled := true;
  actionClosePreview.Enabled := true;
  frmMain.ActiveControl := btnCopy;
end;

procedure TfrmMain.UIPreviewClosed;
begin
  actionClosePreview.Enabled := false;
  actionCopy.Enabled := false;
  tabsSettings.Enabled := true;
  actionPreview.Enabled := true;
  cbSFactor.Enabled := true;
end;

procedure TfrmMain.UIChangeSettings;
begin
  gboxBrightness.Enabled := not preset;
  with TDynScanlines(effect) do
  begin
    txtSettingAbove.Caption := FloatToStr(GetBrightnessFromAboveLine);
    txtSettingBelow.Caption := FloatToStr(GetBrightnessFromBelowLine);
  end;
end;

procedure TfrmMain.LogMessage(msgType: TLogMessageType; msg: string);
var
  mt: string = '';
begin
  case msgType of
    lmtDebug:
      mt := '[Debug] ';
    lmtHint:
      mt := '[Hint] ';
    lmtWarning:
      mt := '[Warning] ';
    lmtError:
      mt := '[Error] ';
  end;

  memoMessages.Append(mt + msg);
end;


end.


