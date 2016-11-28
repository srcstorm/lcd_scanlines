unit imgeffect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics;

const
  TGTWINDOWX = 0;
  TGTWINDOWY = 0;

type

  TImgEvent = procedure of object;

  { TImgEffect }

  TImgEffect = class
  protected
    sImg, tImg: TBitmap;
    sSize, sMinSize, sMaxSize,
    tSize, tWindow: TPoint;
    effectName: string;
    doImageEffect: TImgEvent;
    procedure SetSourceSize(sw, sh: integer);
    procedure SetTargetSize(tw, th: integer);
  public
    { public declarations }
    function ApplyEffect(sourceImage: TBitmap): TBitmap;
    function GetEffect: TBitmap;
    function GetSourceMinSize: TPoint;
    function GetSourceMaxSize: TPoint;
    function GetWindow: TPoint;
    function GetEffectSize: TPoint;
    function GetEffectName: string;
    constructor Create(windowWidth: integer = TGTWINDOWX; windowHeight: integer = TGTWINDOWY); virtual;
    destructor Destroy; override;
  end;

procedure SetXY(var v: TPoint; const x, y: integer);


implementation


const
  SRCMINX = 1;
  SRCMINY = 1;
  SRCMAXX = 2048;
  SRCMAXY = 2048;

procedure SetXY(var v: TPoint; const x, y: integer);
begin
  v.x := x;
  v.y := y;
end;


{ TImgEffect }

procedure TImgEffect.SetSourceSize(sw, sh: integer);
begin
  if ((sw < sMinSize.x) or (sh < sMinSize.y)) then
    raise Exception.Create('Resolution of source image is too small.');
  if (sw > sMaxSize.x) then
    sw := sMaxSize.x;
  if (sh > sMaxSize.y) then
    sh := sMaxSize.y;
  SetXY(sSize, sw, sh);
end;

procedure TImgEffect.SetTargetSize(tw, th: integer);
begin
  if (tWindow.x > 0) then
    if (tw > tWindow.x) then
      tw := tWindow.x;
  if (tWindow.y > 0) then
    if (th > tWindow.y) then
      th := tWindow.y;
  SetXY(tSize, tw, th);
  tImg.SetSize(tw, th);
end;

function TImgEffect.ApplyEffect(sourceImage: TBitmap): TBitmap;
var
  h, v: integer;
begin
  sImg := sourceImage;
  tImg.Clear;
  result := tImg;
  SetSourceSize(sImg.Width, sImg.Height);
  if (assigned(doImageEffect)) then
  begin
    doImageEffect;
    exit;
  end;

// Default behavior:
// Make an exact copy with necessary cropping
  SetTargetSize(sSize.x, sSize.y);

  for v := 0 to sSize.y - 1 do
  begin
    if (v > tSize.y - 1) then
      break;
    for h := 0 to sSize.x - 1 do
    begin
      if (h > tSize.x - 1) then
        break;
      tImg.Canvas.Pixels[h, v] := sImg.Canvas.Pixels[h, v];
    end;
  end;
end;

function TImgEffect.GetEffect: TBitmap;
begin
  result := tImg;
end;

function TImgEffect.GetSourceMinSize: TPoint;
begin
  result := sMinSize;
end;

function TImgEffect.GetSourceMaxSize: TPoint;
begin
  result := sMaxSize;
end;

function TImgEffect.GetWindow: TPoint;
begin
  result := tWindow;
end;

function TImgEffect.GetEffectSize: TPoint;
begin
  result := tSize;
end;

function TImgEffect.GetEffectName: string;
begin
  result := effectName;
end;

constructor TImgEffect.Create(windowWidth: integer = TGTWINDOWX; windowHeight: integer = TGTWINDOWY);
begin
  inherited Create;
  effectName := 'Image effect';
  doImageEffect := nil;
  sImg := nil;
  tImg := TBitmap.Create;
  SetXY(sSize, 0, 0);
  SetXY(tSize, 0, 0);
  SetXY(sMinSize, SRCMINX, SRCMINY);
  SetXY(sMaxSize, SRCMAXX, SRCMAXY);
  SetXY(tWindow, windowWidth, windowHeight);
end;

destructor TImgEffect.Destroy;
begin
  tImg.Free;
  inherited Destroy;
end;

end.

