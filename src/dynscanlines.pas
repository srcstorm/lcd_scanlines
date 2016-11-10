unit dynscanlines;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  imgeffect;

const
  TGTWINDOWX_DS = 8192;
  TGTWINDOWY_DS = 8192;

type

  TDSSettings = record
    scalingFactor: integer;
    brightnessAbove, brightnessBelow: double;
  end;

  { TDynScanlines }

  TDynScanlines = class(TImgEffect)
  private
    settings: TDSSettings;
    procedure DSImageEffect;
  public
    { public declarations }
    function GetScalingFactor: integer;
    procedure SetScalingFactor(sf: integer);
    function GetBrightnessFromAboveLine: double;
    procedure SetBrightnessFromAboveLine(ba: double);
    function GetBrightnessFromBelowLine: double;
    procedure SetBrightnessFromBelowLine(bb: double);
    constructor Create(windowWidth: integer = TGTWINDOWX_DS; windowHeight: integer = TGTWINDOWY_DS); override;
  end;


implementation


const
  SRCMINX_DS = 1;
  SRCMINY_DS = 2;

type
  TRGBColor = record
    r, g, b: byte;
  end;

procedure Blend(var px1: TRGBColor; const px2: TRGBColor; const br1, br2: double);
var
  rt, gt, bt: double;
begin
  rt := (px2.r / 255) * br2;
  gt := (px2.g / 255) * br2;
  bt := (px2.b / 255) * br2;

  rt := ((px1.r / 255) * br1) + rt;
  gt := ((px1.g / 255) * br1) + gt;
  bt := ((px1.b / 255) * br1) + bt;

  if (rt > 255) then
    rt := 255
  else if (rt < 0) then
    rt := 0;
  if (gt > 255) then
    gt := 255
  else if (gt < 0) then
    gt := 0;
  if (bt > 255) then
    bt := 255
  else if (bt < 0) then
    bt := 0;

  px1.r := byte(trunc(rt));
  px1.g := byte(trunc(gt));
  px1.b := byte(trunc(bt));
end;

{ TDynScanlines }

// Dynamic Scanline Image Effect
procedure TDynScanlines.DSImageEffect;
var
  h, v, // Coordinates of pixel in source image
  ht, vt, // Coordinates of scaled area in target image
  sf: integer;
  sCoor, tCoor: TPoint;
  px: TColor; // Active pixel
  pxs, // Black line pixel
  pxb: TRGBColor; // Pixel below
begin
  sf := settings.scalingFactor;
  SetTargetSize(sSize.x * sf, (sSize.y * sf) - (sf div 2));

  for v := 0 to sSize.y - 2 do
  begin
    sCoor.y := v;
    tCoor.y := v * sf;
    if (tCoor.y + sf > tSize.y) then
      break;
    for h := 0 to sSize.x - 1 do
    begin
      sCoor.x := h;
      tCoor.x := h * sf;
      if (tCoor.x + sf > tSize.x) then
        break;
      px := sImg.Canvas.Pixels[sCoor.x, sCoor.y];
      RedGreenBlue(px, pxs.r, pxs.g, pxs.b);
      RedGreenBlue(sImg.Canvas.Pixels[sCoor.x, sCoor.y + 1], pxb.r, pxb.g, pxb.b);
      Blend(pxs, pxb, settings.brightnessAbove, settings.brightnessBelow);

// Generate active line
      for vt := tCoor.y to tCoor.y + (sf div 2) - 1 do
        for ht := tCoor.x to tCoor.x + sf - 1 do
          tImg.Canvas.Pixels[ht, vt] := px;

// Generate black line
      for vt := tCoor.y + (sf div 2) to tCoor.y + sf - 1 do
        for ht := tCoor.x to tCoor.x + sf - 1 do
          tImg.Canvas.Pixels[ht, vt] := RGBToColor(pxs.r, pxs.g, pxs.b);
    end;
  end;

// Generate last line
  sCoor.y := sSize.y - 1;
  tCoor.y := (sSize.y - 1) * sf;
  if (tCoor.y + (sf div 2) > tSize.y) then
    exit;
  for h := 0 to sSize.x - 1 do
  begin
    sCoor.x := h;
    tCoor.x := h * sf;
    if (tCoor.x + sf > tSize.x) then
      break;
    px := sImg.Canvas.Pixels[sCoor.x, sCoor.y];

// Generate active line
    for vt := tCoor.y to tCoor.y + (sf div 2) - 1 do
      for ht := tCoor.x to tCoor.x + sf - 1 do
        tImg.Canvas.Pixels[ht, vt] := px;
  end;
end;

function TDynScanlines.GetScalingFactor: integer;
begin
  result := settings.scalingFactor;
end;

procedure TDynScanlines.SetScalingFactor(sf: integer);
begin
  if (sf < 2) then
    exit;
  if ((sf mod 2) = 0) then
    settings.scalingFactor := sf;
end;

function TDynScanlines.GetBrightnessFromAboveLine: double;
begin
  result := settings.brightnessAbove;
end;

procedure TDynScanlines.SetBrightnessFromAboveLine(ba: double);
begin
  if ((ba >= 0.0) and (ba <= 255.0)) then
    settings.brightnessAbove := ba;
end;

function TDynScanlines.GetBrightnessFromBelowLine: double;
begin
  result := settings.brightnessBelow;
end;

procedure TDynScanlines.SetBrightnessFromBelowLine(bb: double);
begin
  if ((bb >= 0.0) and (bb <= 255.0)) then
    settings.brightnessBelow := bb;
end;

constructor TDynScanlines.Create(windowWidth: integer = TGTWINDOWX_DS; windowHeight: integer = TGTWINDOWY_DS);
begin
  inherited Create(windowWidth, windowHeight);
  effectName := 'LCD Scanlines image effect';
  doImageEffect := @DSImageEffect;
  SetXY(sMinSize, SRCMINX_DS, SRCMINY_DS);
  with settings do
  begin
    scalingFactor := 2;
    brightnessAbove := 53.75;
    brightnessBelow := 53.75;
  end;
end;

end.

