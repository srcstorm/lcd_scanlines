unit srgb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Math;

const
  minC = 0;
  maxC = 255;

type

  TColorSpace = (csLinearRGB, csSRGB);

function SRGBToLinear(const sRGBColor: double): double;
function LinearToSRGB(const linearColor: double): double;
function DecodeColor(const deviceColor: integer; colorSpace: TColorSpace = csLinearRGB): double; inline;
function EncodeColor(const linearColor: double; colorSpace: TColorSpace = csLinearRGB): integer; inline;


implementation


var
  SRGBToLinearArr: array[minC..maxC] of double;
  LinearToSRGBArr: array[minC..maxC] of double;


procedure BuildDecodeLookup;
var
  i: integer;
begin
  for i := minC to maxC do
    SRGBToLinearArr[i] := SRGBToLinear(i / maxC);
end;

procedure BuildEncodeLookup;
var
  i: integer;
begin
  for i := minC to maxC do
    LinearToSRGBArr[i] := LinearToSRGB(i / maxC);
end;

function SRGBToLinear(const sRGBColor: double): double;
const
  a = 0.055;
begin
  if (sRGBColor <= 0.04045) then
    result := sRGBColor / 12.92
  else
    result := ((sRGBColor + a) / (1 + a)) ** 2.4;
end;

function LinearToSRGB(const linearColor: double): double;
const
  a = 0.055;
begin
  if (linearColor <= 0.0031308) then
    result := 12.92 * linearColor
  else
    result := ((1 + a) * (linearColor ** (1 / 2.4))) - a;
end;

function DecodeColor(const deviceColor: integer; colorSpace: TColorSpace): double;
begin
  if (colorSpace = csLinearRGB) then
    result := deviceColor / maxC
  else
    result := SRGBToLinearArr[deviceColor];
end;

function EncodeColor(const linearColor: double; colorSpace: TColorSpace): integer;
begin
  if (colorSpace = csLinearRGB) then
    result := round(linearColor * maxC)
  else
    result := round(LinearToSRGBArr[trunc(linearColor * maxC)] * maxC);
end;


initialization

  BuildDecodeLookup;
  BuildEncodeLookup;

end.

