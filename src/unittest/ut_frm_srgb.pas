unit ut_frm_srgb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  srgb;

type

  { TutFrmSRGB }

  TutFrmSRGB = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
    procedure DecodeList;
    procedure EncodeList;
  public
    { public declarations }
  end;

var
  utFrmSRGB: TutFrmSRGB;

implementation

{$R *.lfm}

{ TutFrmSRGB }

procedure TutFrmSRGB.DecodeList;
var
  i, ic: integer;
  ocLinear, ocSRGB: double;
begin
  Memo2.Clear;
  Memo3.Clear;
  for i := 0 to Memo1.Lines.Count - 1 do
  begin
// Validate input
    ic := StrToIntDef(Memo1.Lines.Strings[i], 0);
    if ic < minC then ic := minC;
    if ic > maxC then ic := maxC;
// Decode
    ocLinear := DecodeColor(ic, csLinearRGB);
    ocSRGB := DecodeColor(ic, csSRGB);
// Display
    Memo2.Lines.Add(inttostr(round(ocLinear * maxC)) + ' (' + FloatToStrF(ocLinear, ffFixed, 3, 4) + ')');
    Memo3.Lines.Add(inttostr(round(ocSRGB * maxC)) + ' (' + FloatToStrF(ocSRGB, ffFixed, 3, 4) + ')');
  end;
end;

procedure TutFrmSRGB.EncodeList;
var
  i, ocLinear, ocSRGB: integer;
  ic: double;
begin
  Memo2.Clear;
  Memo3.Clear;
  for i := 0 to Memo1.Lines.Count - 1 do
  begin
// Validate input
    ic := StrToIntDef(Memo1.Lines.Strings[i], 0) / maxC;
    if ic < 0 then ic := 0;
    if ic > 1 then ic := 1;
// Encode
    ocLinear := EncodeColor(ic, csLinearRGB);
    ocSRGB := EncodeColor(ic, csSRGB);
// Display
    with Memo1.Lines do
      Strings[i] := Strings[i] + ' (' + FloatToStrF(ic, ffFixed, 3, 4) + ')';
    Memo2.Lines.Add(inttostr(ocLinear));
    Memo3.Lines.Add(inttostr(ocSRGB));
  end;
end;

procedure TutFrmSRGB.Button1Click(Sender: TObject);
begin
  if ComboBox1.Text = 'Encode' then
    EncodeList
  else
    DecodeList;
end;

procedure TutFrmSRGB.Button2Click(Sender: TObject);
begin
  Memo1.Clear;
  Memo2.Clear;
  Memo3.Clear;

  Memo1.Lines.Add('0');
  Memo1.Lines.Add('32');
  Memo1.Lines.Add('64');
  Memo1.Lines.Add('96');
  Memo1.Lines.Add('128');
  Memo1.Lines.Add('160');
  Memo1.Lines.Add('187');
  Memo1.Lines.Add('192');
  Memo1.Lines.Add('224');
  Memo1.Lines.Add('255');
end;


end.

