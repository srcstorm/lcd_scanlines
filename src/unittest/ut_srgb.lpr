program ut_srgb;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ut_frm_srgb
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Unit Test: Color Space';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TutFrmSRGB, utFrmSRGB);
  Application.Run;
end.

