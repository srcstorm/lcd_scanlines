program lcd_scanlines;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frm_main, frm_preview, frm_about
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='LCD Scanlines';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmPreview, frmPreview);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.

