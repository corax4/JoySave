program JoySave;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, joysave_main
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
    Application.Scaled := True;
  Application.Initialize;
    Application.CreateForm(TJoySaveMainForm, JoySaveMainForm);
  Application.Run;
end.

