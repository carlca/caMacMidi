program camacmidi;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  camacmidiform, caMidi,caMidiIntf,caMidiTypes,Interfaces, Forms, lazcontrols;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TcaMainForm, MainForm);
  Application.Run;
end.

