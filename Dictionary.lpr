program Dictionary;

{$mode objfpc}{$H+}

uses
  Forms,
  Interfaces,
  uMainForm,
  uViewEditForm;

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TViewEditForm, ViewEditForm);
  Application.Run;
end.
