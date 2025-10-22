unit uApplication;

{$mode objfpc}{$H+}

interface

procedure InitApp;
procedure ExitApp;

implementation

uses
  Classes,
  SysUtils,
  uDict,
  uGlobalData;

procedure InitApp;
begin
  Dict := TDict.Create;
  KeyList := TStringList.Create;
end;

procedure ExitApp;
begin
  FreeAndNil(KeyList);
  FreeAndNil(Dict);
end;

end.
