unit uDictEntry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Types;

type
  TDictEntry = class(TObject)
  private
    sKey: string;
    sData: string; // NL-delimited
  public
    constructor Create(pLine: string); // key TAB data1 TAB data2 ...
    function IsKeyValid(keys: TStringList): boolean;
    procedure GetDataInto(sl: TStringList);
    function ToString: string; override; // key TAB data1 TAB data2 ...
    function Display: string; // for Debugging
    property Key: string read sKey write sKey;
    property Data: string read sData write sData;
  end;

implementation

constructor TDictEntry.Create(pLine: string);
var
  Parts: TStringDynArray;
begin
  inherited Create;
  Parts := pLine.Split(#9);
  if Length(Parts) > 1 then
  begin
    Key := Trim(Parts[0]);
    Data := string.Join(LineEnding, Parts, 1, High(Parts));
  end
  else
  begin
    Key := Trim(Parts[0]);
    Data := '';
  end;
end;

function TDictEntry.IsKeyValid(keys: TStringList): boolean;
begin
  if sKey = '' then
    Result := False
  else if keys.IndexOf(sKey) <> -1 then
    Result := False
  else
    Result := True;
end;

procedure TDictEntry.GetDataInto(sl: TStringList);
var
  Parts: TStringDynArray;
  i: integer;
begin
  Parts := sData.Split([LineEnding]);
  sl.Clear;
  for i := Low(Parts) to High(Parts) do
    sl.Add(Parts[i]);
end;

function TDictEntry.ToString: string;
var
  Parts: TStringDynArray;
begin
  Parts := sData.Split([LineEnding]);
  Result := sKey + #9 + string.Join(#9, Parts);
end;

function TDictEntry.Display: string;
var
  Parts: TStringDynArray;
begin
  Parts := sData.Split([LineEnding]);
  Result := 'key="' + sKey + '", data="' + string.Join('|', Parts) + '"';
end;

end.
