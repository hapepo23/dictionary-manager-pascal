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
    constructor Create(const pLine: string); // TAB-delimited
    function ToString: string; override; // TAB-delimited
    property Key: string read sKey write sKey;
    property Data: string read sData write sData;
  end;

implementation

constructor TDictEntry.Create(const pLine: string);
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

function TDictEntry.ToString: string;
var
  Parts: TStringDynArray;
begin
  Parts := sData.Split([LineEnding]);
  Result := sKey + #9 + string.Join(#9, Parts);
end;

end.
