unit uDict;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Generics.Collections,
  uDictEntry;

type
  TStrDEDictionary = specialize TDictionary<string, TDictEntry>;

type
  TDict = class(TObject)
  private
    sData: TStrDEDictionary;
    sFilename: string;
    sDirty: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function AddDictEntry(pDE: TDictEntry): boolean;
    function AddString(pLine: string): boolean;
    function GetDictEntry(pKey: string): TDictEntry;
    function Remove(pKey: string): boolean;
    function FileImport(pFilePath: string): boolean;
    function FileExportNew(pFilePath: string): boolean;
    function FileExportOld: boolean;
    function KeyExists(pKey: string): boolean;
    function Length: integer;
    function ToString: string; override;
    procedure Clear;
    procedure GetKeysList(pKeys: TStringList; pFilter: string);
    procedure GetSortedKeysList(pSortedKeys: TStringList; pFilter: string);
    property IsDirty: boolean read sDirty;
    property Filename: string read sFilename;
  end;

implementation

constructor TDict.Create;
begin
  inherited Create;
  sData := TStrDEDictionary.Create;
  sFilename := '';
  sDirty := False;
end;

destructor TDict.Destroy;
begin
  Clear;
  FreeAndNil(sData);
  inherited Destroy;
end;

function TDict.AddDictEntry(pDE: TDictEntry): boolean;
begin
  if pDE.Key = '' then
    Result := False
  else if sData.ContainsKey(pDE.Key) then
    Result := False
  else
  begin
    sData.Add(pDE.Key, pDE);
    sDirty := True;
    Result := True;
  end;
  if not Result then // pDE freed if not added
    FreeAndNil(pDE);
end;

function TDict.AddString(pLine: string): boolean;
var
  DE: TDictEntry;
begin
  DE := TDictEntry.Create(pLine);
  Result := AddDictEntry(DE);
end;

function TDict.GetDictEntry(pKey: string): TDictEntry;
begin
  Result := nil;
  if sData.ContainsKey(pKey) then
    Result := sData.Items[pKey];
end;

function TDict.Remove(pKey: string): boolean;
begin
  Result := False;
  if sData.ContainsKey(pKey) then
  begin
    sData.Items[pKey].Free;
    sData.Remove(pKey);
    sDirty := True;
    Result := True;
  end;
end;

function TDict.FileImport(pFilePath: string): boolean;
var
  myFile: TextFile;
  line: string;
begin
  try
    AssignFile(myFile, pFilePath);
    Reset(myFile);
    Clear;
    while not EOF(myFile) do
    begin
      ReadLn(myFile, line);
      AddString(line);
    end;
    CloseFile(myFile);
    sFilename := pFilePath;
    sDirty := False;
    Result := True;
  except
    Result := False;
  end;
end;

function TDict.FileExportNew(pFilePath: string): boolean;
var
  myFile: TextFile;
  DE: TDictEntry;
begin
  try
    AssignFile(myFile, pFilePath);
    ReWrite(myFile);
    for DE in sData.Values do
      WriteLn(myfile, DE.ToString);
    CloseFile(myFile);
    sFilename := pFilePath;
    sDirty := False;
    Result := True;
  except
    Result := False;
  end;
end;

function TDict.FileExportOld: boolean;
begin
  Result := FileExportNew(sFilename);
end;

function TDict.KeyExists(pKey: string): boolean;
begin
  Result := sData.ContainsKey(pKey);
end;

function TDict.Length: integer;
begin
  Result := sData.Count;
end;

function TDict.ToString: string;
var
  line: string;
  DE: TDictEntry;
begin
  line := '';
  for DE in sData.Values do
    line := line + DE.ToString + LineEnding;
  Result := line;
end;

procedure TDict.Clear;
var
  DE: TDictEntry;
begin
  for DE in sData.Values do
    DE.Free;
  sData.Clear;
  sFilename := '';
  sDirty := False;
end;

procedure TDict.GetKeysList(pKeys: TStringList; pFilter: string);
var
  Key: string;
  trimmedfilter: string;
begin
  pKeys.Clear;
  pKeys.CaseSensitive := False;
  pKeys.Sorted := False;
  trimmedfilter := Trim(pFilter);
  if trimmedfilter = '' then
  begin
    for Key in sData.Keys do
      pKeys.Add(Key);
  end
  else
  begin
    for Key in sData.Keys do
    begin
      if AnsiContainsText(Key, trimmedfilter) then
      begin
        pKeys.Add(Key);
      end
      else
      begin
        if AnsiContainsText(sData.Items[Key].Data, trimmedfilter) then
          pKeys.Add(Key);
      end;
    end;
  end;
end;

procedure TDict.GetSortedKeysList(pSortedKeys: TStringList; pFilter: string);
begin
  GetKeysList(pSortedKeys, pFilter);
  pSortedKeys.Sort;
end;

end.
