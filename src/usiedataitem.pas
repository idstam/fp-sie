unit USieDataItem;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, USieClasses;

type
  TSieDataItem = class
  private
    class function FirstWhiteSpace(aStr: string): integer;
    class function SplitLine(aLine: string): TStringList;

  public
    Document: TSieDocument;
    ItemType: string;
    Data: TStringList;
    RawData: string;
    constructor Create(aLine: string);
    constructor Create(aLine: string; aDocument: TSieDocument);
    function GetInt(aIndex: integer): integer;
    function GetIntNull(aIndex: integer): TNullableCurrency;
    function GetLong(aIndex: integer): longint;
    function GetString(aIndex: integer): string;
    function GetDate(aIndex: integer): string;
    function GetDecimal(aIndex: integer): double;
    function GetObjects(): TListSieObject;
  end;

implementation

constructor TSieDataItem.Create(aLine: string);
var
  line: string;
  p: integer;
begin
  RawData := aLine;
  line := aLine.Trim();
  p := FirstWhiteSpace(line);

  if (p = -1) then
  begin
    ItemType := line;
    Data := TStringList.Create();
  end
  else
  begin
    ItemType := line.Substring(0, p);
    Data := SplitLine(aLine.Substring(p + 1, aLine.Length - (p + 1)));

  end;
end;

constructor TSieDataItem.Create(aLine: string; aDocument: TSieDocument);
begin
  self.Document := aDocument;
  self.Create(aLine);
end;

function TSieDataItem.GetInt(aIndex: integer): integer;
begin
  if Data.Count <= aIndex then exit(0);
  exit(StrToIntDef(Data[aIndex], 0));
end;
function TSieDataItem.GetIntNull(aIndex: integer): TNullableCurrency;
var
  ret:TNullableCurrency;
begin

  if Data.Count <= aIndex then exit(Default(TNullableCurrency));
  exit(StrToIntDef(Data[aIndex], 0));
end;


function TSieDataItem.GetLong(aIndex: integer): longint;
begin
  if Data.Count <= aIndex then exit(0);
  exit(StrToIntDef(Data[aIndex], 0));
end;


function TSieDataItem.GetString(aIndex: integer): string;
begin
  if Data.Count <= aIndex then exit('');
  exit(Data[aIndex]);
end;

function TSieDataItem.GetDate(aIndex: integer): string;
begin
  if Data.Count <= aIndex then exit('');
  exit(Data[aIndex]);
end;

function TSieDataItem.GetDecimal(aIndex: integer): double;
var
  tmpStr:string;
  tmpCur:Currency;
begin
  if Data.Count <= aIndex then exit(0);

  tmpStr := StringReplace(Data[aIndex], '.', DefaultFormatSettings.DecimalSeparator, []);
  tmpCur := StrToFloatDef(tmpStr, 0);
  exit(tmpCur);

end;

function TSieDataItem.GetObjects(): TListSieObject;
var
  dimNumber: string;
  objectNumber: string;
  ret: TListSieObject;
  idata: string;
  item: string;
  dimData: TStringList;
  dim: TSieDimension;
  i: integer;
  tmpObj: TSieObject;
begin
  idata := '--missing-object--';
  if RawData.Contains('{}') then exit(nil);
  for item in Data do
  begin
    if item.StartsWith('{') then
    begin
      idata := item.Trim.Replace('{', '').Replace('}', '');
      break;
    end;
  end;

  if idata = '--missing-object--' then
  begin
    Document.ValidationErrors.Add(TSieError.Create(SieMissingObjectError, RawData));
    exit(nil);
  end;
  dimData := SplitLine(idata);

  ret := TListSieObject.Create();
  for i := 0 to dimData.Count - 1 do
  begin
    if (i mod 2) = 1 then continue;
    dimNumber := dimData[i];
    Document.DIM.TryAdd(dimNumber, TSieDimension.Create(dimNumber, '[TEMP]', False));
    dim := Document.DIM[dimNumber];
    objectNumber := dimData[i + 1];
    tmpObj := TSieObject.Create();
    tmpObj.Number := objectNumber;
    tmpObj.Dimension := dim;
    tmpObj.Name := '[TEMP]';
    dim.Objects.TryAdd(objectNumber, TSieObject.Create());
    ret.Add(tmpObj);
  end;

  exit(ret);
end;

class function TSieDataItem.FirstWhiteSpace(aStr: string): integer;
var
  a: integer;
  b: integer;
begin
  a := aStr.IndexOf(' ');
  b := aStr.IndexOf(chr(9));
  if (a = -1) and (b = -1) then exit(-1);
  if (a = -1) and (b <> -1) then exit(b);
  if (b = -1) then exit(a);

  if (a <= b) then exit(a);

  exit(b);
end;

class function TSieDataItem.SplitLine(aLine: string): TStringList;
var
  ret: TStringList;
  line: string;
  isInField: integer;
  isInObject: boolean;
  buffer: string;
  trimBuf: string;
  skipNext: boolean;
  c: char;
begin
  ret := TStringList.Create();
  line := aLine.Trim();
  isInObject := False;
  isInField := 0;
  buffer := '';
  skipNext:= False;

  for C in line do
  begin
    if skipNext and (C = '"') then
    begin
      skipNext := False;
      continue;
    end;

    if C = '\' then
    begin
      skipNext := True;
      continue;
    end;

    if (C = '"') and (not isInObject) then
    begin
      isInField := isInField + 1;
      continue;
    end;

    if C = '{' then isInObject := True;
    if C = '}' then isInObject := False;

    if ((C = ' ') or (C = Chr(9))) and (isInField <> 1) and (not isInObject) then
    begin
      trimBuf := buffer.Trim();
      if (not trimBuf.IsEmpty()) or (isInField = 2) then
      begin
        ret.Add(trimBuf);
        buffer := '';
      end;
      isInField := 0;
    end;
    buffer := buffer + C;
  end;
  if (not buffer.IsEmpty()) then
  begin
    ret.Add(buffer.Trim());
  end;
  exit(ret);
end;

end.
