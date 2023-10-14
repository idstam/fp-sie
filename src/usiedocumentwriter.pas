unit USieDocumentWriter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, USieClasses, LConvEncoding;

type TSieDocumentWriter = class
  private
    F:TextFile;
    procedure writeLine(line: string);
    procedure writeRAR(aDoc:TSieDocument);
    function makeField(data:string):string;
    function makeSieDate(data:string):string;
  public
    procedure Write(aDoc:TSieDocument; aFileName:string);

end;
implementation

procedure TSieDocumentWriter.Write(aDoc:TSieDocument; aFileName:string);
var
  s:string;
  tmp:string;
begin
  AssignFile(F, aFileName);
  Rewrite(F);

  writeLine('#FLAGGA ' + IntToStr(aDoc.FLAGGA));
  //#KSUMMA
  tmp := '';
  for s in aDoc.PROGRAMS do
  begin
    tmp+= makeField(s) + ' ';
  end;
  writeLine('#PROGRAM "fp-sie" ' + tmp);
  writeLine('#FORMAT PC8');
  writeLine('#SIETYPE ' + IntToStr(aDoc.SIETYP));
  if aDoc.PROSA <> '' then writeLine('#PROSA "' + aDoc.PROSA + '"');
  if aDoc.FNAMN.Code <> '' then writeLine('#FNR "' + aDoc.FNAMN.Code + '"');
  writeLine('#ORGNR ' + aDoc.FNAMN.OrgIdentifier + '"');
  writeLine('#FNAMN ' + aDoc.FNAMN.Name + '"');
  tmp := aDoc.FNAMN.Contact + aDoc.FNAMN.Street + aDoc.FNAMN.ZipCity + aDoc.FNAMN.Phone;
  if tmp <> '' then
  begin
    writeLine('#ADRESS "' + aDoc.FNAMN.Contact + '" "' + aDoc.FNAMN.Street + '" "' + aDoc.FNAMN.ZipCity + '" "' + aDoc.FNAMN.Phone + '"');
  end;
  if aDoc.FNAMN.OrgType <> '' then writeLine('#FTYP ' + aDoc.FNAMN.OrgType);
  if aDoc.KPTYP <> '' then writeLine('#KPTYP ' + aDoc.KPTYP);
  if aDoc.VALUTA <> '' then writeLine('#VALUTA ' + aDoc.VALUTA);
  if aDoc.VALUTA <> '' then writeLine('#VALUTA ' + aDoc.VALUTA);
  if aDoc.TAXAR > 0 then writeLine('#TAXAR ' + IntToStr(aDoc.TAXAR));
  writeLine('#OMFATTN ' + makeSieDate(aDoc.OMFATTN));
  writeRAR(aDoc);

  CloseFile(F);
end;
procedure TSieDocumentWriter.writeRAR(aDoc:TSieDocument);
var
  by:TSieBookingYear;
begin
  for by in aDoc.RAR.Values do
  begin
    writeLine('#RAR ' + IntToStr(by.ID) + ' ' + makeSieDate(by.StartDate) + ' ' + makeSieDate(by.EndDate));
  end;
end;

function TSieDocumentWriter.makeField(data:string):string;
var
  i:integer;
begin
  if TryStrToInt(data, i) then
  begin
    exit(data);
  end
  else
  begin
    exit('"' + data + '"');
  end;
end;

function TSieDocumentWriter.makeSieDate(data:string):string;
begin
  if data <> '' then exit(data) else exit('00000000');

end;

procedure TSieDocumentWriter.writeLine(line: string);
var
  codepaged:string;
begin
  codepaged:= UTF8ToCP437(line);
  // CP437ToUTF8(codePagedLine);
  WriteLn(F, codepaged);
end;

end.

