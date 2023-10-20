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
    procedure writeDIM(aDoc:TSieDocument);
    procedure writeKonto(aDoc:TSieDocument);
    procedure writePeriodValue(aDoc:TSieDocument; aTag:string; aValues:TListSiePeriodValue);
    procedure writePeriodSaldo(aDoc:TSieDocument; aTag:string; aValues:TListSiePeriodValue);
    procedure writeVER(aDoc:TSieDocument);
    function getObjektLista(aDoc:TSieDocument; aValues:TListSieObject):string;
    function makeField(data:string):string;
    function makeAmount(aAmount:double):string;
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
  writeLine('#PROGRAM ' + tmp);
  writeLine('#FORMAT PC8');
  writeLine('#GEN ' + aDoc.GEN_DATE + ' ' + aDoc.GEN_NAMN);
  writeLine('#SIETYP ' + IntToStr(aDoc.SIETYP));
  if aDoc.PROSA <> '' then writeLine('#PROSA "' + aDoc.PROSA + '"');
  if aDoc.FNAMN.Code <> '' then writeLine('#FNR ' + aDoc.FNAMN.Code + '');
  writeLine('#ORGNR "' + aDoc.FNAMN.OrgIdentifier + '"');
  writeLine('#FNAMN "' + aDoc.FNAMN.Name + '"');
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
  writeDIM(aDoc);
  writeKonto(aDoc);
  WritePeriodValue(aDoc, '#IB', aDoc.IB);
  WritePeriodValue(aDoc, '#UB', aDoc.UB);
  if aDoc.SIETYP >= 3 then
  begin
      WritePeriodValue(aDoc, '#OIB', aDoc.OIB);
      WritePeriodValue(aDoc, '#OUB', aDoc.OUB);
  end;

  if aDoc.SIETYP > 1 then
  begin
      WritePeriodSaldo(aDoc, '#PBUDGET', aDoc.PBUDGET);
      WritePeriodSaldo(aDoc, '#PSALDO', aDoc.PSALDO);
  end;

  WritePeriodValue(aDoc, '#RES', aDoc.RES);
  writeVER(aDoc);
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

procedure TSieDocumentWriter.writeDIM(aDoc:TSieDocument);
var
  d:TSieDimension;
  o:TSieObject;
begin
  if aDoc.DIM.Count = 0 then exit;
  for d in aDoc.DIM.Values do
  begin
    writeLine('#DIM ' + d.Number + ' "' + d.Name + '"');

    for o in d.Objects.Values do
    begin
      writeLine('#OBJEKT ' + d.Number + ' ' + o.Number + ' "' + o.Name + '"');
    end;
  end;
end;

procedure TSieDocumentWriter.writeKonto(aDoc:TSieDocument);
var
  k:TSieAccount;
  s:string;
begin
  if aDoc.Konto.Count = 0 then exit;
  for k in aDoc.KONTO.Values do
  begin
     writeLine('#KONTO ' + k.Number + ' "' + k.Name + '"');
     if k.AccUnit <> '' then writeLine('#ENHET ' + k.Number + ' "' + k.AccUnit + '"');
     if k.AccType <> '' then writeLine('#KTYP ' + k.Number + ' "' + k.AccType + '"');
  end;

  for k in aDoc.KONTO.Values do
  begin
       for s in k.SRU do writeLine('#SRU ' + k.Number + ' ' + s);
  end;

end;

procedure TSieDocumentWriter.writePeriodValue(aDoc:TSieDocument; aTag:string; aValues:TListSiePeriodValue);
var
  objekt:string;
  v:TSiePeriodValue;
begin
  if aValues.Count = 0 then exit;
  objekt := '';

  for v in aValues do
  begin
     objekt := '';
     if not '#IB#UB#RES'.Contains(aTag) then objekt := getObjektLista(aDoc, v.Objects);

     writeLine(aTag + ' ' + IntToStr(v.YearNr) + ' ' + v.Account.Number + ' ' + objekt + ' ' + makeAmount(v.Amount));
  end;
end;


procedure TSieDocumentWriter.writePeriodSaldo(aDoc:TSieDocument; aTag:string; aValues:TListSiePeriodValue);
var
  objekt:string;
  v:TSiePeriodValue;
begin
  objekt := '';

  for v in aValues do
  begin
    objekt := getObjektLista(aDoc, v.Objects);
    writeLine(aTag + ' ' + IntToStr(v.YearNr) + ' ' + IntToStr(v.Period) + ' ' + v.Account.Number + ' ' + objekt + ' ' + makeAmount(v.Amount));
  end;
end;

function TSieDocumentWriter.getObjektLista(aDoc:TSieDocument; aValues:TListSieObject):string;
var
  ret:string;
  o:TSieObject;
begin
  if aDoc.SIETYP < 3 then exit('');

  if aValues = nil then exit('');

  ret := '{';
  for o in aValues do
  begin
    ret += o.Dimension.Number;
    ret += ' "' + o.Number + '" ';
  end;

  ret += '}';
  exit(ret);
end;

procedure TSieDocumentWriter.writeVER(aDoc:TSieDocument);
var
  v:TSieVoucher;
  vr:TSieVoucherRow;
  createdBy:string;
  createdDate:string;
  obj:string;
  quantity:string;
begin
  if aDoc.VER.Count = 0 then exit;
  createdBy := '';
  createdDate := '';

  for v in aDoc.VER do
  begin
    if v.CreatedBy <> '' then createdBy := '"' + v.CreatedBy + '"';
    if v.CreatedDate <> '' then createdDate := makeSieDate(v.CreatedDate);
    writeLine('#VER "' + v.Series + '" "' + v.Number + '" ' + makeSieDate(v.VoucherDate) + ' "' + v.Text + '" ' + createdDate + ' ' + createdBy);

    writeLine('{');

    for vr in v.Rows do
    begin
      obj := getObjektLista(aDoc, vr.Objects);
      if not vr.Quantity.HasValue then quantity := '' else quantity := makeAmount(vr.Quantity.Value) ;
      if vr.CreatedBy <> '' then createdBy := '"' + vr.CreatedBy + '"';
      writeLine(vr.Token +  ' ' + vr.Account.Number + ' ' + obj + ' ' + makeAmount(vr.Amount) + ' ' + makeSieDate(vr.RowDate) + ' "' + vr.Text + '" ' + quantity + ' ' + createdBy);
    end;

    writeLine('}');
  end;
end;

function TSieDocumentWriter.makeAmount(aAmount:double):string;
begin
  exit(CurrToStr(aAmount).Replace(',', '.'));
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

