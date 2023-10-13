unit USieDocumentReader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, USieClasses,USieDataItem, LConvEncoding;

type
  TSieDocumentReader = class
  private
    IgnoreBtrans:boolean;
    procedure initializeFields(aDoc:TSieDocument);
    procedure parseTRANS(aDoc:TSieDocument;aDataItem: TSieDataItem; aCurVoucher: TSieVoucher);
    procedure parseDimension(aDoc: TSieDocument; aDataItem: TSieDataItem);
    procedure parseEnhet(aDoc: TSieDocument; aDataItem: TSieDataItem);
    procedure parseIB(aDoc: TSieDocument; aDataItem: TSieDataItem);
    procedure parseUB(aDoc: TSieDocument; aDataItem: TSieDataItem);
    procedure parseKONTO(aDoc: TSieDocument; aDataItem: TSieDataItem);
    procedure parseKSUMMA(aDoc: TSieDocument; aDataItem: TSieDataItem);
    procedure parseKTYP(aDoc: TSieDocument; aDataItem: TSieDataItem);
    procedure parseOBJEKT(aDoc: TSieDocument; aDataItem: TSieDataItem);
    function parseOIB_OUB(aDoc: TSieDocument; aDataItem: TSieDataItem):TSiePeriodValue;
    function parsePBUDGET_PSALDO(aDoc: TSieDocument; aDataItem: TSieDataItem):TSiePeriodValue;
    procedure parseRAR(aDoc: TSieDocument; aDataItem: TSieDataItem);
    procedure parseSRU(aDoc: TSieDocument; aDataItem: TSieDataItem);
    procedure parseRES(aDoc: TSieDocument; aDataItem: TSieDataItem);
    function parseVER(aDoc: TSieDocument; aDataItem: TSieDataItem):TSieVoucher;
    procedure closeVoucher(aDoc: TSieDocument; aVoucher: TSieVoucher);
  public
    constructor Create(aIgnoreBtrans:boolean);
    function CreateSieDocument():TSieDocument;
    function ReadDocument(aFileName: string):TSieDocument;

    class function GetSieVersion(aFileName: string): integer; static;
  end;


implementation
constructor TSieDocumentReader.Create(aIgnoreBtrans:boolean);
begin
  IgnoreBtrans := aIgnoreBtrans;
end;

function TSieDocumentReader.CreateSieDocument():TSieDocument;
var
  ret: TSieDocument;
begin
  ret := TSieDocument.Create();
  exit(ret);
end;


procedure TSieDocumentReader.parseTrans(aDoc:TSieDocument; aDataItem: TSieDataItem; aCurVoucher: TSieVoucher);
begin
  if(not aDoc.KONTO.ContainsKey(aDataItem.GetString(0))) then
  begin
    aDoc.KONTO.AddOrSetValue(aDataItem.GetString(0), TSieAccount.Create(aDataItem.GetString(0)));
  end;
end;

procedure TSieDocumentReader.initializeFields(aDoc: TSieDocument);
begin
  aDoc.DateFormat := 'yyyyMMdd';
  aDoc.FNAMN := TSieCompany.Create();
  aDoc.KONTO := TDictStringSieAccount.Create();
  aDoc.DIM := TDictStringSieDimension.Create();
  aDoc.OIB := TListSiePeriodValue.Create();
  aDoc.OUB := TListSiePeriodValue.Create();
  aDoc.PSALDO := TListSiePeriodValue.Create();
  aDoc.PBUDGET := TListSiePeriodValue.Create();
  aDoc.PROGRAMS := TStringList.Create();
  aDoc.RAR := TDictStringSieBookingYear.Create();
  aDoc.IB := TListSiePeriodValue.Create();
  aDoc.UB := TListSiePeriodValue.Create();
  aDoc.RES := TListSiePeriodValue.Create();
  aDoc.VER := TListSieVoucher.Create();
  aDoc.ValidationErrors := TListSieError.Create();

  aDoc.DIM.AddOrSetValue('1', TSieDimension.Create('1', 'Resultatenhet', True));
  aDoc.DIM.AddOrSetValue('2', TSieDimension.Create('2', 'Kostnadsbärare',
    aDoc.DIM['1'], True));
  aDoc.DIM.AddOrSetValue('3', TSieDimension.Create('3', 'Reserverat', True));
  aDoc.DIM.AddOrSetValue('4', TSieDimension.Create('4', 'Reserverat', True));
  aDoc.DIM.AddOrSetValue('5', TSieDimension.Create('5', 'Reserverat', True));
  aDoc.DIM.AddOrSetValue('6', TSieDimension.Create('6', 'Projekt', True));
  aDoc.DIM.AddOrSetValue('7', TSieDimension.Create('7', 'Anställd', True));
  aDoc.DIM.AddOrSetValue('8', TSieDimension.Create('8', 'Kund', True));
  aDoc.DIM.AddOrSetValue('9', TSieDimension.Create('9', 'Leverantör', True));
  aDoc.DIM.AddOrSetValue('10', TSieDimension.Create('10', 'Faktura', True));
  aDoc.DIM.AddOrSetValue('11', TSieDimension.Create('11', 'Reserverat', True));
  aDoc.DIM.AddOrSetValue('12', TSieDimension.Create('12', 'Reserverat', True));
  aDoc.DIM.AddOrSetValue('13', TSieDimension.Create('13', 'Reserverat', True));
  aDoc.DIM.AddOrSetValue('14', TSieDimension.Create('14', 'Reserverat', True));
  aDoc.DIM.AddOrSetValue('15', TSieDimension.Create('15', 'Reserverat', True));
  aDoc.DIM.AddOrSetValue('16', TSieDimension.Create('16', 'Reserverat', True));
  aDoc.DIM.AddOrSetValue('17', TSieDimension.Create('17', 'Reserverat', True));
  aDoc.DIM.AddOrSetValue('18', TSieDimension.Create('18', 'Reserverat', True));
  aDoc.DIM.AddOrSetValue('19', TSieDimension.Create('19', 'Reserverat', True));

end;

function TSieDocumentReader.ReadDocument(aFileName: string):TsieDocument;
var
  firstLine:boolean;
  curVoucher: TSieVoucher;
  line:string;
  di: TSieDataItem;
  F: Text;
  pv: TSiePeriodValue;
  fileName:string;
  ret:TsieDocument;
  codePagedLine:string;
begin
  ret := TSieDocument.Create();
  firstLine := true;
  fileName := afileName;
  initializeFields(ret);
  curVoucher := nil;
  AssignFile(F, aFileName);
  Reset(F);
  while not EOF(F) do
  begin
    ReadLn(F, codePagedLine);
    line := CP437ToUTF8(codePagedLine);

    di := TSieDataItem.Create(line, ret);
    if firstLine then
    begin
      firstLine := false;
      if di.ItemType <> '#FLAGGA' then
      begin
           ret.ValidationErrors.Add(TSieError.Create('File should start with #FLAGGA'));
           exit(ret);
      end;
    end;

    //Add #KSUMMA to CRC

    case di.ItemType of
      '#ADRESS': begin
        ret.FNAMN.Contact := di.GetString(0);
        ret.FNAMN.Street := di.GetString(1);
        ret.FNAMN.ZipCity := di.GetString(2);
        ret.FNAMN.Phone := di.GetString(3);
      end;
      '#BKOD': begin
        ret.FNAMN.SNI := di.GetInt(0);
      end;
      '#BTRANS': begin
        if (not IgnoreBtrans) then parseTrans(ret,di, curVoucher);
      end;
      '#DIM': begin
        parseDimension(ret,di);
      end;
      '#ENHET': begin
        parseEnhet(ret,di);
      end;
      '#FLAGGA': begin
        ret.FLAGGA := di.GetInt(0);
      end;
      '#FNAMN': begin
        ret.FNAMN.Name := di.GetString(0);
      end;
      '#FNR': begin
        ret.FNAMN.OrgType := di.GetString(0);
      end;
      '#FORMAT': begin
        ret.FORMAT := di.GetString(0);
      end;
      '#FTYP': begin
        ret.FNAMN.OrgType := di.GetString(0);
      end;
      '#GEN': begin
        ret.GEN_DATE := di.GetDate(0);
        ret.GEN_NAMN := di.GetString(0);
      end;
      '#IB': begin
        parseIB(ret,di);
      end;
      '#KONTO': begin
        parseKONTO(ret,di);
      end;
      '#KSUMMA': begin
        //TODO: Handle CRC
        parseKSUMMA(ret,di);
      end;
      '#KPTYP': begin
        ret.KPTYP := di.GetString(0);
      end;
      '#KTYP': begin
        parseKTYP(ret,di);
      end;
      '#OJEKT': begin
        parseOBJEKT(ret, di)
      end;
      '#OIB': begin
        pv:= parseOIB_OUB(ret, di);
        //TODO: Handle streaming callback
        ret.OIB.Add(pv);
      end;
      '#OUB': begin
        pv:= parseOIB_OUB(ret, di);
        //TODO: Handle streaming callback
        ret.OUB.Add(pv);
      end;
      '#ORGNR': begin
        ret.FNAMN.OrgIdentifier := di.GetString(0);
      end;
      '#OMFATTNI': begin
        ret.OMFATTN := di.GetDate(0);
      end;
      '#PBUDGET': begin
        pv := parsePBUDGET_PSALDO(ret, di);
        if pv <> nil then
        begin
          //TODO: Handle streaming
          ret.PBUDGET.Add(pv);
        end;
      end;
      '#PROGRAM': begin
        ret.PROGRAMS := di.Data;
      end;
      '#PROSA': begin
        ret.PROSA := di.GetString(0);
      end;
      '#PSALDO': begin
        pv := parsePBUDGET_PSALDO(ret, di);
        if pv <> nil then
        begin
          //TODO: Handle streaming
          ret.PSALDO.Add(pv);
        end;
      end;
      '#RAR': begin
        parseRAR(ret, di);
      end;
      '#RES': begin
        parseRES(ret, di);
      end;
      '#RTRANS': begin
        if (not IgnoreBtrans) then parseTrans(ret,di, curVoucher);
      end;
      '#SIETYP': begin
        ret.SIETYP := di.GetInt(0);
      end;
      '#SRU': begin
        parseSRU(ret, di);
      end;
      '#TAXAR': begin
        ret.TAXAR := di.GetInt(0);
      end;
      '#UB': begin
        parseUB(ret, di);
      end;
      '#TRANS': begin
        parseTRANS(ret, di, curVoucher);
      end;
      '#VALUTA': begin
        ret.VALUTA := di.GetString(0);
      end;
      '#VER': begin
        curVoucher := parseVER(ret, di);
      end;
      '': begin
        //Empty line
      end;
      '{': begin
        //Empty line
      end;
      '}': begin
        if curVoucher <> nil then closeVoucher(ret, curVoucher);
        curVoucher := nil;
      end;

    else
      begin
        ret.ValidationErrors.Add(TSieError.Create('ItemType not  implemented:' + di.ItemType));
        break;
      end;
    end;
  end;
  exit(ret);
end;

procedure TSieDocumentReader.parseDimension(aDoc: TSieDocument; aDataItem: TSieDataItem);
var
  d:string;
  n:string;
  dim:TSieDimension;
begin
  d := aDataItem.GetString(0);
  n := aDataItem.GetString(1);
  if(aDoc.DIM.ContainsKey(d)) then
  begin
    dim := aDoc.DIM[d];
    dim.Name := n;
    dim.IsDefault:=false;
  end
  else
  begin
    dim := TSieDimension.Create(d, n, False);
  end;

  aDoc.DIM.AddOrSetValue(d, dim)
end;

procedure TSieDocumentReader.parseEnhet(aDoc: TSieDocument; aDataItem: TSieDataItem);
var
  konto: TSieAccount;
begin
  if aDoc.KONTO.ContainsKey(aDataItem.GetString(0)) then
  begin
    konto := aDoc.KONTO[aDataItem.GetString(0)];
  end
  else
  begin
    konto := TSieAccount.Create(aDataItem.GetString(0));
  end;

  konto.AccUnit := aDataItem.GetString(1);
  aDoc.KONTO.AddOrSetValue(aDataItem.GetString(0), konto);
end;
procedure TSieDocumentReader.parseIB(aDoc: TSieDocument; aDataItem: TSieDataItem);
var
  v:TSiePeriodValue;
begin
  aDoc.KONTO.TryAdd(aDataItem.GetString(1), TSieAccount.Create(aDataItem.GetString(1)));
  v := TSiePeriodValue.Create();
  v.YearNr := aDataItem.GetInt(0);
  v.Account := aDoc.KONTO[aDataItem.GetString(1)];
  v.Amount := aDataItem.GetDecimal(2);
  v.Quantity := aDataItem.GetDecimal(3);
  v.Token := aDataItem.ItemType;
  //TODO: Handle callbacks

  aDoc.IB.Add(v);
end;
procedure TSieDocumentReader.parseUB(aDoc: TSieDocument; aDataItem: TSieDataItem);
var
  v:TSiePeriodValue;
begin
  aDoc.KONTO.TryAdd(aDataItem.GetString(1), TSieAccount.Create(aDataItem.GetString(1)));
  v := TSiePeriodValue.Create();
  v.YearNr := aDataItem.GetInt(0);
  v.Account := aDoc.KONTO[aDataItem.GetString(1)];
  v.Amount := aDataItem.GetDecimal(2);
  v.Quantity := aDataItem.GetDecimal(3);
  v.Token := aDataItem.ItemType;
  //TODO: Handle callbacks

  aDoc.IB.Add(v);

end;
procedure TSieDocumentReader.parseKONTO(aDoc: TSieDocument; aDataItem: TSieDataItem);
var
  konto:TSieAccount;
begin
  aDoc.KONTO.TryAdd(aDataItem.GetString(0), TSieAccount.Create(aDataItem.GetString(0)));
  konto := aDoc.KONTO[aDataItem.GetString(0)];
  konto.Name:=aDataItem.GetString(1);
  aDoc.KONTO.AddOrSetValue(aDataItem.GetString(0), konto);
end;
procedure TSieDocumentReader.parseKSUMMA(aDoc: TSieDocument; aDataItem: TSieDataItem);
begin
  aDoc.KSUMMA := aDataItem.GetLong(0);
  //TODO: Handle checksum
end;
procedure TSieDocumentReader.parseKTYP(aDoc: TSieDocument; aDataItem: TSieDataItem);
var
  konto:TSieAccount;
begin
  aDoc.KONTO.TryAdd(aDataItem.GetString(0), TSieAccount.Create(aDataItem.GetString(0)));
  konto := aDoc.KONTO[aDataItem.GetString(0)];
  konto.AccType:=aDataItem.GetString(1);
  aDoc.KONTO.AddOrSetValue(aDataItem.GetString(0), konto);
end;
procedure TSieDocumentReader.parseOBJEKT(aDoc: TSieDocument; aDataItem: TSieDataItem);
var
  dimNumber:string;
  number:string;
  name:string;
  dim:TSieDimension;
  obj:TSieObject;
begin
  dimNumber := aDataItem.GetString(0);
  number := aDataItem.GetString(1);
  name := aDataItem.GetString(2);

  aDoc.DIM.TryAdd(dimNumber, TSieDimension.Create(dimNumber, '', false));
  dim :=  aDoc.DIM[dimNumber];
  obj := TSieObject.Create();
  obj.Dimension := dim;
  obj.Number := number;
  obj.Name := name;
  aDoc.OBJEKT.AddOrSetValue(number, obj);
end;
function TSieDocumentReader.parseOIB_OUB(aDoc: TSieDocument; aDataItem: TSieDataItem):TSiePeriodValue;
var
  offset:integer;
  v:TSiePeriodValue;
begin
  aDoc.KONTO.TryAdd(aDataItem.GetString(1), TSieAccount.Create(aDataItem.GetString(1)));
  if aDoc.SIETYP < 3 then
  begin
    aDoc.ValidationErrors.Add(TSieError.Create('Neither OIB or OUB is part of SIE < 3'));
  end;
  offset := 0;
  if aDataItem.RawData.Contains('{') then offset := 1;

  v := TSiePeriodValue.Create();
  v.YearNr := aDataItem.GetInt(0);
  v.Account := aDoc.KONTO[aDataItem.GetString(1)];
  v.Amount := aDataItem.GetDecimal(2 + offset);
  v.Quantity := aDataItem.GetDecimal(3) + offset;
  v.Objects := aDataItem.GetObjects();
  v.Token := aDataItem.ItemType;
  exit(v);
end;
function TSieDocumentReader.parsePBUDGET_PSALDO(aDoc: TSieDocument; aDataItem: TSieDataItem):TSiePeriodValue;
var
  offset:integer;
  v:TSiePeriodValue;
begin
  aDoc.KONTO.TryAdd(aDataItem.GetString(1), TSieAccount.Create(aDataItem.GetString(1)));
  if aDoc.SIETYP = 1 then
  begin
    aDoc.ValidationErrors.Add(TSieError.Create('Neither PSALDO or PBUDGET is part of SIE 1'));
  end;

  if (aDoc.SIETYP = 2) and (aDataItem.RawData.Contains('{')) and (not aDataItem.RawData.Contains('{}')) then
    begin
      //Applications reading SIE type 2 should ignore PSALDO containing non empty dimension.
      exit(nil);
  end;

  offset := 0;
  if aDataitem.RawData.Contains('{') then offset := 1;

  v := TSiePeriodValue.Create();
  v.YearNr := aDataItem.GetInt(0);
  v.Period := aDataItem.GetInt(1);
  v.Account := aDoc.KONTO[aDataItem.GetString(2)];
  v.Amount := aDataItem.GetDecimal(3 + offset);
  v.Quantity := aDataItem.GetDecimal(4) + offset;
  v.Token := aDataItem.ItemType;

  if (aDoc.SIETYP <> 2) and (aDataItem.RawData.Contains('{')) then
    begin
      v.Objects := aDataItem.GetObjects();
    end;
  exit(v);
end;
procedure TSieDocumentReader.parseRAR(aDoc: TSieDocument; aDataItem: TSieDataItem);
var
  rar:TSieBookingYear;
begin
  rar := TSieBookingYear.Create();
  rar.ID := aDataItem.GetInt(0);
  rar.StartDate := aDataItem.GetDate(1);
  rar.EndDate := aDataItem.GetDate(2);
  aDoc.RAR.AddOrSetValue(aDataItem.GetString(0), rar);
end;
procedure TSieDocumentReader.parseRES(aDoc: TSieDocument; aDataItem: TSieDataItem);
var
  offset:integer;
  v:TSiePeriodValue;
begin
  aDoc.KONTO.TryAdd(aDataItem.GetString(1), TSieAccount.Create(aDataItem.GetString(1)));

  offset := 0;
  if aDataItem.RawData.Contains('{') then offset := 1;

  v := TSiePeriodValue.Create();
  v.YearNr := aDataItem.GetInt(0);
  v.Account := aDoc.KONTO[aDataItem.GetString(1)];
  v.Amount := aDataItem.GetDecimal(2 + offset);
  v.Quantity := aDataItem.GetDecimal(3) + offset;
  v.Objects := aDataItem.GetObjects();
  v.Token := aDataItem.ItemType;
  aDoc.RES.Add(v);
  //TODO:Callback

end;

procedure TSieDocumentReader.parseSRU(aDoc: TSieDocument; aDataItem: TSieDataItem);
var
  konto:TSieAccount;
begin
  aDoc.KONTO.TryAdd(aDataItem.GetString(0), TSieAccount.Create(aDataItem.GetString(0)));
  konto := aDoc.KONTO[aDataItem.GetString(0)];
  konto.SRU.Add(aDataItem.GetString(1));

end;

function TSieDocumentReader.parseVER(aDoc: TSieDocument; aDataItem: TSieDataItem):TSieVoucher;
var
  v:TSieVoucher;
begin
  if aDataItem.GetDate(2) = '' then aDoc.ValidationErrors.Add(TSieError.Create('MissingFieldException VoucherDate'));

  v:= TSieVoucher.Create();
  v.Series := aDataItem.GetString(0);
  v.Number := aDataItem.GetString(1);
  v.VoucherDate := aDataItem.GetDate(2);
  v.Text := aDataItem.GetString(3);
  v.CreatedDate := aDataItem.GetDate(4);
  v.CreatedBy := aDataItem.GetString(5);
  v.Token := aDataItem.ItemType;

  exit(v);
end;
procedure TSieDocumentReader.closeVoucher(aDoc: TSieDocument; aVoucher: TSieVoucher);
begin
  here i am
end;

class function TSieDocumentReader.GetSieVersion(aFileName: string): integer; static;
var
  F: Text;
  line: string;
  di: TSieDataItem;
  ret: integer;
begin
  ret := -1;
  AssignFile(F, aFileName);
  Reset(F);
  while not EOF(F) do
  begin
    ReadLn(F, line);
    if line.StartsWith('#SIETYP') then
    begin
      di := TSieDataItem.Create(line);
      ret := di.GetInt(0);
      break;
    end;
  end;
  exit(ret);
end;
end.

