unit USieDocumentReader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, USieClasses,USieDataItem;

type
  TSieDocumentReader = class
  private
    procedure initializeFields(aDoc:TSieDocument);
    procedure parseTrans(aDoc:TSieDocument;aDataItem: TSieDataItem; aCurVoucher: TSieVoucher);
  public
    function CreateSieDocument():TSieDocument;
    function ReadDocument(aFileName: string):TSieDocument;

    class function GetSieVersion(aFileName: string): integer; static;
  end;


implementation

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
begin
  ret := TSieDocument.Create();
  firstLine := true;
  fileName := afileName;
  initializeFields(ret);
  curVoucher := TSieVoucher.Create();
  AssignFile(F, aFileName);
  Reset(F);
  while not EOF(F) do
  begin
    ReadLn(F, line);
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
      '#ADDRESS': begin
        ret.FNAMN.Contact := di.GetString(0);
        ret.FNAMN.Street := di.GetString(1);
        ret.FNAMN.ZipCity := di.GetString(2);
        ret.FNAMN.Phone := di.GetString(3);
      end;
      '#BKOD': begin
        ret.FNAMN.SNI := di.GetInt(0);
      end;
      '#BTRANS': begin
        parseTrans(ret,di, curVoucher);
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

